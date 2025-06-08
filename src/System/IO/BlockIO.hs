{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

module System.IO.BlockIO (

    -- * I\/O context and initialisation
    IOCtx,
    IOCtxParams(..),
    defaultIOCtxParams,
    withIOCtx,
    initIOCtx,
    closeIOCtx,

    -- * Performing I\/O
    submitIO,
    IOOp(IOOpRead, IOOpWrite),
    IOResult(IOResult, IOError),
    ByteCount, Errno(..),

  ) where

import Data.Bits
import Data.Primitive.ByteArray
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Control.Monad
import Control.Monad.Primitive
import Control.Concurrent (forkOn, myThreadId, threadCapability,
                           getNumCapabilities)
import Control.Concurrent.MVar
import Control.Concurrent.QSemN
import Control.Concurrent.Chan
import Control.Exception (mask_, throw, ArrayException(UndefinedElement),
                          finally, assert, throwIO, bracket, onException)
import System.IO.Error
import GHC.IO.Exception (IOErrorType(ResourceVanished, InvalidArgument))
import GHC.Conc.Sync (labelThread)

import Foreign.Ptr (plusPtr)
import Foreign.C.Error (Errno(..))
import System.Posix.Types (Fd (..), FileOffset, ByteCount)
import System.Posix.Internals (hostIsThreaded)

import qualified System.IO.BlockIO.URing as URing
import           System.IO.BlockIO.URing (IOResult(..))

-- | IO context: a handle used by threads submitting IO batches.
--
-- Internally, each GHC capability in the program creates its own independent IO
-- context. This means that each capability can process batches of I/O
-- operations independently. As such, running with more capabilities can
-- increase throughput.
newtype IOCtx = IOCtx (V.Vector IOCapCtx) -- one per RTS capability.

type CapNo = Int
data IOCapCtx = IOCapCtx {
               -- | This is initialised from the 'ioctxBatchSizeLimit' from the 'IOCtxParams'.
               ioctxBatchSizeLimit' :: !Int,

               -- | IO concurrency control: used by writers to reserve the
               -- right to submit an IO batch of a given size, and by the
               -- completion thread to return it on batch completion.
               ioctxQSemN :: !QSemN,

               -- | Locking of the writer end of the URing: used by writers
               -- while they are modifying the uring submission queue.
               ioctxURing :: !(MVar (Maybe URing.URing)),

               -- | Communication channel from writers to the completion thread:
               -- letting it know about new batches of IO that they have
               -- submitted.
               ioctxChanIOBatch :: !(Chan IOBatch),

               -- | Communication channel from the completion thread to writers:
               -- letting them grab the next batch index, which they need when
               -- submitting IO operations.
               ioctxChanIOBatchIx :: !(Chan IOBatchIx),

               -- | An MVar to synchronise on for shutdown
               ioctxCloseSync :: !(MVar ())
             }

-- | Parameters for instantiating an 'IOCtx' IO context.
--
-- Picking suitable parameters for high performance depends on the hardware that
-- is used and the workload of the program. There are benchmarks included in the
-- [@blockio-uring@](https://github.com/well-typed/blockio-uring) repository
-- that can help gauge the performance for a random 4k read workload, but some
-- trial and error with custom benchmarks might be required.
--
-- Ideally the concurrency limit should be a multiple of the batch size limit.
-- This should ensure that there can be multiple batches in flight at once. If
-- so, the SSD can more often than not pick up new I/O batches to perform as
-- soon as it has finished other batches.
--
-- Ideally the batch size limit should be equal to if not larger than the number
-- of I/O operations that an SSD can perform concurrently. This ensures that the
-- SSD can perform most if not all I/O operations in a batch concurrently.
--
-- Internally, each GHC capability in the program creates its own independent IO
-- context. These parameters apply separately to each capability's IO context.
-- For example, if the program runs with 2 capabilities, a batch size limit of
-- 64, and a concurrency limit of 256, then each capability gets its own IO
-- context, and each IO context can process batches of at most 64 I/O operations
-- at a time, and each IO context can only process 4 such batches concurrently.
data IOCtxParams = IOCtxParams {
    -- | The maximum size of a batch of I\/O operations that can be processed as
    -- a whole.
    --
    -- Note that his does /not/ affect the size of batches of I/O operations
    -- that can be submitted using 'submitIO'. There is no restriction on the
    -- size of batches submitted using 'submitIO'. If the size of a batch that
    -- is passed to 'submitIO' exceeds this limit, then internally these batches
    -- will split into sub-batches of size at most the limit, and each sub-batch
    -- is processed individually.
    ioctxBatchSizeLimit   :: !Int,
    -- | The total number of I/O operations that can be processed concurrently.
    --
    -- If a use of 'submitIO' would lead to this limit being exceeded, then the
    -- call to 'submitIO' will block until enough I/O batches have been
    -- processed.
    ioctxConcurrencyLimit :: !Int
  }
  deriving stock Show

-- | Default parameters. Some manual tuning of parameters might be required to
-- achieve higher performance targets (see 'IOCtxParams' for hints).
defaultIOCtxParams :: IOCtxParams
defaultIOCtxParams =
  IOCtxParams {
    ioctxBatchSizeLimit   = 64,
    ioctxConcurrencyLimit = 64 * 3
  }

validateIOCtxParams :: IOCtxParams -> Maybe String
validateIOCtxParams IOCtxParams{..} = ("IOCtxParams are invalid because " ++) <$>
    if
      | ioctxBatchSizeLimit <= 0
      -> Just "the batch size limit is non-positive"
      | ioctxBatchSizeLimit >= 2^(15 :: Int)
      -> Just "the batch size limit is greater than or equal to 2^15"
      | ioctxConcurrencyLimit <= 0
      -> Just "the concurrency limit is non-positive"
      | ioctxConcurrencyLimit >= 2^(16 :: Int)
      -> Just "the concurrency limit is greater than or equal to 2^16"
      | ioctxBatchSizeLimit > ioctxConcurrencyLimit
      -> Just "the batch size limit exceeds the concurrency limit"
      | otherwise
      -> Nothing

withIOCtx :: IOCtxParams -> (IOCtx -> IO a) -> IO a
withIOCtx params = bracket (initIOCtx params) closeIOCtx

initIOCtx :: IOCtxParams -> IO IOCtx
initIOCtx ioctxparams = do
    unless hostIsThreaded $ throwIO rtsNotThreaded
    forM_ (validateIOCtxParams ioctxparams) $ throwIO . mkInvalidArgumentError
    ncaps <- getNumCapabilities
    IOCtx <$> V.generateM ncaps (initIOCapCtx ioctxparams)
  where
    rtsNotThreaded =
        mkIOError
          illegalOperationErrorType
          "The run-time system should be threaded, make sure you are passing the -threaded flag"
          Nothing
          Nothing

    mkInvalidArgumentError :: String -> IOError
    mkInvalidArgumentError msg =
        mkIOError
          InvalidArgument
          msg
          Nothing
          Nothing

initIOCapCtx :: IOCtxParams -> CapNo -> IO IOCapCtx
initIOCapCtx IOCtxParams {
               ioctxBatchSizeLimit,
               ioctxConcurrencyLimit
             } capno = do
    mask_ $ do
      ioctxQSemN         <- newQSemN ioctxConcurrencyLimit
      uring              <- URing.setupURing (URing.URingParams ioctxBatchSizeLimit ioctxConcurrencyLimit)
      ioctxURing         <- newMVar (Just uring)
      ioctxChanIOBatch   <- newChan
      ioctxChanIOBatchIx <- newChan
      ioctxCloseSync     <- newEmptyMVar
      t <- forkOn capno $
             -- Use forkOn to bind the thread to this capability
             completionThread
               uring
               ioctxCloseSync
               ioctxConcurrencyLimit
               ioctxQSemN
               ioctxChanIOBatch
               ioctxChanIOBatchIx
      labelThread t ("System.IO.BlockIO.completionThread " ++
                     "(for cap " ++ show capno ++ ")")
      let initialBatchIxs :: [IOBatchIx]
          initialBatchIxs = [0 .. ioctxConcurrencyLimit-1]
      writeList2Chan ioctxChanIOBatchIx initialBatchIxs
      return IOCapCtx {
        ioctxBatchSizeLimit' = ioctxBatchSizeLimit,
        ioctxQSemN,
        ioctxURing,
        ioctxChanIOBatch,
        ioctxChanIOBatchIx,
        ioctxCloseSync
      }

closeIOCtx :: IOCtx -> IO ()
closeIOCtx (IOCtx capctxs) = closeIOCapCtxs capctxs
  where
    -- If an exception was raised while closing one context, we still try to
    -- close the remaining IOCapCtxs
    closeIOCapCtxs xs
      | Just (x, ys) <- V.uncons xs
      = closeIOCapCtx x `finally` closeIOCapCtxs ys
      | otherwise
      = pure ()

closeIOCapCtx :: IOCapCtx -> IO ()
closeIOCapCtx IOCapCtx {ioctxURing, ioctxCloseSync} = do
    uringMay <- takeMVar ioctxURing
    case uringMay of
      Nothing -> putMVar ioctxURing Nothing
      Just uring -> do
        --TODO: there's a problem with the keepAlives here. By sending the
        -- Nop with maxBound we're telling the completionThread to shut down,
        -- but this may be the only thing keeping the IO buffers from being
        -- GCd. We could get heap corruption if we get a GC during shutdown.
        -- We need to prevent new operations being submitted, and wait for all
        -- existing operations to complete.
        URing.prepareNop uring (URing.IOOpId maxBound)
        URing.submitIO uring
        takeMVar ioctxCloseSync
        URing.closeURing uring
        putMVar ioctxURing Nothing

-- | The 'MutableByteArray' buffers within __must__ be pinned. Addresses into
-- these buffers are passed to @io_uring@, and the buffers must therefore not be
-- moved around. 'submitIO' will check that buffers are pinned, and will throw
-- errors if it finds any that are not pinned.
data IOOp s = IOOpRead  !Fd !FileOffset !(MutableByteArray s) !Int !ByteCount
            | IOOpWrite !Fd !FileOffset !(MutableByteArray s) !Int !ByteCount

-- | Submit a batch of I\/O operations, and wait for them all to complete.
-- The sequence of results matches up with the sequence of operations.
-- Any I\/O errors are reported in the result list, not as IO exceptions.
--
-- Note that every operation in the batch is performed concurrently with
-- respect to each other (and any other concurrent batches): their effects
-- may be performed in any order. It is up to you to ensure the effects do
-- not interfere with each other (i.e. not mixing reads and writes to
-- overlapping areas of files).
--
-- It is permitted to submit multiple batches concurrently from different
-- Haskell threads. Submitting I\/O only blocks the calling Haskell thread, it
-- does not block other Haskell threads. The maximum concurrency is set when
-- the 'IOCtx' is created: submitting more operations than this will block until
-- enough previous operations have completed.
--
-- Performance tips:
--
-- * Use reasonable batch sizes to amortise the overheads over multiple
--   operations. Batch sizes that are within the I\/O limit of the
--   'IOCtx' can be initiated with a single system call.
--
-- * Consider that most SSDs can perform up to 64 operations concurrently. So
--   use reasonable batch sizes, and submit multiple batches concurrently.
--
-- * Think of I\/O as a queue, with I\/O operations being added at one end,
--   and results arriving at the other: keep the queue full with 64 operations
--   in progress at once.
--
-- * Pipeline your I\/O submissions to keep the queue full: submit enough
--   batches to keep the queue full, and as batches complete, submit more.
--   For example follow a strategy of submitting batches up to double the
--   target SSD queue depth (i.e. 2x 64 = 128) and when it drains to nearly
--   the target depth, fill it up to double again. This way there is always
--   at least the target number in flight at once.
--
submitIO :: IOCtx -> V.Vector (IOOp RealWorld) -> IO (VU.Vector IOResult)
submitIO (IOCtx capctxs) !ioops = do
    -- Find out which capability the thread is currently running on and use
    -- that one. It does _not matter_ for correctness if the thread is migrated
    -- while the I/O is submitted or when waiting for completion. Migration
    -- happens sufficiently infrequently that it should not be a performance
    -- problem.
    tid <- myThreadId
    (capno, _) <- threadCapability tid
    let !capctx = capctxs V.! (capno `mod` V.length capctxs)
    submitCapIO capctx ioops

submitCapIO :: IOCapCtx -> V.Vector (IOOp RealWorld) -> IO (VU.Vector IOResult)
submitCapIO ioctx@IOCapCtx {ioctxBatchSizeLimit'} !ioops
    -- Typical small case. We can be more direct.
  | V.length ioops > 0 && V.length ioops <= ioctxBatchSizeLimit'
  = mask_ $ do
      iobatchCompletion <- newEmptyMVar
      prepAndSubmitIOBatch ioctx ioops iobatchCompletion
      takeMVar iobatchCompletion

submitCapIO ioctx@IOCapCtx {ioctxBatchSizeLimit'} !ioops0 =
    -- General case. Needs multiple batches and combining results, or the vector
    -- of I/O operations is empty.
    --
    --TODO: instead of the completion thread allocating result arrays
    -- allocate them in the calling thread and have the completionThread
    -- fill them in. Then for batches we can send in a bunch of slices of
    -- a contiguous array, and then we can avoid having to re-combine them
    -- at the end here.
    mask_ $ do
      iobatchCompletions <- prepAndSubmitIOBatches [] ioops0
      awaitIOBatches iobatchCompletions
  where
    prepAndSubmitIOBatches acc !ioops
      | V.null ioops = return acc
      | otherwise = do
          let batch = V.take ioctxBatchSizeLimit' ioops
          iobatchCompletion <- newEmptyMVar
          prepAndSubmitIOBatch ioctx batch iobatchCompletion
          prepAndSubmitIOBatches (iobatchCompletion:acc)
                                 (V.drop ioctxBatchSizeLimit' ioops)

    awaitIOBatches iobatchCompletions =
      VU.concat <$> mapM takeMVar (reverse iobatchCompletions)

-- Must be called with async exceptions masked. See mask_ above in submitIO.
prepAndSubmitIOBatch :: IOCapCtx
                     -> V.Vector (IOOp RealWorld)
                     -> MVar (VU.Vector IOResult)
                     -> IO ()
prepAndSubmitIOBatch IOCapCtx {
                       ioctxQSemN,
                       ioctxURing,
                       ioctxChanIOBatch,
                       ioctxChanIOBatchIx
                     }
                     !iobatch !iobatchCompletion = do
    let !iobatchOpCount = V.length iobatch
    -- We're called with async exceptions masked, but 'waitQSemN' can block and
    -- receive exceptions. That's ok. But once we acquire the semaphore
    -- quantitiy we must eventully return it. There's two cases for returning:
    -- 1. we successfully submit the I/O and pass the information off to the
    --    completionThread which will signal the semaphore upon completion, or
    -- 2. we encounter an exception here in which case we need to undo the
    --    semaphore acquisition.
    -- For the latter case we use 'onException'. We also need to obtain a
    -- batch index. This should never block because we have as many tokens as
    -- QSemN initial quantitiy, and the batch ix is released before the QSemN
    -- is signaled in the completionThread.
    waitQSemN ioctxQSemN iobatchOpCount
    !iobatchIx <- readChan ioctxChanIOBatchIx
    -- Thus undoing the acquisition involves releasing the batch index and
    -- semaphore quantitiy (which themselves cannot blocks).
    let undoAcquisition = do writeChan ioctxChanIOBatchIx iobatchIx
                             signalQSemN ioctxQSemN iobatchOpCount
    flip onException undoAcquisition $ do
      -- We can receive an async exception if takeMVar blocks. That's ok, we'll
      -- undo the acquisition.
      muring <- takeMVar ioctxURing
      -- From here on we cannot receive any async exceptions, because we do not
      -- do any more blocking operations. But we can encounter sync exceptions,
      -- so we may still need to release the mvar on exception.
      flip onException (putMVar ioctxURing muring) $ do
        uring <- maybe (throwIO closed) pure muring
        V.iforM_ iobatch $ \ioopix ioop -> case ioop of
          IOOpRead  fd off buf bufOff cnt -> do
            guardPinned buf
            URing.prepareRead  uring fd off
                              (mutableByteArrayContents buf `plusPtr` bufOff)
                              cnt (packIOOpId iobatchIx ioopix)
          IOOpWrite fd off buf bufOff cnt -> do
            guardPinned buf
            URing.prepareWrite uring fd off
                              (mutableByteArrayContents buf `plusPtr` bufOff)
                              cnt (packIOOpId iobatchIx ioopix)
        -- TODO: if submitIO or guardPinned throws an exception, we need to
        -- undo / clear the SQEs that we prepared.
        URing.submitIO uring

        -- More async exception safety: we want to inform the completionThread
        -- /if and only if/ we successfully submitted a batch of IO. So now that
        -- we have submitted a batch we need to inform the completionThread
        -- without interruptions. We're still masked, but writeChan does not
        -- throw exceptions and never blocks (unbounded channel) so we should
        -- not get async or sync exceptions.
        writeChan ioctxChanIOBatch
                  IOBatch {
                    iobatchIx,
                    iobatchOpCount,
                    iobatchCompletion,
                    iobatchKeepAlives = iobatch
                  }
        putMVar ioctxURing muring
  where
    guardPinned mba = unless (isMutableByteArrayPinned mba) $ throwIO notPinned
    closed    = mkIOError ResourceVanished "IOCtx closed" Nothing Nothing
    notPinned = mkIOError InvalidArgument "MutableByteArray is unpinned" Nothing Nothing

data IOBatch = IOBatch {
                 iobatchIx         :: !IOBatchIx,
                 iobatchOpCount    :: !Int,
                 iobatchCompletion :: !(MVar (VU.Vector IOResult)),
                 -- | The list of I\/O operations is sent to the completion
                 -- thread so that the buffers are kept alive while the kernel
                 -- is using them.
                 iobatchKeepAlives :: V.Vector (IOOp RealWorld)
               }

-- | We submit and processes the completions in batches. This is the index into
-- the tracking arrays of the batch.
type IOBatchIx = Int

-- | This is the index of an operation within a batch. The pair of the
-- 'IOBatchIx' and 'IOOpIx' is needed to identity a specific operation within
-- the tracking data structures.
type IOOpIx    = Int

{-# INLINE packIOOpId #-}
-- | The pair of the 'IOBatchIx' and 'IOOpIx' is needed to identify a specific
-- operation within the tracking data structures. We pair up the batch index
-- and the intra-batch index into the operation identifier. This identifier is
-- submitted with the operation and returned with the completion. Thus upon
-- completion this allows us to match up the operation with the tracking data
-- structures and process the operation completion.
packIOOpId :: IOBatchIx -> IOOpIx -> URing.IOOpId
packIOOpId batchix opix =
    URing.IOOpId $ unsafeShiftL (fromIntegral batchix) 32
                .|. fromIntegral opix

{-# INLINE unpackIOOpId #-}
unpackIOOpId :: URing.IOOpId -> (IOBatchIx, IOOpIx)
unpackIOOpId (URing.IOOpId w64) =
    (batchix, opix)
  where
    batchix :: Int
    batchix = fromIntegral (unsafeShiftR w64 32)

    opix :: Int
    opix    = fromIntegral (w64 .&. 0xffffffff)

-- Note: all these arrays are indexed by 'IOBatchIx'.
--
-- The 'counts' array keeps track (per batch) of the number of operations that
-- remain to complete. When we processes the last operation in a batch we can
-- complete the whole batch. Batch indexes that are not currently in use contain
-- a count value of -1. This is used to identify when an operation completes
-- that is for a previously unused batch index, and thus tells us we have a new
-- batch and we need to find and set up the tracking information appropriately.
--
-- The 'results' array keeps track (per-batch) of the results of individual I/O
-- operations. Each element is an array indexed by 'IOOpIx', containing the
-- 'IOResult' for that operation. This result array is accumulated and then
-- frozen and returned as the result for the batch. Batch indexes that are not
-- currently in use contain an invalid entry.
--
-- The 'completions' array keeps track (per batch) of the completion MVar used
-- to communicate the batch result back to the thread that submitted the batch.
--
-- The 'keepAlives' array ensures (per batch) that certain heap objects are kept
-- live for the duration of the I/O operations in the batch. Specifically, it is
-- the I/O buffers for each operation that we must keep live (otherwise if they
-- were GC'd the kernel could scribble on top of whatever got placed there
-- next). We reuse the original vector of IOOps that was submitted since this
-- conveniently exists anyway and it contains the IOOps which themselves contain
-- the I/O buffers. The 'keepAlives' entries are overwritten with 'invalidEntry'
-- once they are no longer needed.
--
-- Algorithm outline:
-- + wait for single IO result
-- + if the count is -1, grab new batches from the chan (and process them)
--   repeatedly until the batch count in question is found.
-- + if the count is positive, decrement and update result array
-- + if the count is now 0, also fill in the completion
-- + reset count to -1, and result entries to invalid
completionThread :: URing.URing
                 -> MVar ()
                 -> Int
                 -> QSemN
                 -> Chan IOBatch
                 -> Chan IOBatchIx
                 -> IO ()
completionThread !uring !done !maxc !qsem !chaniobatch !chaniobatchix = do
    counts      <- VUM.replicate maxc (-1)
    results     <- VM.replicate maxc invalidEntry
    completions <- VM.replicate maxc invalidEntry
    keepAlives  <- VM.replicate maxc invalidEntry
    collectCompletion counts results completions keepAlives
      `finally` putMVar done ()
  where
    collectCompletion :: VUM.MVector RealWorld Int
                      -> VM.MVector  RealWorld (VUM.MVector RealWorld IOResult)
                      -> VM.MVector  RealWorld (MVar (VU.Vector IOResult))
                      -> VM.MVector  RealWorld (V.Vector (IOOp RealWorld))
                      -> IO ()
    collectCompletion !counts !results !completions !keepAlives = do
      iocompletion <- URing.awaitIO uring
      let (URing.IOCompletion !ioopid !iores) = iocompletion
      unless (ioopid == URing.IOOpId maxBound) $ do
        let (!iobatchix, !ioopix) = unpackIOOpId ioopid
        count <- do
          c <- VUM.read counts iobatchix
          if c < 0 then collectIOBatches iobatchix
                   else return c
        assert (count > 0) (return ())
        VUM.write counts iobatchix (count-1)
        result <- VM.read results iobatchix
        VUM.write result ioopix iores
        when (count == 1) $ do
          completion <- VM.read completions iobatchix
          VUM.write counts     iobatchix (-1)
          VM.write results     iobatchix invalidEntry
          VM.write completions iobatchix invalidEntry
          VM.write keepAlives  iobatchix invalidEntry
          result' <- VU.unsafeFreeze result
          putMVar completion (result' :: VU.Vector IOResult)
          -- Important: release batch index _before_ we signal the QSemN.
          -- The other side needs the guarantee that the index is available
          -- once it acquires the QSemN.
          writeChan chaniobatchix iobatchix
          let !qrelease = VU.length result'
          signalQSemN qsem qrelease
        collectCompletion counts results completions keepAlives

      where
        collectIOBatches :: IOBatchIx -> IO Int
        collectIOBatches !iobatchixNeeded = do
          IOBatch{
              iobatchIx,
              iobatchOpCount,
              iobatchCompletion,
              iobatchKeepAlives
            } <- readChan chaniobatch
          oldcount <- VUM.read counts iobatchIx
          assert (oldcount == (-1)) (return ())
          VUM.write counts iobatchIx iobatchOpCount
          result <- VUM.replicate iobatchOpCount (IOResult (-1))
          VM.write results iobatchIx result
          VM.write completions iobatchIx iobatchCompletion
          VM.write keepAlives iobatchIx iobatchKeepAlives
          if iobatchIx == iobatchixNeeded
            then return $! iobatchOpCount
            else collectIOBatches iobatchixNeeded

    {-# NOINLINE invalidEntry #-}
    invalidEntry :: a
    invalidEntry =
      throw (UndefinedElement "System.IO.BlockIO.completionThread")
