{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module System.IO.BlockIO (

    -- * I\/O context and initialisation
    IOCtx,
    initIOCtx,
    IOCtxParams(..),
    defaultIOCtxParams,
    closeIOCtx,

    -- * Performing I\/O
    submitIO,
    IOOp(..),
    IOResult(IOResult, IOError),
    ByteCount, Errno(..),

  ) where

import Data.Bits
import Data.Coerce
import Data.Primitive.ByteArray
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Control.Monad
import Control.Monad.Primitive
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.QSemN
import Control.Concurrent.Chan
import Control.Exception (mask_, throw, ArrayException(UndefinedElement),
                          finally, assert, throwIO)
import System.IO.Error
import GHC.IO.Exception (IOErrorType(ResourceVanished, InvalidArgument))

import Foreign.Ptr (plusPtr)
import Foreign.C.Error (Errno(..))
import System.Posix.Types (Fd, FileOffset, ByteCount)
import System.Posix.Internals (hostIsThreaded)

import qualified System.IO.BlockIO.URing as URing
import           System.IO.BlockIO.URing (IOResult(..))


-- | IO context: a handle used by threads submitting IO batches.
--
data IOCtx = IOCtx {
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

data IOCtxParams = IOCtxParams {
                     ioctxBatchSizeLimit   :: !Int,
                     ioctxConcurrencyLimit :: !Int
                   }

defaultIOCtxParams :: IOCtxParams
defaultIOCtxParams =
  IOCtxParams {
    ioctxBatchSizeLimit   = 64,
    ioctxConcurrencyLimit = 64 * 3
  }

initIOCtx :: IOCtxParams -> IO IOCtx
initIOCtx IOCtxParams {ioctxBatchSizeLimit, ioctxConcurrencyLimit} = do
    unless hostIsThreaded $ throwIO rtrsNotThreaded
    mask_ $ do
      ioctxQSemN         <- newQSemN ioctxConcurrencyLimit
      uring              <- URing.setupURing (URing.URingParams ioctxBatchSizeLimit)
      ioctxURing         <- newMVar (Just uring)
      ioctxChanIOBatch   <- newChan
      ioctxChanIOBatchIx <- newChan
      ioctxCloseSync     <- newEmptyMVar
      _ <- forkIO $
             completionThread
               uring
               ioctxCloseSync
               ioctxConcurrencyLimit
               ioctxQSemN
               ioctxChanIOBatch
               ioctxChanIOBatchIx
      let initialBatchIxs :: [IOBatchIx]
          initialBatchIxs = [0 .. ioctxConcurrencyLimit-1]
      writeList2Chan ioctxChanIOBatchIx initialBatchIxs
      return IOCtx {
        ioctxQSemN,
        ioctxURing,
        ioctxChanIOBatch,
        ioctxChanIOBatchIx,
        ioctxCloseSync
      }
  where
    rtrsNotThreaded =
        mkIOError
          illegalOperationErrorType
          "The run-time system should be threaded, make sure you are passing the -threaded flag"
          Nothing
          Nothing

closeIOCtx :: IOCtx -> IO ()
closeIOCtx IOCtx {ioctxURing, ioctxCloseSync} = do
    uringMay <- takeMVar ioctxURing
    case uringMay of
      Nothing -> putMVar ioctxURing Nothing
      Just uring -> do
        URing.prepareNop uring (URing.IOOpId maxBound)
        URing.submitIO uring
        takeMVar ioctxCloseSync
        URing.closeURing uring
        putMVar ioctxURing Nothing

-- | The 'MutableByteArray' buffers within __must__ be pinned. Addresses into
-- these buffers are passed to @io_uring@, and the buffers must therefore not be
-- moved around.
data IOOp m = IOOpRead  !Fd !FileOffset !(MutableByteArray (PrimState m)) !Int !ByteCount
            | IOOpWrite !Fd !FileOffset !(MutableByteArray (PrimState m)) !Int !ByteCount


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
submitIO :: IOCtx -> V.Vector (IOOp IO) -> IO (VU.Vector IOResult)
submitIO IOCtx {
           ioctxQSemN,
           ioctxURing,
           ioctxChanIOBatch,
           ioctxChanIOBatchIx
         }
         ioops = do
    let !iobatchOpCount = V.length ioops
    waitQSemN ioctxQSemN iobatchOpCount
    iobatchIx         <- readChan ioctxChanIOBatchIx
    iobatchCompletion <- newEmptyMVar
    let iobatchKeepAlives = ioops
    writeChan ioctxChanIOBatch
              IOBatch {
                iobatchIx,
                iobatchOpCount,
                iobatchCompletion,
                iobatchKeepAlives
              }
    withMVar ioctxURing $ \case
      Nothing -> throwIO closed
      Just uring -> do
--      print ("submitIO", iobatchOpCount)
        V.iforM_ ioops $ \ioopix ioop ->
            let !ioopid = packIOOpId iobatchIx ioopix
            in
              --print ioop >>
              case ioop of
              IOOpRead  fd off buf bufOff cnt -> do
                guardPinned buf
                URing.prepareRead  uring fd off
                                  (mutableByteArrayContents buf `plusPtr` bufOff)
                                  cnt ioopid
              IOOpWrite fd off buf bufOff cnt -> do
                guardPinned buf
                URing.prepareWrite uring fd off
                                  (mutableByteArrayContents buf `plusPtr` bufOff)
                                  cnt ioopid
        URing.submitIO uring
--      print ("submitIO", "submitting done")
    coerce <$> takeMVar iobatchCompletion
  where
    closed = mkIOError ResourceVanished "IOCtx closed" Nothing Nothing
    guardPinned mba = do
      unless (isMutableByteArrayPinned mba) $ throwIO notPinned
    notPinned = mkIOError InvalidArgument "MutableByteArray is unpinned" Nothing Nothing

data IOBatch = IOBatch {
                 iobatchIx         :: !IOBatchIx,
                 iobatchOpCount    :: !Int,
                 iobatchCompletion :: MVar (VU.Vector IOResult),
                 -- | The list of I\/O operations is sent to the completion
                 -- thread so that the buffers are kept alive while the kernel
                 -- is using them.
                 iobatchKeepAlives :: V.Vector (IOOp IO)
               }

type IOBatchIx = Int
type IOOpIx    = Int

{-# INLINE packIOOpId #-}
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
                      -> VM.MVector  RealWorld (V.Vector (IOOp IO))
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
          writeChan chaniobatchix iobatchix
          let !qrelease = VU.length result'
          signalQSemN qsem qrelease
        collectCompletion counts results completions keepAlives

      -- wait for single IO result
      -- if the count is positive, decrement and update result array
      -- if the count is now 0, also fill in the completion
      -- if the count was 0 before, grab next batch from the chan and process it
      -- may have to do that repeatedly until the batch count in question is positive
      -- reset count to -1, and result entries to undefined
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
          VUM.write counts iobatchIx (fromIntegral iobatchOpCount)
          result <- VUM.replicate iobatchOpCount (IOResult (-1))
          VM.write results iobatchIx result
          VM.write completions iobatchIx iobatchCompletion
          VM.write keepAlives iobatchIx iobatchKeepAlives
          if iobatchIx == iobatchixNeeded
            then return $! fromIntegral iobatchOpCount
            else collectIOBatches iobatchixNeeded

    {-# NOINLINE invalidEntry #-}
    invalidEntry :: a
    invalidEntry =
      throw (UndefinedElement "System.IO.BlockIO.completionThread")
