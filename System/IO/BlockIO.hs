{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

import Data.Word
import Data.Int
import Data.Bits
import Data.Ix
import Data.Array.IArray
import Data.Array.IO
import Data.Array.Unboxed
import Data.Coerce

import Control.Monad
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.QSemN
import Control.Concurrent.Chan
import Control.Exception (mask_, throw, ArrayException(UndefinedElement),
                          finally, assert, throwIO)
import System.IO.Error
import GHC.IO.Exception (IOErrorType(ResourceVanished))

import Foreign.Ptr
import Foreign.C.Error (Errno(..))
import Foreign.C.Types (CInt(..), CSize)
import System.Posix.Types (Fd, FileOffset, ByteCount)
import System.Posix.Internals (hostIsThreaded)

import qualified System.IO.BlockIO.URing as URing


-- | IO context: a handle used by threads submitting IO batches.
--
data IOCtx = IOCtx {
               -- | IO concurrency control: used by writers to reserve the
               -- right to submit an IO batch of a given size, and by the
               -- completion thread to return it on batch completion.
               ioctxQSemN :: !QSemN,

               -- | Locking of the writer end of the URing: used by writers
               -- while they are modifying the uring submission queue.
               ioctxURing :: !(MVar URing.URing),

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
      ioctxURing         <- newMVar uring
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
          initialBatchIxs =
            [IOBatchIx 0 .. IOBatchIx (fromIntegral (ioctxConcurrencyLimit-1))]
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
    uring <- takeMVar ioctxURing
    URing.prepareNop uring (URing.IOOpId maxBound)
    URing.submitIO uring
    takeMVar ioctxCloseSync
    URing.closeURing uring
    putMVar ioctxURing (throw closed)
  where
    closed = mkIOError ResourceVanished "IOCtx closed" Nothing Nothing

data IOOp = IOOpRead  !Fd !FileOffset !(Ptr Word8) !ByteCount
          | IOOpWrite !Fd !FileOffset !(Ptr Word8) !ByteCount
  deriving Show

newtype IOResult = IOResult_ URing.IOResult

pattern IOResult :: ByteCount -> IOResult
pattern IOResult c <- (viewIOResult -> Just c)
  where
    IOResult count = IOResult_ ((fromIntegral :: CSize -> CInt) count)

pattern IOError :: Errno -> IOResult
pattern IOError e <- (viewIOError -> Just e)
  where
    IOError (Errno e) = IOResult_ (-e)

viewIOResult :: IOResult -> Maybe ByteCount
viewIOResult (IOResult_ c)
  | c >= 0    = Just ((fromIntegral :: CInt -> CSize) c)
  | otherwise = Nothing

viewIOError  :: IOResult -> Maybe Errno
viewIOError (IOResult_ e)
  | e < 0     = Just (Errno e)
  | otherwise = Nothing


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
submitIO :: IOCtx -> [IOOp] -> IO [IOResult]
submitIO IOCtx {
           ioctxQSemN,
           ioctxURing,
           ioctxChanIOBatch,
           ioctxChanIOBatchIx
         }
         ioops = do
    let iobatchOpCount :: Word32
        !iobatchOpCount = fromIntegral (length ioops)
    waitQSemN ioctxQSemN (fromIntegral iobatchOpCount)
    iobatchIx         <- readChan ioctxChanIOBatchIx
    iobatchCompletion <- newEmptyMVar
    writeChan ioctxChanIOBatch
              IOBatch {
                iobatchIx,
                iobatchOpCount,
                iobatchCompletion
              }
    withMVar ioctxURing $ \uring -> do
--      print ("submitIO", iobatchOpCount)
      sequence_
        [ --print ioop >>
          case ioop of
           IOOpRead  fd off buf cnt ->
             URing.prepareRead  uring fd off buf cnt ioopid

           IOOpWrite fd off buf cnt ->
             URing.prepareWrite uring fd off buf cnt ioopid
        | (ioop, ioopix) <- zip ioops [IOOpIx 0 ..]
        , let !ioopid = packIOOpId iobatchIx ioopix ]
      URing.submitIO uring
--      print ("submitIO", "submitting done")
    map (IOResult_ . coerce) . elems <$> takeMVar iobatchCompletion

data IOBatch = IOBatch {
                 iobatchIx         :: !IOBatchIx,
                 iobatchOpCount    :: !Word32,
                 iobatchCompletion :: MVar (UArray IOOpIx Int32)
               }

newtype IOBatchIx = IOBatchIx Word32
  deriving (Eq, Ord, Ix, Enum, Show)

newtype IOOpIx = IOOpIx Word32
  deriving (Eq, Ord, Ix, Enum, Show)

{-# INLINE packIOOpId #-}
packIOOpId :: IOBatchIx -> IOOpIx -> URing.IOOpId
packIOOpId (IOBatchIx batchix) (IOOpIx opix) =
    URing.IOOpId $ unsafeShiftL (fromIntegral batchix) 32
                .|. fromIntegral opix

{-# INLINE unpackIOOpId #-}
unpackIOOpId :: URing.IOOpId -> (IOBatchIx, IOOpIx)
unpackIOOpId (URing.IOOpId w64) =
    (IOBatchIx batchix, IOOpIx opix)
  where
    batchix :: Word32
    batchix = fromIntegral (unsafeShiftR w64 32)

    opix :: Word32
    opix    = fromIntegral w64

completionThread :: URing.URing
                 -> MVar ()
                 -> Int
                 -> QSemN
                 -> Chan IOBatch
                 -> Chan IOBatchIx
                 -> IO ()
completionThread uring done maxc qsem chaniobatch chaniobatchix = do
    let iobatchixBounds :: (IOBatchIx, IOBatchIx)
        iobatchixBounds = (IOBatchIx 0, IOBatchIx (fromIntegral maxc-1))
    counts      <- newArray iobatchixBounds (-1)
    results     <- newArray iobatchixBounds invalidEntry
    completions <- newArray iobatchixBounds invalidEntry
    collectCompletion counts results completions
      `finally` putMVar done ()
  where
    collectCompletion :: IOUArray IOBatchIx Int
                      -> IOArray  IOBatchIx (IOUArray IOOpIx Int32)
                      -> IOArray  IOBatchIx (MVar (UArray IOOpIx Int32))
                      -> IO ()
    collectCompletion counts results completions = do
      iocompletion <- URing.awaitIO uring
      let (URing.IOCompletion ioopid iores) = iocompletion
      unless (ioopid == URing.IOOpId maxBound) $ do
        let (iobatchix, ioopix) = unpackIOOpId ioopid
        count <- do
          c <- readArray counts iobatchix
          if c < 0 then collectIOBatches iobatchix
                   else return c
        assert (count > 0) (return ())
        writeArray counts iobatchix (count-1)
        result <- readArray results iobatchix
        writeArray result ioopix (coerce iores)
        when (count == 1) $ do
          completion <- readArray completions iobatchix
          writeArray counts      iobatchix (-1)
          writeArray results     iobatchix invalidEntry
          writeArray completions iobatchix invalidEntry
          result' <- freeze result
          putMVar completion (result' :: UArray IOOpIx Int32)
          writeChan chaniobatchix iobatchix
          let !qrelease = rangeSize (bounds result')
          signalQSemN qsem qrelease
        collectCompletion counts results completions

      -- wait for single IO result
      -- if the count is positive, decrement and update result array
      -- if the count is now 0, also fill in the completion
      -- if the count was 0 before, grab next batch from the chan and process it
      -- may have to do that repeatedly until the batch count in question is positive
      -- reset count to -1, and result entries to undefined
      where
        collectIOBatches :: IOBatchIx -> IO Int
        collectIOBatches iobatchixNeeded = do
          IOBatch{
              iobatchIx,
              iobatchOpCount,
              iobatchCompletion
            } <- readChan chaniobatch
          oldcount <- readArray counts iobatchIx
          assert (oldcount == (-1)) (return ())
          writeArray counts iobatchIx (fromIntegral iobatchOpCount)
          result <- newArray (IOOpIx 0, IOOpIx (iobatchOpCount-1)) (-1)
          writeArray results iobatchIx result
          writeArray completions iobatchIx iobatchCompletion
          if iobatchIx == iobatchixNeeded
            then return $! fromIntegral iobatchOpCount
            else collectIOBatches iobatchixNeeded

    {-# NOINLINE invalidEntry #-}
    invalidEntry :: a
    invalidEntry =
      throw (UndefinedElement "System.IO.BlockIO.completionThread")
