{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module System.IO.BlockIO.URing (
    URing,
    URingParams(..),
    setupURing,
    closeURing,
    withURing,
    IOOpId(..),
    prepareRead,
    prepareWrite,
    prepareNop,
    prepareTimeout,
    submitIO,
    IOCompletion(..),
    IOResult(IOResult, IOError),
    awaitIO,
  ) where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Vector.Unboxed.Mutable (RealWorld)
import qualified Data.Vector.Unboxed.Base

import Foreign
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import System.IO.Error
import System.Posix.Types

import Control.Monad
import Control.Exception

import qualified System.IO.BlockIO.URingFFI as FFI

--import Debug.Trace (traceEventIO)
import Debug.Trace (traceIO)

traceEventIO :: String -> IO ()
traceEventIO = traceIO


--
-- Init
--

data URing = URing {
               -- | The uring itself.
               uringptr  :: !(Ptr FFI.URing),

               -- | A pre-allocated buffer to help with FFI marshalling.
               cqeptrsfptr :: {-# UNPACK #-} !(ForeignPtr (Ptr FFI.URingCQE))
             }

data URingParams = URingParams {
                     sizeSQRing :: !Int,
                     sizeCQRing :: !Int
                   }

setupURing :: URingParams -> IO URing
setupURing URingParams { sizeSQRing, sizeCQRing } = do
    uringptr <- malloc
    cqeptrsfptr <- mallocForeignPtrArray sizeSQRing
    alloca $ \paramsptr -> do
      poke paramsptr params
      throwErrnoResIfNegRetry_ "setupURing" $
        FFI.io_uring_queue_init_params
          (fromIntegral sizeSQRing)
          uringptr
          paramsptr
      params' <- peek paramsptr
      when (fromIntegral sizeSQRing /= FFI.sq_entries params') $
        setupFailure uringptr $ "unexected SQ ring size "
                             ++ show (sizeSQRing, FFI.sq_entries params')
      when (fromIntegral sizeCQRing > FFI.cq_entries params') $ do
        setupFailure uringptr $ "unexected CQ ring size "
                             ++ show (sizeCQRing, FFI.cq_entries params')
    return URing { uringptr, cqeptrsfptr }
  where
    flags  = FFI.iORING_SETUP_CQSIZE
    params = FFI.URingParams {
               FFI.sq_entries = 0,
               FFI.cq_entries = fromIntegral sizeCQRing,
               FFI.flags      = flags,
               FFI.features   = 0
             }
    setupFailure uringptr msg = do
      FFI.io_uring_queue_exit uringptr
      throwIO (userError $ "setupURing initialisation failure: " ++ msg)

closeURing :: URing -> IO ()
closeURing URing {uringptr} = do
    FFI.io_uring_queue_exit uringptr
    free uringptr

withURing :: URingParams -> (URing -> IO a) -> IO a
withURing params =
    bracket (setupURing params) closeURing


--
-- Submitting I/O
--

-- | An identifier that is submitted with the I\/O operation and returned with
-- the completion.
newtype IOOpId = IOOpId Word64
  deriving (Eq, Ord, Bounded, Show)

prepareRead :: URing -> Fd -> FileOffset -> Ptr Word8 -> ByteCount -> IOOpId -> IO ()
prepareRead URing {uringptr} fd off buf len (IOOpId ioopid) = do
    sqeptr <- throwErrResIfNull "prepareRead" fullErrorType
                                "URing I/O queue full" $
      FFI.io_uring_get_sqe uringptr
    FFI.io_uring_prep_read sqeptr fd buf (fromIntegral len) (fromIntegral off)
    FFI.io_uring_sqe_set_data sqeptr (fromIntegral ioopid)

prepareWrite :: URing -> Fd -> FileOffset -> Ptr Word8 -> ByteCount -> IOOpId -> IO ()
prepareWrite URing {uringptr} fd off buf len (IOOpId ioopid) = do
    sqeptr <- throwErrResIfNull "prepareWrite" fullErrorType
                                "URing I/O queue full" $
      FFI.io_uring_get_sqe uringptr
    FFI.io_uring_prep_write sqeptr fd buf (fromIntegral len) (fromIntegral off)
    FFI.io_uring_sqe_set_data sqeptr (fromIntegral ioopid)

prepareNop :: URing -> IOOpId -> IO ()
prepareNop URing {uringptr} (IOOpId ioopid) = do
    sqeptr <- throwErrResIfNull "prepareNop" fullErrorType
                                "URing I/O queue full" $
      FFI.io_uring_get_sqe uringptr
    FFI.io_uring_prep_nop sqeptr
    FFI.io_uring_sqe_set_data sqeptr (fromIntegral ioopid)

prepareTimeout :: URing -> Int -> IOOpId -> IO ()
prepareTimeout URing {uringptr} nsec (IOOpId ioopid) = do
    sqeptr <- throwErrResIfNull "prepareTimeout" fullErrorType
                                "URing I/O queue full" $
      FFI.io_uring_get_sqe uringptr
    with (FFI.TimeSpec 0 (fromIntegral nsec)) $ \ts ->
      FFI.io_uring_prep_timeout sqeptr ts 1 FFI._IORING_TIMEOUT_ETIME_SUCCESS
    FFI.io_uring_sqe_set_data sqeptr (fromIntegral ioopid)

submitIO :: URing -> IO ()
submitIO URing {uringptr} = do
    traceEventIO "START: submitIO"
    throwErrnoResIfNegRetry_ "submitIO" $
      FFI.io_uring_submit uringptr
    traceEventIO "END: submitIO"


--
-- Types for completing I/O
--

data IOCompletion = IOCompletion !IOOpId !IOResult

newtype IOResult = IOResult_ Int
  deriving (Eq, Show)

{-# COMPLETE IOResult, IOError #-}

pattern IOResult :: ByteCount -> IOResult
pattern IOResult c <- (viewIOResult -> Just c)
  where
    IOResult count = IOResult_ ((fromIntegral :: CSize -> Int) count)

pattern IOError :: Errno -> IOResult
pattern IOError e <- (viewIOError -> Just e)
  where
    IOError (Errno e) = IOResult_ (fromIntegral (-e))

viewIOResult :: IOResult -> Maybe ByteCount
viewIOResult (IOResult_ c)
  | c >= 0    = Just ((fromIntegral :: Int -> CSize) c)
  | otherwise = Nothing

viewIOError  :: IOResult -> Maybe Errno
viewIOError (IOResult_ e)
  | e < 0     = Just (Errno (fromIntegral e))
  | otherwise = Nothing


--
-- Unboxed vector support for IOResult and IOOp
--

newtype instance VUM.MVector s IOResult = MV_IOResult (VP.MVector s Int)
newtype instance VU.Vector     IOResult = V_IOResult  (VP.Vector    Int)

deriving newtype instance VGM.MVector VUM.MVector IOResult
deriving newtype instance VG.Vector   VU.Vector   IOResult

instance VU.Unbox IOResult

newtype instance VUM.MVector s IOOpId = MV_IOOpId (VP.MVector s Word64)
newtype instance VU.Vector     IOOpId = V_IOOpId  (VP.Vector    Word64)

deriving newtype instance VGM.MVector VUM.MVector IOOpId
deriving newtype instance VG.Vector   VU.Vector   IOOpId

instance VU.Unbox IOOpId


--
-- Completing I/O
--

-- | Wait for a batch of at least n completions and write the results into
-- the given mutable arrays, returning the number of completions written.
-- The returned count is guaranteed to be at least n (assuming the result
-- arrays are big enough), but it may return more if more completions are
-- available without waiting.
--
-- The count @n@ should be no more than the 'uringSize'. The arrays should be
-- of equal length and should be at least @n@ large. The maximum number of
-- results written into the arrays is bounded by their length.
--
-- This is not thread safe and must only be called from one thread at once.
--
awaitIO :: URing
        -> Int -- ^ Wait for at least @n@ I\/O completions.
        -> VUM.MVector RealWorld IOOpId
           -- ^ To be filled in. Should be at least @n@ large.
        -> VUM.MVector RealWorld IOResult
           -- ^ To be filled in. Should be at least @n@ large.
        -> IO Int
awaitIO !URing { uringptr, cqeptrsfptr } !count !ioopids !ioresults = do

    traceEventIO ("START: awaitIO " ++ show count)
    -- Algorithm outline:
    -- iterate until the count is decreased to zero:
    --   io_uring_peek_batch_cqe remaining count
    --   io_uring_wait_cqe_nr remaining count
    -- fill in result arrays
    -- io_uring_cq_advance count

    let -- The number of IO completions we will wait for.
        !nwait = min count nmax

        -- The maximum number of IO completions we can return. This may be
        -- more than nwait, the number we wait for.
        !nmax  = min (VUM.length ioopids) (VUM.length ioresults)

        -- We use unsafeForeignPtrToPtr and touchForeignPtr here rather than
        -- withForeignPtr because using withForeignPtr defeats GHCs CPR
        -- analysis which causes the 'IOCompletion' result to be allocated on
        -- the heap rather than returned in registers.
        !cqeptrsptr = unsafeForeignPtrToPtr cqeptrsfptr

    traceEventIO "START: collectAndAwaitIO"
    nmax' <- collectAndAwaitIO 0 nwait nmax cqeptrsptr
    traceEventIO "END: collectAndAwaitIO"
    touchForeignPtr cqeptrsfptr
    traceEventIO ("END: awaitIO " ++ show count)
    return (nmax - nmax')
  where
    collectAndAwaitIO :: Int -> Int -> Int -> Ptr (Ptr FFI.URingCQE) -> IO Int
    collectAndAwaitIO !i !nwait !nmax !cqeptrsptr  = do
      traceEventIO ("collectAndAwaitIO, i=" ++ show i
                                   ++ " nwait=" ++ show nwait
                                   ++ " nmax="  ++ show nmax)
      traceEventIO ("START: io_uring_peek_batch_cqe nmax=" ++ show nmax)
      nfilled <- fromIntegral <$>
                 FFI.io_uring_peek_batch_cqe uringptr cqeptrsptr
                                             (fromIntegral nmax)
      traceEventIO ("END: io_uring_peek_batch_cqe, nfilled=" ++ show nfilled)
      fillResults i (i+nfilled) cqeptrsptr
      FFI.io_uring_cq_advance uringptr (fromIntegral nfilled)

      let !i'          = i     + nfilled
          !nwait'      = nwait - nfilled
          !nmax'       = nmax  - nfilled
          !cqeptrsptr' = advancePtr cqeptrsptr nfilled

      if nwait' <= 0
        then return nmax'
        else do
        traceEventIO ("START: io_uring_wait_cqe_nr, nwait=" ++ show nwait')
        throwErrnoResIfNegRetry_ "awaitIO (blocking)" $
          FFI.io_uring_wait_cqe_nr uringptr cqeptrsptr'
                                   (fromIntegral nwait')
        traceEventIO "END: io_uring_wait_cqe_nr"
        nready <- FFI.io_uring_cq_ready uringptr
        traceEventIO ("io_uring_cq_ready " ++ show nready)
        --assert (fromIntegral nready >= nwait') (return ())

        collectAndAwaitIO i' nwait' nmax' cqeptrsptr'

    fillResults :: Int -> Int -> Ptr (Ptr FFI.URingCQE) -> IO ()
    fillResults !i !n !_ | i == n = return ()
    fillResults !i !n !cqeptrptr  = do
      traceEventIO ("fillResults " ++ show (i,n))
      cqeptr <- peek cqeptrptr
      FFI.URingCQE { FFI.cqe_data, FFI.cqe_res } <- peek cqeptr
      VUM.write ioopids   i (IOOpId    (fromIntegral cqe_data))
      VUM.write ioresults i (IOResult_ (fromIntegral cqe_res))
      fillResults (i+1) n (advancePtr cqeptrptr 1)

--
-- Utils
--

throwErrnoResIfNegRetry_ :: String -> IO CInt -> IO ()
throwErrnoResIfNegRetry_ label action = go
  where
    go = do
      res <- action
      when (res < 0) $
        if Errno (-res) == eINTR
          then go
          else throwIO $
                 errnoToIOError
                   label
                   (Errno (-res))
                   Nothing Nothing

throwErrResIfNull :: String -> IOErrorType -> String -> IO (Ptr a) -> IO (Ptr a)
throwErrResIfNull location ioErrorType description action = do
    res <- action
    if res == nullPtr
      then throwIO $
             ioeSetErrorString
              (mkIOError
                 ioErrorType
                 location
                 Nothing Nothing)
              description
      else return res

