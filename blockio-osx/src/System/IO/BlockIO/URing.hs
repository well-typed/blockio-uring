{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeFamilies     #-}

module System.IO.BlockIO.URing (
    URing,
    URingParams(..),
    setupURing,
    closeURing,
    withURing,
    setIOWait,
    IOOpId(..),
    prepareRead,
    prepareWrite,
    prepareNop,
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
import qualified Data.Vector.Unboxed.Base

import Foreign
import Foreign.C
import Foreign.ForeignPtr.Unsafe
import System.Posix.Types

import Control.Monad
import Control.Exception

import qualified System.IO.BlockIO.DarwinFFI as FFI


--
-- Init
--

data URing = URing {
               -- | Pointer to the darwin_ring returned by darwin_ring_create.
               ringptr    :: !(Ptr ()),

               -- | A pre-allocated buffer to help with FFI marshalling.
               cqeptrfptr :: {-# UNPACK #-} !(ForeignPtr (Ptr FFI.URingCQE))
             }

-- | @sizeSQRing@ sets the capacity of the pending submission buffer —
-- the maximum number of operations that can be queued between a
-- 'setupURing' call and a 'submitIO' call.
newtype URingParams = URingParams { sizeSQRing :: Int }

setupURing :: URingParams -> IO URing
setupURing (URingParams sizeSQRing) = do
    ringptr <- FFI.c_darwin_ring_create (fromIntegral sizeSQRing)
    when (ringptr == nullPtr) $
        throwIO (userError "setupURing: darwin_ring_create failed")
    cqeptrfptr <- mallocForeignPtr
    return URing { ringptr, cqeptrfptr }

closeURing :: URing -> IO ()
closeURing URing {ringptr} = FFI.c_darwin_ring_destroy ringptr

withURing :: URingParams -> (URing -> IO a) -> IO a
withURing params = bracket (setupURing params) closeURing

-- | No-op on Darwin: @io_uring_set_iowait@ is a Linux-only feature.
setIOWait :: URing -> Bool -> IO ()
setIOWait _ _ = pure ()

--
-- Submitting I/O
--

-- | An identifier that is submitted with the I\/O operation and returned with
-- the completion.
newtype IOOpId = IOOpId Word64
  deriving stock (Eq, Ord, Bounded, Show)

prepareRead :: URing -> Fd -> FileOffset -> Ptr Word8 -> ByteCount -> IOOpId -> IO ()
prepareRead URing {ringptr} (Fd fd) off buf len (IOOpId ioopid) =
    throwErrnoResIfNegRetry_ "prepareRead" $
        FFI.c_darwin_prep_read ringptr fd buf
            (fromIntegral len) (fromIntegral off) (fromIntegral ioopid)

prepareWrite :: URing -> Fd -> FileOffset -> Ptr Word8 -> ByteCount -> IOOpId -> IO ()
prepareWrite URing {ringptr} (Fd fd) off buf len (IOOpId ioopid) =
    throwErrnoResIfNegRetry_ "prepareWrite" $
        FFI.c_darwin_prep_write ringptr fd buf
            (fromIntegral len) (fromIntegral off) (fromIntegral ioopid)

prepareNop :: URing -> IOOpId -> IO ()
prepareNop URing {ringptr} (IOOpId ioopid) =
    throwErrnoResIfNegRetry_ "prepareNop" $
        FFI.c_darwin_prep_nop ringptr (fromIntegral ioopid)

submitIO :: URing -> IO ()
submitIO URing {ringptr} =
    throwErrnoResIfNegRetry_ "submitIO" $
        FFI.c_darwin_submit ringptr


--
-- Types for completing I/O
--

data IOCompletion = IOCompletion !IOOpId !IOResult

newtype IOResult = IOResult_ Int
  deriving stock (Eq, Show)

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
-- Unboxed vector support for IOResult
--

newtype instance VUM.MVector s IOResult = MV_IOResult (VP.MVector s Int)
newtype instance VU.Vector     IOResult = V_IOResult  (VP.Vector    Int)

deriving newtype instance VGM.MVector VUM.MVector IOResult
deriving newtype instance VG.Vector   VU.Vector   IOResult

instance VU.Unbox IOResult


--
-- Completing I/O
--

-- | Must only be called from one thread at once.
awaitIO :: URing -> IO IOCompletion
awaitIO URing {ringptr, cqeptrfptr} = do
      -- We use unsafeForeignPtrToPtr and touchForeignPtr here rather than
      -- withForeignPtr because using withForeignPtr defeats GHCs CPR analysis
      -- which causes the 'IOCompletion' result to be allocated on the heap
      -- rather than returned in registers.
      let !cqeptrptr = unsafeForeignPtrToPtr cqeptrfptr
      -- Try non-blocking first (unsafe FFI call)
      peekres <- FFI.c_darwin_peek_cqe ringptr cqeptrptr
      -- But if nothing is available, use a blocking call (safe FFI call)
      when (peekres /= 0) $ do
        if Errno (-peekres) == eAGAIN
          then throwErrnoResIfNegRetry_ "awaitIO (blocking)" $
                   FFI.c_darwin_wait_cqe ringptr cqeptrptr
          else throwIO $ errnoToIOError "awaitIO (non-blocking)"
                                        (Errno (-peekres)) Nothing Nothing
      cqeptr <- peek cqeptrptr
      FFI.URingCQE { FFI.cqe_data, FFI.cqe_res } <- peek cqeptr
      FFI.c_darwin_cqe_seen ringptr cqeptr
      touchForeignPtr cqeptrfptr
      let opid = IOOpId (fromIntegral cqe_data)
          res  = IOResult_ (fromIntegral cqe_res)
      return $! IOCompletion opid res


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
