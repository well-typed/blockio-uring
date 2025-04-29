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
import System.IO.Error
import System.Posix.Types

import Control.Monad
import Control.Exception

import qualified System.IO.BlockIO.URingFFI as FFI


--
-- Init
--

newtype URing = URing (Ptr FFI.URing)
data URingParams = URingParams {
                     sizeSQRing :: !Int,
                     sizeCQRing :: !Int
                   }

setupURing :: URingParams -> IO URing
setupURing URingParams { sizeSQRing, sizeCQRing } = do
    uringptr <- malloc
    alloca $ \paramsptr -> do
      poke paramsptr params
      throwErrnoResIfNegRetry_ "setupURing" $
        FFI.io_uring_queue_init_params
          (fromIntegral sizeSQRing)
          uringptr
          paramsptr
      params' <- peek paramsptr
      when (fromIntegral sizeSQRing /= FFI.sq_entries params') $ do
        FFI.io_uring_queue_exit uringptr
        throwIO (userError $ show (sizeSQRing, FFI.sq_entries params'))
      when (fromIntegral sizeCQRing > FFI.cq_entries params') $ do
        FFI.io_uring_queue_exit uringptr
        throwIO (userError $ show (sizeCQRing, FFI.cq_entries params'))
      return (URing uringptr)
  where
    flags  = FFI.iORING_SETUP_CQSIZE
    params = FFI.URingParams {
               FFI.sq_entries = 0,
               FFI.cq_entries = fromIntegral sizeCQRing,
               FFI.flags      = flags,
               FFI.features   = 0
             }

closeURing :: URing -> IO ()
closeURing (URing uringptr) = do
    FFI.io_uring_queue_exit uringptr
    free uringptr

withURing :: URingParams -> (URing -> IO a) -> IO a
withURing params =
    bracket (setupURing params) closeURing


--
-- Submitting I/O
--

newtype IOOpId = IOOpId Word64
  deriving (Eq, Ord, Bounded, Show)

prepareRead :: URing -> Fd -> FileOffset -> Ptr Word8 -> ByteCount -> IOOpId -> IO ()
prepareRead (URing uringptr) fd off buf len (IOOpId ioopid) = do
    sqeptr <- throwErrResIfNull "prepareRead" fullErrorType
                                "URing I/O queue full" $
      FFI.io_uring_get_sqe uringptr
    FFI.io_uring_prep_read sqeptr fd buf (fromIntegral len) (fromIntegral off)
    FFI.io_uring_sqe_set_data sqeptr (fromIntegral ioopid)

prepareWrite :: URing -> Fd -> FileOffset -> Ptr Word8 -> ByteCount -> IOOpId -> IO ()
prepareWrite (URing uringptr) fd off buf len (IOOpId ioopid) = do
    sqeptr <- throwErrResIfNull "prepareWrite" fullErrorType
                                "URing I/O queue full" $
      FFI.io_uring_get_sqe uringptr
    FFI.io_uring_prep_write sqeptr fd buf (fromIntegral len) (fromIntegral off)
    FFI.io_uring_sqe_set_data sqeptr (fromIntegral ioopid)

prepareNop :: URing -> IOOpId -> IO ()
prepareNop (URing uringptr) (IOOpId ioopid) = do
    sqeptr <- throwErrResIfNull "prepareNop" fullErrorType
                                "URing I/O queue full" $
      FFI.io_uring_get_sqe uringptr
    FFI.io_uring_prep_nop sqeptr
    FFI.io_uring_sqe_set_data sqeptr (fromIntegral ioopid)

submitIO :: URing -> IO ()
submitIO (URing uringptr) =
    throwErrnoResIfNegRetry_ "submitIO" $
      FFI.io_uring_submit uringptr


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

awaitIO :: URing -> IO IOCompletion
awaitIO (URing uringptr) =
    alloca $ \cqeptrptr -> do
      -- Try non-blocking first (unsafe FFI call)
      peekres <- FFI.io_uring_peek_cqe uringptr cqeptrptr
      -- But if nothing is available, use a blocking call (safe FFI call)
--      when (peekres == 0) $ print ("awaitIO: non-blocking")
      when (peekres /= 0) $ do
        if Errno (-peekres) == eAGAIN
          then do --print ("awaitIO: blocking")
                  throwErrnoResIfNegRetry_ "awaitIO (blocking)" $
                    FFI.io_uring_wait_cqe uringptr cqeptrptr
                  --print ("awaitIO: blocking complete")
          else throwIO $ errnoToIOError "awaitIO (non-blocking)"
                                        (Errno (-peekres)) Nothing Nothing
      cqeptr <- peek cqeptrptr
      FFI.URingCQE { FFI.cqe_data, FFI.cqe_res } <- peek cqeptr
      FFI.io_uring_cqe_seen uringptr cqeptr
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

