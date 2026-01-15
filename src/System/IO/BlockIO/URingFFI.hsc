{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module System.IO.BlockIO.URingFFI where

import Data.Void
import Foreign
import Foreign.C
import Prelude hiding (head, tail)
import System.Posix.Types

#include <liburing.h>

data {-# CTYPE "liburing.h" "struct io_uring" #-} URing = URing

instance Storable URing where
  sizeOf    _ = #{size      struct io_uring}
  alignment _ = #{alignment struct io_uring}
  peek      _ = return URing
  poke    _ _ = return ()

foreign import capi unsafe "liburing.h io_uring_queue_init"
  io_uring_queue_init :: CUInt -> Ptr URing -> CUInt -> IO CInt

foreign import capi unsafe "liburing.h io_uring_queue_exit"
  io_uring_queue_exit :: Ptr URing -> IO ()

foreign import capi safe "liburing.h io_uring_queue_init_params"
  io_uring_queue_init_params :: CUInt -> Ptr URing -> Ptr URingParams -> IO CInt

iORING_SETUP_CQSIZE :: CUInt
iORING_SETUP_CQSIZE = #{const IORING_SETUP_CQSIZE}

data {-# CTYPE "liburing.h" "struct io_uring_params" #-}
     URingParams = URingParams {
                      sq_entries :: !CUInt,
                      cq_entries :: !CUInt,
                      flags      :: !CUInt,
                      features   :: !CUInt
                      -- Note: this is a subset of all the fields. These are
                      -- just the ones we need now or are likely too need.
                      -- If you need more, just add them.
                    }
  deriving stock (Show, Eq)

instance Storable URingParams where
  sizeOf    _    = #{size      struct io_uring_params}
  alignment _    = #{alignment struct io_uring_params}
  peek      p    = do sq_entries     <- #{peek struct io_uring_params, sq_entries} p
                      cq_entries     <- #{peek struct io_uring_params, cq_entries} p
                      flags          <- #{peek struct io_uring_params, flags} p
                      features       <- #{peek struct io_uring_params, features} p
                      return URingParams {..}
  poke      p ps = do -- As we only cover a subset of the fields, we must clear
                      -- the remaining fields we don't set to avoid them
                      -- containing arbitrary values.
                      fillBytes p 0 #{size struct io_uring_params}

                      #{poke struct io_uring_params, sq_entries} p (sq_entries ps)
                      #{poke struct io_uring_params, cq_entries} p (cq_entries ps)
                      #{poke struct io_uring_params, flags} p (flags ps)
                      #{poke struct io_uring_params, features} p (features ps)


--
-- Submitting I/O
--

data {-# CTYPE "liburing.h" "struct io_uring_sqe" #-} URingSQE

foreign import capi unsafe "liburing.h io_uring_get_sqe"
  io_uring_get_sqe :: Ptr URing -> IO (Ptr URingSQE)

#ifdef LIBURING_HAVE_DATA64
foreign import capi unsafe "liburing.h io_uring_sqe_set_data64"
  io_uring_sqe_set_data :: Ptr URingSQE -> CULong -> IO ()
#else
io_uring_sqe_set_data :: Ptr URingSQE -> CULong -> IO ()
io_uring_sqe_set_data p user_data =
  do #{poke struct io_uring_sqe, user_data} p user_data
#endif

foreign import capi unsafe "liburing.h io_uring_prep_read"
  io_uring_prep_read :: Ptr URingSQE -> Fd -> Ptr Word8 -> CUInt -> CULong -> IO ()

foreign import capi unsafe "liburing.h io_uring_prep_write"
  io_uring_prep_write :: Ptr URingSQE -> Fd -> Ptr Word8 -> CUInt -> CULong -> IO ()

foreign import capi unsafe "liburing.h io_uring_prep_nop"
  io_uring_prep_nop :: Ptr URingSQE -> IO ()

io_uring_prep_cancel64_shim :: Ptr URingSQE -> CULong -> CInt -> IO ()
#ifdef LIBURING_HAVE_DATA64
io_uring_prep_cancel64_shim p user_data flags =
    io_uring_prep_cancel64 p user_data flags
#else
io_uring_prep_cancel64_shim p user_data flags =
    alloca $ \(p_user_data :: Ptr CULong) -> do
      poke (castPtr p_user_data) user_data
      io_uring_prep_cancel p p_user_data flags
#endif

foreign import capi unsafe "liburing.h io_uring_prep_cancel64"
  io_uring_prep_cancel64 :: Ptr URingSQE -> CULong -> CInt -> IO ()

foreign import capi unsafe "liburing.h io_uring_prep_cancel"
  io_uring_prep_cancel :: Ptr URingSQE -> Ptr Void -> CInt -> IO ()

iORING_ASYNC_CANCEL_ANY :: CUInt
iORING_ASYNC_CANCEL_ANY = #{const IORING_ASYNC_CANCEL_ANY}

foreign import capi unsafe "liburing.h io_uring_submit"
  io_uring_submit :: Ptr URing -> IO CInt


--
-- Collecting I/O
--

data {-# CTYPE "liburing.h" "struct io_uring_cqe" #-}
     URingCQE = URingCQE {
                  cqe_data  :: !CULong,
                  cqe_res   :: !CInt
                }
  deriving stock Show

instance Storable URingCQE where
  sizeOf    _ = #{size      struct io_uring_cqe}
  alignment _ = #{alignment struct io_uring_cqe}
  peek      p = do cqe_data  <- #{peek struct io_uring_cqe, user_data} p
                   cqe_res   <- #{peek struct io_uring_cqe, res} p
                   return URingCQE { cqe_data, cqe_res }
  poke    _ _ = return ()


foreign import capi safe "liburing.h io_uring_wait_cqe"
  io_uring_wait_cqe :: Ptr URing -> Ptr (Ptr URingCQE) -> IO CInt

foreign import capi unsafe "liburing.h io_uring_peek_cqe"
  io_uring_peek_cqe :: Ptr URing -> Ptr (Ptr URingCQE) -> IO CInt

foreign import capi unsafe "liburing.h io_uring_cqe_seen"
  io_uring_cqe_seen :: Ptr URing -> Ptr URingCQE -> IO ()

