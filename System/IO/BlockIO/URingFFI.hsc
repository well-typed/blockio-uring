{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE InterruptibleFFI #-}

{-# OPTIONS_GHC -fobject-code #-}

module System.IO.BlockIO.URingFFI where

import Foreign
import Foreign.C
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
  deriving Show

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

