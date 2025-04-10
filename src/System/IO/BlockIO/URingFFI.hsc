{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fobject-code #-}

module System.IO.BlockIO.URingFFI where

import Foreign
import Foreign.C
import Prelude hiding (head, tail)
import System.Posix.Types
import Foreign.C.ConstPtr

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

foreign import capi unsafe "liburing.h io_uring_cq_has_overflow"
  io_uring_cq_has_overflow :: ConstPtr URing -> IO Bool

foreign import capi unsafe "liburing.h value IORING_FEAT_NODROP" iORING_FEAT_NODROP :: CUInt

deriving stock instance Eq URingParams
deriving stock instance Show URingParams

deriving stock instance Eq SQRingOffsets
deriving stock instance Show SQRingOffsets

deriving stock instance Eq CQRingOffsets
deriving stock instance Show CQRingOffsets

data {-# CTYPE "liburing.h" "struct io_sqring_offsets" #-}
     SQRingOffsets = SQRingOffsets {
                          sqo_head :: !CUInt,
                          sqo_tail :: !CUInt,
                          sqo_ring_mask :: !CUInt,
                          sqo_ring_entries :: !CUInt,
                          sqo_flags :: !CUInt,
                          sqo_dropped :: !CUInt,
                          sqo_array :: !CUInt,
                          sqo_resv1 :: !CUInt,
                          sqo_user_addr :: !CULong
                        }

instance Storable SQRingOffsets where
  sizeOf    _ = #{size      struct io_sqring_offsets}
  alignment _ = #{alignment struct io_sqring_offsets}
  peek      p = do sqo_head         <- #{peek struct io_sqring_offsets, head}         p
                   sqo_tail         <- #{peek struct io_sqring_offsets, tail}         p
                   sqo_ring_mask    <- #{peek struct io_sqring_offsets, ring_mask}    p
                   sqo_ring_entries <- #{peek struct io_sqring_offsets, ring_entries} p
                   sqo_flags        <- #{peek struct io_sqring_offsets, flags}        p
                   sqo_dropped      <- #{peek struct io_sqring_offsets, dropped}      p
                   sqo_array        <- #{peek struct io_sqring_offsets, array}        p
                   sqo_resv1        <- #{peek struct io_sqring_offsets, resv1}        p
                   sqo_user_addr    <- #{peek struct io_sqring_offsets, user_addr}    p
                   pure SQRingOffsets {..}
  poke      p SQRingOffsets{..} =
                do #{poke struct io_sqring_offsets, head}         p sqo_head
                   #{poke struct io_sqring_offsets, tail}         p sqo_tail
                   #{poke struct io_sqring_offsets, ring_mask}    p sqo_ring_mask
                   #{poke struct io_sqring_offsets, ring_entries} p sqo_ring_entries
                   #{poke struct io_sqring_offsets, flags}        p sqo_flags
                   #{poke struct io_sqring_offsets, dropped}      p sqo_dropped
                   #{poke struct io_sqring_offsets, array}        p sqo_array
                   #{poke struct io_sqring_offsets, resv1}        p sqo_resv1
                   #{poke struct io_sqring_offsets, user_addr}    p sqo_user_addr


data {-# CTYPE "liburing.h" "struct io_cqring_offsets" #-}
     CQRingOffsets = CQRingOffsets {
                          cqo_head :: !CUInt,
                          cqo_tail :: !CUInt,
                          cqo_ring_mask :: !CUInt,
                          cqo_ring_entries :: !CUInt,
                          cqo_overflow :: !CUInt,
                          cqo_cqes :: !CUInt,
                          cqo_flags :: !CUInt,
                          cqo_resv1 :: !CUInt,
                          cqo_user_addr :: !CULong
                        }

instance Storable CQRingOffsets where
  sizeOf    _ = #{size      struct io_cqring_offsets}
  alignment _ = #{alignment struct io_cqring_offsets}
  peek      p = do cqo_head         <- #{peek struct io_cqring_offsets, head}         p
                   cqo_tail         <- #{peek struct io_cqring_offsets, tail}         p
                   cqo_ring_mask    <- #{peek struct io_cqring_offsets, ring_mask}    p
                   cqo_ring_entries <- #{peek struct io_cqring_offsets, ring_entries} p
                   cqo_overflow     <- #{peek struct io_cqring_offsets, overflow}     p
                   cqo_cqes         <- #{peek struct io_cqring_offsets, cqes}         p
                   cqo_flags        <- #{peek struct io_cqring_offsets, flags}        p
                   cqo_resv1        <- #{peek struct io_cqring_offsets, resv1}        p
                   cqo_user_addr    <- #{peek struct io_cqring_offsets, user_addr}    p
                   pure CQRingOffsets {..}
  poke      p CQRingOffsets{..} =
                do #{poke struct io_cqring_offsets, head}         p cqo_head
                   #{poke struct io_cqring_offsets, tail}         p cqo_tail
                   #{poke struct io_cqring_offsets, ring_mask}    p cqo_ring_mask
                   #{poke struct io_cqring_offsets, ring_entries} p cqo_ring_entries
                   #{poke struct io_cqring_offsets, overflow}     p cqo_overflow
                   #{poke struct io_cqring_offsets, cqes}         p cqo_cqes
                   #{poke struct io_cqring_offsets, flags}        p cqo_flags
                   #{poke struct io_cqring_offsets, resv1}        p cqo_resv1
                   #{poke struct io_cqring_offsets, user_addr}    p cqo_user_addr

data {-# CTYPE "liburing.h" "struct io_uring_params" #-}
     URingParams = URingParams {
                      sq_entries :: !CUInt,
                      cq_entries :: !CUInt,
                      flags :: !CUInt,
                      sq_thread_cpu :: !CUInt,
                      sq_thread_idle :: !CUInt,
                      features :: !CUInt,
                      wq_fd :: !CUInt,
                      resv1 :: !CUInt,
                      resv2 :: !CUInt,
                      resv3 :: !CUInt,
                      sq_off :: !SQRingOffsets,
                      cq_off :: !CQRingOffsets
                    }

instance Storable URingParams where
  sizeOf    _    = #{size      struct io_uring_params}
  alignment _    = #{alignment struct io_uring_params}
  peek      p    = do sq_entries     <- #{peek struct io_uring_params, sq_entries} p
                      cq_entries     <- #{peek struct io_uring_params, cq_entries} p
                      flags          <- #{peek struct io_uring_params, flags} p
                      sq_thread_cpu  <- #{peek struct io_uring_params, sq_thread_cpu} p
                      sq_thread_idle <- #{peek struct io_uring_params, sq_thread_idle} p
                      features       <- #{peek struct io_uring_params, features} p
                      wq_fd          <- #{peek struct io_uring_params, wq_fd} p
                      let resvOff i = #{offset struct io_uring_params, resv} + i * 4
                      resv1 <- peekByteOff p (resvOff 0)
                      resv2 <- peekByteOff p (resvOff 1)
                      resv3 <- peekByteOff p (resvOff 2)
                      sq_off         <- #{peek struct io_uring_params, sq_off} p
                      cq_off         <- #{peek struct io_uring_params, cq_off} p
                      return URingParams {..}
  poke      p ps = do #{poke struct io_uring_params, sq_entries} p (sq_entries ps)
                      #{poke struct io_uring_params, cq_entries} p (cq_entries ps)
                      #{poke struct io_uring_params, flags} p (flags ps)
                      #{poke struct io_uring_params, sq_thread_cpu} p (sq_thread_cpu ps)
                      #{poke struct io_uring_params, sq_thread_idle} p (sq_thread_idle ps)
                      #{poke struct io_uring_params, features} p (features ps)
                      #{poke struct io_uring_params, wq_fd} p (wq_fd ps)
                      let resvOff i = #{offset struct io_uring_params, resv} + i * 4
                      pokeByteOff p (resvOff 0) (resv1 ps)
                      pokeByteOff p (resvOff 1) (resv2 ps)
                      pokeByteOff p (resvOff 2) (resv3 ps)
                      #{poke struct io_uring_params, sq_off} p (sq_off ps)
                      #{poke struct io_uring_params, cq_off} p (cq_off ps)

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

