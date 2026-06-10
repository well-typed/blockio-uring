{-# LANGUAGE InterruptibleFFI #-}

{-# OPTIONS_GHC -fobject-code #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module System.IO.BlockIO.DarwinFFI where

import Foreign
import Foreign.C

#include "darwin_io.h"

-- ---------------------------------------------------------------------------
-- URingCQE  (mirrors darwin_cqe layout)
-- ---------------------------------------------------------------------------

data URingCQE = URingCQE
    { cqe_data :: !CULong
    , cqe_res  :: !CInt
    } deriving stock Show

instance Storable URingCQE where
  sizeOf    _ = #{size      darwin_cqe}
  alignment _ = #{alignment darwin_cqe}
  peek p = URingCQE
              <$> #{peek darwin_cqe, user_data} p
              <*> #{peek darwin_cqe, res}       p
  poke _ _ = return ()

-- ---------------------------------------------------------------------------
-- C imports
-- ---------------------------------------------------------------------------

foreign import ccall unsafe "darwin_io.h darwin_ring_create"
  c_darwin_ring_create :: CUInt -> IO (Ptr ())

foreign import ccall unsafe "darwin_io.h darwin_ring_destroy"
  c_darwin_ring_destroy :: Ptr () -> IO ()

foreign import ccall unsafe "darwin_io.h darwin_prep_read"
  c_darwin_prep_read :: Ptr () -> CInt -> Ptr Word8 -> CUInt -> CULong -> CULong -> IO CInt

foreign import ccall unsafe "darwin_io.h darwin_prep_write"
  c_darwin_prep_write :: Ptr () -> CInt -> Ptr Word8 -> CUInt -> CULong -> CULong -> IO CInt

foreign import ccall unsafe "darwin_io.h darwin_prep_nop"
  c_darwin_prep_nop :: Ptr () -> CULong -> IO CInt

foreign import ccall unsafe "darwin_io.h darwin_submit"
  c_darwin_submit :: Ptr () -> IO CInt

-- interruptible so GHC can deliver async exceptions while blocked in kevent
foreign import ccall interruptible "darwin_io.h darwin_wait_cqe"
  c_darwin_wait_cqe :: Ptr () -> Ptr (Ptr URingCQE) -> IO CInt

foreign import ccall unsafe "darwin_io.h darwin_peek_cqe"
  c_darwin_peek_cqe :: Ptr () -> Ptr (Ptr URingCQE) -> IO CInt

foreign import ccall unsafe "darwin_io.h darwin_cqe_seen"
  c_darwin_cqe_seen :: Ptr () -> Ptr URingCQE -> IO ()
