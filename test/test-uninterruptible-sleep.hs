{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns       #-}

module Main (main) where

import           Control.Concurrent.Async
import           Control.Exception        (SomeException, try)
import           Control.Monad            (forM_, when)
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.Primitive.ByteArray as P
import qualified Data.Vector              as V
import           GHC.IO.FD                (FD (..))
import           GHC.IO.Handle.FD         (handleToFd)
import           System.FilePath          ((</>))
import           System.IO
import           System.IO.BlockIO        (IOCtxParams (IOCtxParams, ioctxBatchSizeLimit, ioctxConcurrencyLimit),
                                           IOOp (IOOpRead), defaultIOCtxParams,
                                           submitIO, withIOCtx)
import           System.IO.Temp           (withSystemTempDirectory)

-- | An executable that tries to overflow the completion queue by submitting
-- requests much faster than the system can complete
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    withSystemTempDirectory "test-interrubtile-sleep" $ \dir -> do
      let path = dir </> "test-file"

      -- Create a file to run batches of I/O operations on
      BS.writeFile path (BS.pack [0 .. fileSize - 1])

      -- The concurrency limit is much higher than the batch size to increase
      -- the probability of overflowing the completion queue
      let params = IOCtxParams {
              ioctxBatchSizeLimit   = 64,
              ioctxConcurrencyLimit = 64 * 4
            }

      -- Set up context
      withIOCtx defaultIOCtxParams $ \ctx -> do
        withFile path ReadMode $ \h -> do
          FD { fdFD = fromIntegral -> fd } <- handleToFd h

          -- Create array for read results
          mba <- P.newPinnedByteArray fileSize

          -- Repeat @batchCount@ times
          forM_ [0 .. batchCount - 1] $ \i -> do

            -- Print progress
            when (i `mod` 1000 == 0) $ print i

            -- Submit batches of reads
            let ioops = V.fromList [
                    IOOpRead fd (fileOffset j) mba (bufferOffset j) pageSize
                  | j <- [0 .. batchSize  - 1]
                  ]
            submitIO ctx ioops
  where
    batchSize :: Integral a => a
    batchSize = 256

    batchCount :: Integral a => a
    batchCount = 5_000_000

    fileSize :: Integral a => a
    fileSize = batchSize * pageSize

    pageSize :: Integral a => a
    pageSize = 4096

    fileOffset, bufferOffset :: (Integral a, Integral b) => a -> b
    fileOffset i = fromIntegral i * pageSize
    bufferOffset i = fromIntegral i * pageSize
