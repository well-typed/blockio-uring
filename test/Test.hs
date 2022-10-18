{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Data.Set as Set
import Control.Monad
import Control.Exception
import Control.Concurrent.Async

import Foreign
import System.IO
import System.Posix.IO
import System.Posix.Files

import System.Random
import System.Environment
import System.Exit
import Data.Time

import System.IO.BlockIO
import System.IO.BlockIO.URing hiding (submitIO)
import qualified System.IO.BlockIO.URing as URing
import System.IO.BlockIO.URingFFI

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["1", filename] -> main1 filename
    ["2", filename] -> main2 filename
    ["3", filename] -> main3 filename
    ["4", filename] -> main4 filename
    _   -> do
      putStrLn "Usage: Test [TestNo (1..4)] [DataFile]"
      exitFailure

main1 :: FilePath -> IO ()
main1 filename = do
  fd <- openFd filename ReadOnly Nothing defaultFileFlags
  alloca $ \uring ->
    allocaBytes 4096 $ \bufptr -> do
      print =<< io_uring_queue_init 8 uring 0

      sqe <- io_uring_get_sqe uring
      io_uring_prep_read sqe fd bufptr 4096 0
      print =<< io_uring_submit uring

      cqe <- alloca $ \cqeptrptr -> do
        print =<< io_uring_wait_cqe uring cqeptrptr
        cqeptr <- peek cqeptrptr
        cqe <- peek cqeptr
        io_uring_cqe_seen uring cqeptr
        return cqe
 
      print cqe
      hPutBuf stdout bufptr (fromIntegral (cqe_res cqe))

      io_uring_queue_exit uring

main2 :: FilePath -> IO ()
main2 filename = do
  fd <- openFd filename ReadOnly Nothing defaultFileFlags
  withURing URingParams { uringSize = 8 } $ \uring ->

    allocaBytes 4096 $ \bufptr -> do

      prepareRead uring fd 0 bufptr 4096 (IOOpId 42)
      URing.submitIO uring

      (IOCompletion iometa len) <- awaitIO uring
      print (iometa, len)
      hPutBuf stdout bufptr (fromIntegral len)


main3 :: FilePath -> IO ()
main3 filename = do
  fd <- openFd filename ReadOnly Nothing defaultFileFlags
  status <- getFdStatus fd
  let size      = fileSize status
      lastBlock :: Int
      lastBlock = fromIntegral (size `div` 4096 - 1)
      nqueue    = 64
      nbufs     = 64 * 4
  withURing (URingParams nqueue) $ \uring ->
    allocaBytes (4096 * nbufs) $ \bufptr -> do
      let submitBatch :: [(Int, Int)] -> IO ()
          submitBatch blocks = do
            sequence_
              [ prepareRead uring fd blockoff bufptr' 4096
                            (IOOpId (fromIntegral i))
              | (i, block) <- blocks
              , let bufptr'  = bufptr `plusPtr` ((i `mod` nbufs) * 4096)
                    blockoff = fromIntegral (block * 4096)
              ]
            URing.submitIO uring

          collectBatch :: Int -> IO ()
          collectBatch n =
            replicateM_ n $ do
              (IOCompletion i count) <- awaitIO uring
              when (count /= 4096) $
                fail $ "I/O failure: I/O " ++ show i
                    ++ " returned " ++ show count

          go []     = return ()
          go blocks = do
            let (batch, blocks') = splitAt 32 blocks
            submitBatch batch
            let n = case blocks' of
                      [] -> length batch
                      _  -> 32
            collectBatch n
            go blocks'

      rng <- initStdGen
      let blocks = zip [0..] (randomPermute rng [0..lastBlock])
          total  = lastBlock + 1
          (leadIn, blocks') = splitAt 64 blocks

      before <- getCurrentTime
      submitBatch leadIn
      go blocks'
      collectBatch 64
      after <- getCurrentTime

      let elapsed = after `diffUTCTime` before
      print (total,
             elapsed,
             round (fromIntegral total / realToFrac elapsed :: Double) :: Int)

main4 :: FilePath -> IO ()
main4 filename = do
  fd     <- openFd filename ReadOnly Nothing defaultFileFlags
  status <- getFdStatus fd
  rng    <- initStdGen
  let size      = fileSize status
      lastBlock :: Int
      lastBlock = fromIntegral (size `div` 4096 - 1)
      nbufs     = 64 * 4
      params    = IOCtxParams {
                    ioctxBatchSizeLimit   = 64,
                    ioctxConcurrencyLimit = 64 * 4
                  }
      blocks    = zip [0..] (randomPermute rng [0..lastBlock])
  bracket (initIOCtx params) closeIOCtx $ \ioctx ->
    allocaBytes (4096 * nbufs) $ \bufptr -> do

      before <- getCurrentTime
      forConcurrently_ (groupsOfN 32 blocks) $ \batch ->
        submitIO ioctx
          [ IOOpRead fd blockoff bufptr' 4096
          | (i, block) <- batch
          , let bufptr'  = bufptr `plusPtr` ((i `mod` nbufs) * 4096)
                blockoff = fromIntegral (block * 4096)
          ]
      after <- getCurrentTime

      let total   = lastBlock + 1
          elapsed = after `diffUTCTime` before
          iops    :: Int
          iops    = round (fromIntegral total / realToFrac elapsed :: Double)
      print (total, elapsed, iops)

groupsOfN :: Int -> [a] -> [[a]]
groupsOfN _ [] = []
groupsOfN n xs = take n xs : groupsOfN n (drop n xs)


randomPermute :: Ord a => StdGen -> [a] -> [a]
randomPermute rng0 xs0 =
    go (Set.fromList xs0) rng0
  where
    go !xs !_rng | Set.null xs = []

    go !xs !rng = x : go xs' rng'
      where
        (i, rng') = uniformR (0, Set.size xs - 1) rng
        !x   = Set.elemAt i xs
        !xs' = Set.deleteAt i xs

