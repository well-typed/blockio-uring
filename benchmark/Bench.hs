{-# LANGUAGE BangPatterns #-}
{- HLINT ignore "Use camelCase" -}

module Main (main) where

import Data.Primitive
import qualified Data.Set as Set
import Control.Monad
import Control.Exception
import Control.Concurrent.Async

import Foreign
import System.Posix.IO
import System.Posix.Files

import System.Random
import System.Environment
import System.Exit
import Data.Time

import System.IO.BlockIO
import System.IO.BlockIO.URing hiding (submitIO)
import qualified System.IO.BlockIO.URing as URing
import qualified Data.Vector as V

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["low",  filename] -> main_lowlevel  filename
    ["high", filename] -> main_highlevel filename
    _   -> do
      putStrLn "Usage: Bench [low|high] [DataFile]"
      exitFailure

main_lowlevel :: FilePath -> IO ()
main_lowlevel filename = do
  putStrLn "Low-level API benchmark"
  fd <- openFd filename ReadOnly defaultFileFlags
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
      report before after total


main_highlevel :: FilePath -> IO ()
main_highlevel filename = do
  putStrLn "High-level API benchmark"
  fd     <- openFd filename ReadOnly defaultFileFlags
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
      blocks    = V.fromList $ zip [0..] (randomPermute rng [0..lastBlock])
  bracket (initIOCtx params) closeIOCtx $ \ioctx -> do
    buf <- newPinnedByteArray (4096 * nbufs)

    before <- getCurrentTime
    forConcurrently_ (groupsOfN 32 blocks) $ \batch ->
      submitIO ioctx $ flip fmap batch $ \ (i, block) ->
        let bufOff  = (i `mod` nbufs) * 4096
            blockoff = fromIntegral (block * 4096)
        in  IOOpRead fd blockoff buf bufOff 4096
    after <- getCurrentTime
    let total   = lastBlock + 1
    report before after total

report :: UTCTime -> UTCTime -> Int -> IO ()
report before after total = do
    putStrLn $ "Total I/O ops: " ++ show total
    putStrLn $ "Elapsed time:  " ++ show elapsed
    putStrLn $ "IOPS:          " ++ show iops
  where
    elapsed = after `diffUTCTime` before

    iops    :: Int
    iops = round (fromIntegral total / realToFrac elapsed :: Double)

groupsOfN :: Int -> V.Vector a -> [V.Vector a]
groupsOfN n xs | V.null xs = []
               | otherwise = V.take n xs : groupsOfN n (V.drop n xs)

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

