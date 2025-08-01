{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{- HLINT ignore "Use camelCase" -}

module Main (main) where

import Data.Primitive
import qualified Data.Set as Set
import Control.Monad
import Control.Monad.Primitive (RealWorld)
import Control.Monad.ST (ST, stToIO)
import Control.Exception
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async as Async

import Foreign
import System.Posix.IO
import System.Posix.Files
import System.Posix.Types as Posix
import System.Posix.Fcntl

import System.Random as Random
import System.Environment
import System.Exit
import System.Mem (performMajorGC)
import Data.Time
import qualified GHC.Stats as RTS

import System.IO.BlockIO
import System.IO.BlockIO.URing hiding (submitIO)
import qualified System.IO.BlockIO.URing as URing

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

main :: IO ()
main = do
  args <- getArgs
  case args of
    [level,  cache, filename]
      | Just main_func <- parseLevel level
      , Just useCache <- parseCacheFlag cache
      -> main_func useCache filename
    _   -> do
      putStrLn "Usage: Bench [low|high] [Cache|NoCache] [DataFile]"
      exitFailure
  where
    parseLevel = \case
      "low" -> Just main_lowlevel
      "high" -> Just main_highlevel
      _ -> Nothing
    parseCacheFlag = \case
      "Cache" -> Just True
      "NoCache" -> Just False
      _ -> Nothing

main_lowlevel :: Bool -> FilePath -> IO ()
main_lowlevel useCache filename = do
  putStrLn "Low-level API benchmark"
  fd <- openFd filename ReadOnly defaultFileFlags

  fileSetCaching fd useCache
  putStrLn $ "File caching:    " ++ show useCache

  status <- getFdStatus fd
  let size      = fileSize status
      lastBlock :: Int
      lastBlock = fromIntegral (size `div` 4096 - 1)
      nqueue    = 64
      nbufs     = 64 * 4
  withURing (URingParams nqueue nbufs) $ \uring ->
    allocaBytesAligned (4096 * nbufs) 4096 $ \bufptr -> do
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
              when (count /= IOResult 4096) $
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
          totalOps = lastBlock + 1
          (leadIn, blocks') = splitAt 64 blocks

      withReport totalOps $ do
        submitBatch leadIn
        go blocks'
        collectBatch 64


{-# NOINLINE main_highlevel #-}
main_highlevel :: Bool -> FilePath -> IO ()
main_highlevel useCache filename = do
  putStrLn "High-level API benchmark"
  fd     <- openFd filename ReadOnly defaultFileFlags

  fileSetCaching fd useCache
  putStrLn $ "File caching:    " ++ show useCache

  status <- getFdStatus fd
  rng    <- initStdGen
  ncaps  <- getNumCapabilities
  let size      = fileSize status
      lastBlock :: Int
      lastBlock = fromIntegral (size `div` 4096 - 1)
      params    = IOCtxParams {
                    ioctxBatchSizeLimit   = 64,
                    ioctxConcurrencyLimit = 64 * 4
                  }
      ntasks    = 4 * ncaps
      batchsz   = 32
      nbatches  = lastBlock `div` (ntasks * batchsz) -- batches per task
      totalOps  = nbatches * batchsz * ntasks

  putStrLn $ "Capabilities:    " ++ show ncaps
  putStrLn $ "Threads     :    " ++ show ntasks

  bracket (initIOCtx params) closeIOCtx $ \ioctx ->
    withReport totalOps $ do
      tasks <-
        forRngSplitM ntasks rng $ \ n !rng_task ->
          Async.asyncOn n $ do
            buf <- newAlignedPinnedByteArray (4096 * batchsz) 4096
            -- Each thread gets its own reusable vector
            (v, unsafeGenerateIOOpsBatch) <- stToIO $ mkGenerateIOOpsBatch fd buf lastBlock batchsz
            forRngSplitM_ nbatches rng_task $ \ _ !rng_batch -> do
              stToIO $ unsafeGenerateIOOpsBatch rng_batch
              submitIO ioctx v
      _ <- Async.waitAnyCancel tasks
      return ()

{-# NOINLINE mkGenerateIOOpsBatch #-}
-- | Create a reusable vector and an /unsafe/ function to fill the vector with
-- random elements.
--
-- The returned vector is immutable, but the filler function mutates the vector
-- in place. The user should make sure not to use (e.g., read) the vector while
-- it's being filled.
mkGenerateIOOpsBatch :: Posix.Fd
                     -> MutableByteArray RealWorld
                     -> Int
                     -> Int
                     -> ST RealWorld ( V.Vector (IOOp RealWorld)
                                     , -- fill the vector with new randomly generated IOOps
                                       Random.StdGen -> ST RealWorld ()
                                     )
mkGenerateIOOpsBatch !fd !buf !lastBlock !size = do
    v <- VM.new size
    v' <- V.unsafeFreeze v
    pure (v', \rng -> go v rng 0)
  where
    go :: V.MVector RealWorld (IOOp RealWorld) -> Random.StdGen -> Int -> ST RealWorld ()
    go !_ !_   !i | i == size = return ()
    go !v !rng !i = do
      let (!block, !rng') = Random.uniformR (0, lastBlock) rng
          !bufOff   = i * 4096
          !blockoff = fromIntegral (block * 4096)
      VM.unsafeWrite v i $! IOOpRead fd blockoff buf bufOff 4096
      go v rng' (i+1)

{-# INLINE forRngSplitM_ #-}
forRngSplitM_ :: Monad m
              => Int
              -> Random.StdGen
              -> (Int -> Random.StdGen -> m a)
              -> m ()
forRngSplitM_ n rng0 action = go 0 rng0
  where
    go !i !_   | i == n = return ()
    go !i !rng = let (!rng', !rng'') = Random.splitGen rng
                  in action i rng' >> go (i+1) rng''

{-# INLINE forRngSplitM #-}
forRngSplitM :: Monad m
             => Int
             -> Random.StdGen
             -> (Int -> Random.StdGen -> m a)
             -> m [a]
forRngSplitM n rng0 action = go [] 0 rng0
  where
    go acc !i !_   | i == n = return (reverse acc)
    go acc !i !rng = let (!rng', !rng'') = Random.splitGen rng
                      in action i rng' >>= \x -> go (x:acc) (i+1) rng''

{-# INLINE withReport #-}
withReport :: Int -> IO () -> IO ()
withReport totalOps action = do
    performMajorGC
    beforeRTS  <- RTS.getRTSStats
    beforeTime <- getCurrentTime
    action
    afterTime <- getCurrentTime
    performMajorGC
    afterRTS  <- RTS.getRTSStats
    report beforeTime afterTime beforeRTS afterRTS totalOps

{-# NOINLINE report #-}
report :: UTCTime -> UTCTime -> RTS.RTSStats -> RTS.RTSStats -> Int -> IO ()
report beforeTime afterTime beforeRTS afterRTS totalOps = do
    putStrLn $ "Total I/O ops:   " ++ show totalOps
    putStrLn $ "Elapsed time:    " ++ show elapsed
    putStrLn $ "IOPS:            " ++ show iops
    putStrLn $ "Allocated total: " ++ show allocated
    putStrLn $ "Allocated per:   " ++ show apio
  where
    elapsed   = afterTime `diffUTCTime` beforeTime
    allocated = RTS.allocated_bytes afterRTS - RTS.allocated_bytes beforeRTS

    iops, apio :: Int
    iops = round (fromIntegral totalOps / realToFrac elapsed :: Double)
    apio = round (fromIntegral allocated / realToFrac totalOps :: Double)

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

