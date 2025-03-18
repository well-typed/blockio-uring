{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use camelCase" -}

module Main (main) where

import           Control.Exception (SomeException, try)
import           Control.Monad
import           Data.Word         (Word64)
import qualified Data.Vector.Unboxed.Mutable as VUM
import           System.IO.BlockIO.URing
import qualified System.IO.BlockIO.URingFFI as FFI
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "test-internals" [
      testCase "example_simpleNoop 1" $ example_simpleNoop 1
    , testCase "example_simpleNoop maxBound" $ example_simpleNoop maxBound
    , testCase "example_multipleNoop" $ example_multipleNoop 42
    , testCase "example_timeouts" $ example_timeouts 42
    ]

example_simpleNoop :: Word64 -> Assertion
example_simpleNoop n = do
    uring <- setupURing (URingParams 1)
    prepareNop uring (IOOpId n)
    submitIO uring
    ioopids     <- VUM.new 1
    ioresults   <- VUM.new 1
    awaitIO uring 1 ioopids ioresults
    closeURing uring
    ioopid   <- VUM.read ioopids 0
    ioresult <- VUM.read ioresults 0
    IOCompletion (IOOpId n) (IOResult 0) @=? IOCompletion ioopid ioresult

example_multipleNoop :: Int -> Assertion
example_multipleNoop n = do
    uring <- setupURing (URingParams n)
    replicateM n $ prepareNop uring (IOOpId 42)
    submitIO uring
    ioopids     <- VUM.new n
    ioresults   <- VUM.new n
    awaitIO uring n ioopids ioresults
    closeURing uring
    sequence_
      [ do ioopid   <- VUM.read ioopids 0
           ioresult <- VUM.read ioresults 0
           IOCompletion (IOOpId 42) (IOResult 0)
             @=? IOCompletion ioopid ioresult
      | _ <- [0..n-1] ]

example_timeouts :: Int -> Assertion
example_timeouts n = do
    uring <- setupURing (URingParams n)
    forM_ [0 .. n-1] $ \i -> prepareTimeout uring (i * 50000) (IOOpId 42)
    submitIO uring
    ioopids     <- VUM.new (n*2)
    ioresults   <- VUM.new (n*2)
    awaitIO uring n ioopids ioresults
    closeURing uring
    sequence_
      [ do ioopid   <- VUM.read ioopids 0
           ioresult <- VUM.read ioresults 0
           IOCompletion (IOOpId 42) (IOError FFI._ETIME)
             @=? IOCompletion ioopid ioresult
      | _ <- [0..n-1] ]

deriving instance Eq IOCompletion
deriving instance Show IOCompletion
