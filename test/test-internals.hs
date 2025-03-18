{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use camelCase" -}

module Main (main) where

import           Control.Concurrent          (threadDelay)
import           Control.Exception           (SomeException, try)
import           Control.Monad
import           Data.Proxy
import qualified Data.Vector.Storable        as VS
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Data.Word                   (Word64)
import           System.IO                   (BufferMode (NoBuffering),
                                              hSetBuffering)
import           System.IO.BlockIO.URing
import qualified System.IO.BlockIO.URingFFI  as FFI
import           Test.QuickCheck.Classes
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "test-internals" [
      testCase "example_simpleNoop 1" $ example_simpleNoop 1
    , testCase "example_simpleNoop maxBound" $ example_simpleNoop maxBound
    , testCase "example_multipleNoop" $ example_multipleNoop 42
    , testCase "example_timeouts" $ example_timeouts 42
    , testClassLaws "URingParams" $ storableLaws (Proxy @FFI.URingParams)
    ]

example_simpleNoop :: Word64 -> Assertion
example_simpleNoop n = do
    uring <- setupURing (URingParams 1 2)
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
    uring <- setupURing (URingParams n (2* n))
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
    uring <- setupURing (URingParams n (2* n))
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

{-------------------------------------------------------------------------------
  Storable
-------------------------------------------------------------------------------}

testClassLaws :: String -> Laws -> TestTree
testClassLaws typename laws = testClassLawsWith typename laws testProperty

testClassLawsWith ::
     String -> Laws
  -> (String -> Property -> TestTree)
  -> TestTree
testClassLawsWith typename Laws {lawsTypeclass, lawsProperties} k =
  testGroup ("class laws" ++ lawsTypeclass ++ " " ++ typename)
    [ k name prop
    | (name, prop) <- lawsProperties ]

instance Arbitrary FFI.URingParams where
  arbitrary = FFI.URingParams
    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (FFI.URingParams a b c d) = [
      FFI.URingParams a' b' c' d'
    | (a', b', c', d') <- shrink (a, b, c, d)
    ]
