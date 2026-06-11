{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use camelCase" -}

module Main (main) where

import Data.Word (Word64)
import System.IO.BlockIO.URing
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "test-internals"
    [ testCase "example_simpleNoop 1"        $ example_simpleNoop 1
    , testCase "example_simpleNoop maxBound" $ example_simpleNoop maxBound
    ]

example_simpleNoop :: Word64 -> Assertion
example_simpleNoop n = do
    uring <- setupURing (URingParams 1)
    prepareNop uring (IOOpId n)
    submitIO uring
    completion <- awaitIO uring
    closeURing uring
    IOCompletion (IOOpId n) (IOResult 0) @=? completion

deriving stock instance Eq IOCompletion
deriving stock instance Show IOCompletion
