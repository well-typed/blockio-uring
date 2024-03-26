{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use camelCase" -}

module Main (main) where

import           Control.Exception       (SomeException, try)
import           Data.Word               (Word64)
import           System.IO.BlockIO
import           System.IO.BlockIO.URing as URing
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "test" [
      testCase "example_simpleNoop 1" $ example_simpleNoop 1
    , testCase "example_simpleNoop maxBound" $ example_simpleNoop maxBound
    , testCase "example_initClose" example_initClose
    , testCase "example_closeIsIdempotent" example_closeIsIdempotent
    ]

example_simpleNoop :: Word64 -> Assertion
example_simpleNoop n = do
    uring <- setupURing (URingParams 1)
    prepareNop uring (IOOpId n)
    URing.submitIO uring
    completion <- awaitIO uring
    closeURing uring
    IOCompletion (IOOpId n) 0 @=? completion

deriving instance Eq IOCompletion
deriving instance Show IOCompletion

example_initClose :: Assertion
example_initClose = do
    ctx <- initIOCtx defaultIOCtxParams
    closeIOCtx ctx

example_closeIsIdempotent :: Assertion
example_closeIsIdempotent = do
    ctx <- initIOCtx defaultIOCtxParams
    closeIOCtx ctx
    eith <- try @SomeException (closeIOCtx ctx)
    case eith of
      Left (e :: SomeException) ->
        assertFailure ("Close on a closed context threw an error : " <> show e)
      Right () ->
        pure ()
