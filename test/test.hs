module Main (main) where

import           Control.Exception (SomeException, try)
import qualified Data.Vector       as V
import           System.IO.BlockIO
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "test"
    [ testCase "example_initClose" example_initClose
    , testCase "example_initEmptyClose" example_initEmptyClose
    , testCase "example_closeIsIdempotent" example_closeIsIdempotent
    ]

example_initClose :: Assertion
example_initClose = do
    ctx <- initIOCtx defaultIOCtxParams
    closeIOCtx ctx

example_initEmptyClose :: Assertion
example_initEmptyClose = do
    ctx <- initIOCtx defaultIOCtxParams
    _ <- submitIO ctx V.empty
    closeIOCtx ctx

example_closeIsIdempotent :: Assertion
example_closeIsIdempotent = do
    ctx <- initIOCtx defaultIOCtxParams
    closeIOCtx ctx
    eith <- try (closeIOCtx ctx)
    case eith of
      Left e ->
        assertFailure ("Close on a closed context threw an error : " <> show (e :: SomeException))
      Right () ->
        pure ()
