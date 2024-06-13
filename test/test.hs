{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import           Control.Exception        (SomeException, try)
import qualified Data.Primitive.ByteArray as P
import qualified Data.Vector              as V
import           GHC.IO.FD                (FD (..))
import           GHC.IO.Handle.FD         (handleToFd)
import           System.IO
import           System.IO.BlockIO
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "test"
    [ testCase "example_initClose" example_initClose
    , testCase "example_initReadClose 32" $ example_initReadClose 32
    , testCase "example_initReadClose 96" $ example_initReadClose 96
    , testCase "example_initReadClose 200" $ example_initReadClose 200
    , testCase "example_closeIsIdempotent" example_closeIsIdempotent
    ]

example_initClose :: Assertion
example_initClose = do
    ctx <- initIOCtx defaultIOCtxParams
    closeIOCtx ctx

example_initReadClose :: Int -> Assertion
example_initReadClose size = do
    ctx <- initIOCtx defaultIOCtxParams
    withFile "blockio-uring.cabal" ReadMode $ \hdl -> do
        -- handleToFd is available since base-4.16.0.0
        FD { fdFD = fromIntegral -> fd } <- handleToFd hdl
        mba <- P.newPinnedByteArray 10 -- TODO: shouldn't use the same array for all ops :)
        submitIO ctx $ V.replicate size $
            IOOpRead fd 0 mba 0 10
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
