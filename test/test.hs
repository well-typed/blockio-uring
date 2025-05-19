{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use camelCase" -}

module Main (main) where

import           Control.Exception        (Exception (displayException),
                                           IOException, SomeException, try)
import           Control.Monad            (void)
import           Data.List                (isPrefixOf)
import qualified Data.Primitive.ByteArray as P
import qualified Data.Vector.Unboxed      as VU
import           GHC.IO.Exception         (IOException (ioe_location))
import           GHC.IO.FD                (FD (..))
import           GHC.IO.Handle.FD         (handleToFd)
import           System.IO
import           System.IO.BlockIO
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "test"
    [ testCase "example_initClose" example_initClose
    , testCase "example_initReadClose 32" $ example_initReadClose 32
    , testCase "example_initReadClose 96" $ example_initReadClose 96
    , testCase "example_initReadClose 200" $ example_initReadClose 200
    , testCase "example_initEmptyClose" example_initEmptyClose
    , testCase "example_closeIsIdempotent" example_closeIsIdempotent
    , testProperty "prop_ValidIOCtxParams" prop_ValidIOCtxParams
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
        void $ submitIO ctx $ VU.replicate size $
            IOOpRead fd 0 mba 0 10
    closeIOCtx ctx

example_initEmptyClose :: Assertion
example_initEmptyClose = do
    ctx <- initIOCtx defaultIOCtxParams
    _ <- submitIO ctx VU.empty
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

{-------------------------------------------------------------------------------
  Valid IOCtxParams
-------------------------------------------------------------------------------}

-- | We test @validateIOCtxParams@ through 'withIOCtx'. If any params slip
-- through the cracks, then the call to @setupURing@ inside 'withIOCtx' should
-- throw an unexpected exception, which causes the property to fail.
prop_ValidIOCtxParams :: IOCtxParams -> Property
prop_ValidIOCtxParams params@IOCtxParams{..} =
    checkCoverage $
    coverTable "Result" [("Success", 5)] $
    ioProperty $ do
      eith <- try @IOException $ withIOCtx params $ \_ctx -> pure ()
      pure $ case eith of
        Left e
          |  "IOCtxParams are invalid" `isPrefixOf` ioe_location e
            &&
            not (
              inBoundsExcl ioctxBatchSizeLimit batchSizeLimitBoundsExcl &&
              inBoundsExcl ioctxConcurrencyLimit (concurrencyLimitBoundsExcl (ioctxBatchSizeLimit - 1))
            )
          -> tabulate "Result" [displayException e] True
          | otherwise
          -> counterexample ("Unknown exception: " ++ displayException e) False
        Right () -> tabulate "Result" ["Success"] True
  where
    inBoundsExcl x (lb, ub) = lb < x && x < ub

    batchSizeLimitBoundsExcl = (0, 2^(15 :: Int))
    concurrencyLimitBoundsExcl batchSizeLimit =  (batchSizeLimit, 2^(16::Int))

instance Arbitrary IOCtxParams where
  arbitrary = IOCtxParams <$> genLimit <*> genLimit
  shrink (IOCtxParams a b) =
      [ IOCtxParams a' b' | (a', b') <- liftShrink2 shrinkLimit shrinkLimit (a, b) ]

genLimit :: Gen Int
genLimit = frequency [
      -- Generate powers of 2 to hit the upper bounds on the batch size limit
      -- and concurrency limit
      (10, genPowerOf2)
      -- Get some coverage of the whole range between the lower and upper
      -- bounds on the batch size limit and concurrency limit.
    , (10, chooseNonNegativeInt)
      -- Otherwise, generate integers like normal
    , (10, arbitrary)
    ]

shrinkLimit :: Int -> [Int]
shrinkLimit x = shrinkPowerOf2 x ++ shrinkNonNegativeInt x ++ shrink x

genPowerOf2 :: Gen Int
genPowerOf2 = do
    i <- chooseInt (minExponent, maxExponent)
    pure (2^i)

shrinkPowerOf2 :: Int -> [Int]
shrinkPowerOf2 x = [x' | i <- [ minExponent .. maxExponent ], let x' = 2^i, x' < x]

minExponent, maxExponent :: Int
minExponent = 0
maxExponent = 20

chooseNonNegativeInt :: Gen Int
chooseNonNegativeInt = chooseInt (minValue, maxValue)

shrinkNonNegativeInt :: Int -> [Int]
shrinkNonNegativeInt x = [ x' | NonNegative x' <- shrink (NonNegative x) ]

minValue, maxValue :: Int
minValue = 0
maxValue = 2^maxExponent
