{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{- HLINT ignore "Use camelCase" -}

module Main (main) where

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (SomeException, try)
import           Control.Monad
import           Data.Proxy
import qualified Data.Vector.Storable       as VS
import           Data.Word                  (Word64)
import           System.IO                  (BufferMode (NoBuffering),
                                             hSetBuffering)
import           System.IO.BlockIO.URing
import qualified System.IO.BlockIO.URingFFI as FFI
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
    , testClassLaws "URingParams" $ storableLaws (Proxy @FFI.URingParams)
    ]

example_simpleNoop :: Word64 -> Assertion
example_simpleNoop n = do
    uring <- setupURing (URingParams 1)
    prepareNop uring (IOOpId n)
    submitIO uring
    completion <- awaitIO uring
    closeURing uring
    IOCompletion (IOOpId n) (IOResult 0) @=? completion

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
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (FFI.URingParams a b c d e f g h i j k l) = [
      FFI.URingParams a' b' c' d' e' f' g' h' i' j' k' l'
    | (a', b', c', d', e', f', g', h', i', j', k', l')
        <- shrink (a, b, c, d, e, f, g, h, i, j, k, l)
    ]

instance Arbitrary FFI.SQRingOffsets where
  arbitrary = FFI.SQRingOffsets
    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary
  shrink (FFI.SQRingOffsets a b c d e f g h i) = [
      FFI.SQRingOffsets a' b' c' d' e' f' g' h' i'
    | (a', b', c', d', e', f', g', h', i') <- shrink (a, b, c, d, e, f, g, h, i)
    ]

instance Arbitrary FFI.CQRingOffsets where
  arbitrary = FFI.CQRingOffsets
    <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary
  shrink (FFI.CQRingOffsets a b c d e f g h i) = [
      FFI.CQRingOffsets a' b' c' d' e' f' g' h' i'
    | (a', b', c', d', e', f', g', h', i') <- shrink (a, b, c, d, e, f, g, h, i)
    ]

instance ( Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         , Arbitrary f, Arbitrary g, Arbitrary h, Arbitrary i, Arbitrary j
         , Arbitrary k, Arbitrary l
         )
      => Arbitrary (a,b,c,d,e,f,g,h,i,j,k,l)
 where
  arbitrary = return (,,,,,,,,,,,)
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
          <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  shrink (o, p, q, r, s, t, u, v, w, x, y, z) =
    [ (o', p', q', r', s', t', u', v', w', x', y', z')
    | (o', (p', (q', (r', (s', (t', (u', (v', (w', (x', (y', z')))))))))))
      <- shrink (o, (p, (q, (r, (s, (t, (u, (v, (w, (x, (y, z))))))))))) ]
