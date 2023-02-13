{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FixedVectorSpec where

import Prelude hiding (not)

import Bit
import BitSpec ()
import FixedVector
import GHC.TypeLits
import Test.Hspec
import Test.QuickCheck

-- Induction base case: 0
instance Arbitrary t => Arbitrary (FixedVector t 0) where
  arbitrary = pure empty

-- Induction step: n -> n + 1
instance (Arbitrary t, KnownNat m, Arbitrary (FixedVector t m), n ~ (1 + m)) => Arbitrary (FixedVector t n) where
  arbitrary = prepend <$> arbitrary <*> arbitrary

testSingletonsAreEqual :: Bit -> Bit -> Expectation
testSingletonsAreEqual v w
  | v == w = singleton v `shouldBe` singleton w
  | otherwise = singleton v `shouldNotBe` singleton w

testPrependToEmpty :: Bit -> FixedVector Bit 0 -> Expectation
testPrependToEmpty v e = prepend v e `shouldBe` singleton v

testVectorIsEqualToItself :: KnownNat n => FixedVector Bit n -> Expectation
testVectorIsEqualToItself v = v `shouldBe` v

-- Could not figure out how to use arbitrary vector size without causing stack overflow
testConcatenation :: FixedVector Bit 32 -> FixedVector Bit 32 -> Expectation
testConcatenation a b = toList (a ++# b) `shouldBe` (toList a ++ toList b)

spec :: Spec
spec = do
  describe "Vector construction" $ do
    it "singleton vectors with equal elements are equal" $
      property testSingletonsAreEqual
  describe "Vector equality" $ do
    it "an empty vector is equal to itself" $ property $ testVectorIsEqualToItself @0
    it "a singleton vector is equal to itself" $ property $ testVectorIsEqualToItself @1
    it "a vector of size 2 is equal to itself" $ property $ testVectorIsEqualToItself @2
    it "a vector of size 4 is equal to itself" $ property $ testVectorIsEqualToItself @4
  describe "Prepend to empty" $ do
    it "prepending to an empty vector returns a singleton" $
      property testPrependToEmpty
  describe "Vector concatenation" $ do
    it "The elements of two concatenated vectors are the same as a concatenated list of the elements of both vectors" $
      property testConcatenation
