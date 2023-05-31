{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module FixedVectorSpec where

import Bit
import BitSpec ()
import Control.Monad (when)
import Data.Proxy (Proxy (..))
import FixedVector
import GHC.TypeLits
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (not)

-- Induction base case: 0
instance Arbitrary t => Arbitrary (FixedVector 0 t) where
    arbitrary = pure empty

-- Induction step: n -> n + 1
instance
    ( Arbitrary t
    , KnownNat m
    , Arbitrary (FixedVector m t)
    , n ~ (1 + m)
    )
    => Arbitrary (FixedVector n t)
    where
    arbitrary = prepend <$> arbitrary <*> arbitrary

testSingletonsAreEqual :: (Arbitrary t, Eq t, Show t) => t -> t -> Expectation
testSingletonsAreEqual v w
    | v == w = singleton v `shouldBe` singleton w
    | otherwise = singleton v `shouldNotBe` singleton w

testEmptyIsConcatenationUnit :: (Arbitrary t, Eq t, Show t) => t -> FixedVector 0 t -> Expectation
testEmptyIsConcatenationUnit v e = do
    prepend v e `shouldBe` singleton v
    singleton v ++# e `shouldBe` singleton v
    e ++# singleton v `shouldBe` singleton v

testConcatenation
    :: ( KnownNat n
       , KnownNat m
       , KnownNat (n + m)
       , Arbitrary t
       , Eq t
       , Show t
       )
    => FixedVector n t
    -> FixedVector m t
    -> Expectation
testConcatenation a b = toList (a ++# b) `shouldBe` (toList a ++ toList b)

testRotate :: forall n t. (KnownNat n, Arbitrary t, Eq t, Show t) => Int -> FixedVector n t -> Expectation
testRotate k v = do
    when (n /= 0 && k `mod` n == 0) $ rotate k v `shouldBe` v
    when (n == 0) $ rotate k v `shouldBe` v
    rotate (-k) (rotate k v) `shouldBe` v
  where
    n = fromIntegral $ natVal (Proxy @n)

spec :: Spec
spec = do
    describe "construction" $ do
        it "preserves equality of singletons" $ property $ testSingletonsAreEqual @Bit
    describe "concatenation" $ do
        it "has unit" $ property $ testEmptyIsConcatenationUnit @Bit
    describe "concatenation" $ do
        it "preserves structure" $ property $ testConcatenation @0 @0 @Bit
        it "preserves structure" $ property $ testConcatenation @1 @1 @Bit
        it "preserves structure" $ property $ testConcatenation @2 @2 @Bit
        it "preserves structure" $ property $ testConcatenation @4 @2 @Bit
        it "preserves structure" $ property $ testConcatenation @2 @4 @Bit
    describe "rotation" $ do
        it "is cyclic and symmetric" $ property $ testRotate @0 @Bit
        it "is cyclic and symmetric" $ property $ testRotate @1 @Bit
        it "is cyclic and symmetric" $ property $ testRotate @2 @Bit
        it "is cyclic and symmetric" $ property $ testRotate @4 @Bit
