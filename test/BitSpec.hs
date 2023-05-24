{-# OPTIONS_GHC -Wno-orphans #-}

module BitSpec where

import Bit
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (and, not, or)

instance Arbitrary Bit where
    arbitrary = elements [O, I]

testNotIdentity :: Bit -> Expectation
testNotIdentity x = not (not x) `shouldBe` x

testNotDifference :: Bit -> Expectation
testNotDifference x = not x `shouldNotBe` x

testAnd :: Bit -> Bit -> Expectation
testAnd a b
    | a == b = and a b `shouldBe` a
    | otherwise = and a b `shouldBe` O

testOr :: Bit -> Bit -> Expectation
testOr a b
    | a == I = or a b `shouldBe` I
    | b == I = or a b `shouldBe` I
    | otherwise = or a b `shouldBe` O

testXor :: Bit -> Bit -> Expectation
testXor a b
    | a == b = xor a b `shouldBe` O
    | otherwise = xor a b `shouldBe` I

testNand :: Bit -> Bit -> Expectation
testNand a b
    | a == O = nand a b `shouldBe` I
    | b == O = nand a b `shouldBe` I
    | otherwise = nand a b `shouldBe` O

testNor :: Bit -> Bit -> Expectation
testNor a b
    | a == I = nor a b `shouldBe` O
    | b == I = nor a b `shouldBe` O
    | otherwise = nor a b `shouldBe` I

testXnor :: Bit -> Bit -> Expectation
testXnor a b
    | a == b = xnor a b `shouldBe` I
    | otherwise = xnor a b `shouldBe` O

testImplies :: Bit -> Bit -> Expectation
testImplies a b
    | a == I = implies a b `shouldBe` b
    | otherwise = implies a b `shouldBe` I

spec :: Spec
spec = do
    describe "Unary NOT function" $ do
        it "should be identity when applied twice" $
            property testNotIdentity
        it "should not have the same output as the input" $
            property testNotDifference
    describe "Binary AND function" $ do
        it "returns 'I' if both inputs are 'I'" $
            property testAnd
    describe "Binary OR function" $ do
        it "returns 'I' if at least one input is 'I'" $
            property testOr
    describe "Binary XOR function" $ do
        it "returns 'O' if input values are the same" $
            property testXor
    describe "Binary NAND function" $ do
        it "returns 'O' if both inputs are 'I'" $
            property testNand
    describe "Binary NOR function" $ do
        it "returns 'I' if both inputs are 'O'" $
            property testNor
    describe "Binary XNOR function" $ do
        it "returns 'I' if both inputs are the same" $
            property testXnor
    describe "Binary IMPLIES function" $ do
        it "returns I unless input values are I O in that order" $
            property testImplies
