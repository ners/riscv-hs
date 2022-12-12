module Main (main) where

import Adders
import Bits
import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (and, not, or)

instance Arbitrary Bit where
  arbitrary = elements [I, O]

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

comHalfAdder :: Bit -> Bit -> Expectation
comHalfAdder a b = halfAdder a b `shouldBe` halfAdder b a

idHalfAdder :: Bit -> Bit -> Expectation
idHalfAdder a b
  | b == O = halfAdder a b `shouldBe` (a, b)
  | otherwise = halfAdder a b `shouldNotBe` (a, b)

comFullAdder :: Bit -> Bit -> Bit -> Expectation
comFullAdder a b c = fullAdder a b c `shouldBe` fullAdder c b a

carryFullAdder :: Bit -> Bit -> Bit -> Expectation
carryFullAdder a b c
  | c == O = fullAdder a b c `shouldBe` halfAdder a b
  | otherwise = fullAdder a b c `shouldNotBe` halfAdder a b

main :: IO ()
main = do
  hspec $ do
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
    describe "Binary half adder" $ do
      it "changing the order of inputs does not change the result" $
        property comHalfAdder
      it "if O is added the result is identical to the input" $
        property idHalfAdder
    describe "Binary full adder" $ do
      it "changing the order of inputs does not affect the result" $
        property comFullAdder
      it "if the carry bit is O it behaves like a half adder" $
        property carryFullAdder
