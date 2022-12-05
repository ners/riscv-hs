module Main (main) where

import Bits
import Control.Monad
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (and, not)

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

testImplies :: Bit -> Bit -> Expectation
testImplies a b
  | a == I = implies a b `shouldBe` b
  | otherwise = implies a b `shouldBe` I

main :: IO ()
main = do
  hspec $ do
    describe "Unary NOT function" $ do
      it "should be identity when applied twice" $
        property testNotIdentity
      it "should not have the same output as the input" $
        property testNotDifference
    describe "Binary AND function" $ do
      it "returns the input value if both are the same" $
        property testAnd
    describe "Binary IMPLIES function" $ do
      it "returns I unless input values are I O in that order" $
        property testImplies
  unless (nor I I == O) (fail "nor test failed")
