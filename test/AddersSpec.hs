module AddersSpec where

import Adders
import Bit
import BitSpec ()
import Control.Monad
import Prelude hiding (and, not, or)
import Test.Hspec
import Test.QuickCheck

testHalfAdderCommutative :: Bit -> Bit -> Expectation
testHalfAdderCommutative a b = halfAdder a b `shouldBe` halfAdder b a

testHalfAdderIdentity :: Bit -> Bit -> Expectation
testHalfAdderIdentity a b
  | b == O = halfAdder a b `shouldBe` (a, b)
  | otherwise = halfAdder a b `shouldNotBe` (a, b)

testFullAdderCommutative :: Bit -> Bit -> Bit -> Expectation
testFullAdderCommutative a b c = fullAdder a b c `shouldBe` fullAdder c a b

testFullAdderCarry :: Bit -> Bit -> Bit -> Expectation
testFullAdderCarry a b c
  | c == O = fullAdder a b c `shouldBe` halfAdder a b
  | otherwise = fullAdder a b c `shouldNotBe` halfAdder a b

spec :: Spec
spec = do
    describe "Binary half adder" $ do
      it "changing the order of inputs does not change the result" $
        property testHalfAdderCommutative
      it "if O is added the result is identical to the input" $
        property testHalfAdderIdentity
    describe "Binary full adder" $ do
      it "changing the order of inputs does not affect the result" $
        property testFullAdderCommutative
      it "if the carry bit is O it behaves like a half adder" $
        property testFullAdderCarry
