{-# LANGUAGE DataKinds #-}

module RegisterSpec where

import Bit
import Control.Monad
import FixedVector
import FixedVectorSpec ()
import GHC.TypeLits
import Register
import Test.Hspec
import Test.QuickCheck
import Prelude hiding ((+))

notChangesEachBit :: KnownNat n => Register n -> Expectation
notChangesEachBit r = zipWithM_ shouldNotBe (toList r) (toList $ Register.not r)

notIsItsOwnInverse :: KnownNat n => Register n -> Expectation
notIsItsOwnInverse r = Register.not (Register.not r) `shouldBe` r

allZeros :: KnownNat n => Register n
allZeros = fromList [O, O ..]

allOnes :: KnownNat n => Register n
allOnes = fromList [I, I ..]

testAnd :: KnownNat n => Register n -> Expectation
testAnd r = do
    Register.and r r `shouldBe` r
    Register.and r (Register.not r) `shouldBe` allZeros
    Register.and r allZeros `shouldBe` allZeros

testOr :: KnownNat n => Register n -> Expectation
testOr r = do
    Register.or r r `shouldBe` r
    Register.or r (Register.not r) `shouldBe` allOnes

testOrNotZero :: KnownNat n => Register n -> Expectation
testOrNotZero r
    | r == allZeros = Register.or r allZeros `shouldBe` allZeros
    | otherwise = Register.or r allZeros `shouldNotBe` allZeros

testNand :: KnownNat n => Register n -> Register n -> Expectation
testNand r1 r2 = Register.nand r1 r2 `shouldBe` (Register.not $ Register.and r1 r2)

testNor :: KnownNat n => Register n -> Register n -> Expectation
testNor r1 r2 = Register.nor r1 r2 `shouldBe` (Register.not $ Register.or r1 r2)

testXor :: KnownNat n => Register n -> Expectation
testXor r = do
    Register.xor r (Register.not r) `shouldBe` allOnes
    Register.xor r r `shouldBe` allZeros

testXnor :: KnownNat n => Register n -> Register n -> Expectation
testXnor r1 r2 = Register.xnor r1 r2 `shouldBe` (Register.not $ Register.xor r1 r2)

impliesAllZeros :: KnownNat n => Register n -> Register n -> Expectation
impliesAllZeros r1 r2
    | r2 == allZeros = Register.implies allOnes r2 `shouldBe` allZeros
    | otherwise = Register.implies r1 r2 `shouldNotBe` allZeros

impliesAllOnes :: KnownNat n => Register n -> Expectation
impliesAllOnes r = Register.implies r allOnes `shouldBe` allOnes

spec :: Spec
spec = do
    describe "not" $ do
        it "changes each Bit" $ property $ notChangesEachBit @4
        it "is its own inverse" $ property $ notIsItsOwnInverse @4
    describe "and" $
        it "r and r = r; r and (not r) = zero, r and zero = zero" $
            property $
                testAnd @4
    describe "or" $ do
        it "r or r == r; r or not r == all ones" $ property $ testOr @4
        it "is only zero if both inputs are zero" $ property $ testOrNotZero @4
    describe "nand" $
        it "is the opposite of and" $
            property $
                testNand @4
    describe "nor" $
        it "is the opposite of or" $
            property $
                testNor @4
    describe "xor" $
        it "r xor r = zero, r xor not r = all ones" $
            property $
                testXor @4
    describe "xnor" $
        it "is the opposite of xor" $
            property $
                testXnor @4
    describe "implies" $ do
        it "only returns zero if the second input is zero" $ property $ impliesAllZeros @4
        it "returns all ones if the second input is all ones" $ property $ impliesAllOnes @4
