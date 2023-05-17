{-# LANGUAGE DataKinds #-}

module RegisterSpec where

import Register
import Test.Hspec
import Test.QuickCheck
import Bit
import FixedVector
import FixedVectorSpec ()
import GHC.TypeLits
import Prelude hiding ((+))
import Control.Monad

notChangesEachBit :: KnownNat n => Register n -> Expectation
notChangesEachBit r = zipWithM_ shouldNotBe (toList r) (toList $ Register.not r)

notIsItsOwnInverse :: KnownNat n => Register n -> Expectation
notIsItsOwnInverse r = Register.not (Register.not r) `shouldBe` r

allZeros = fromList [O, O ..]
allOnes = fromList [I, I ..]

testAnd :: KnownNat n => Register n -> Expectation
testAnd r = do
    Register.and r r `shouldBe` r
    Register.and r (Register.not r) `shouldBe` fromList [O, O ..]
    -- TODO(thies): and with allZeros and allOnes, implement other tests

testOr :: KnownNat n => Register n -> Expectation
testOr r = do
    Register.or r r `shouldBe` r
    Register.or r (Register.not r) `shouldBe` fromList [I, I ..]

spec :: Spec
spec = do
  describe "not" $ do
    it "changes each Bit" $ property $ notChangesEachBit @4
    it "is its own inverse" $ property $ notIsItsOwnInverse @4
  describe "and" $ undefined
  describe "or" $ undefined
