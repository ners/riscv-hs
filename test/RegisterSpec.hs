module RegisterSpec where

import Bit (Bit(..))
import Register
import FixedVectorSpec ()
import Test.Hspec
import Test.QuickCheck
import Prelude hiding ((+))

zeroRegister :: Register
zeroRegister = fromList []

testRegisterIdentity :: Register -> Expectation
testRegisterIdentity reg1 = reg1 + zeroRegister `shouldBe` (reg1, O)

testRegisterAdditionIsCommutative :: Register -> Register -> Expectation
testRegisterAdditionIsCommutative reg1 reg2 = reg1 + reg2 `shouldBe` reg2 + reg1

-- testRegisterAdditionIsAssociative :: Register -> Register -> Register -> Expectation
-- testRegisterAdditionIsAssociative reg1 reg2 reg3 = (reg1 + reg2) + reg3 `shouldBe` reg1 + (reg2 + reg3)

spec :: Spec
spec = do
  describe "Register (+)" $ do
    it "adding a Register of zeros doesn't change the value" $
      property testRegisterIdentity
    it "is commutative" $
      property testRegisterAdditionIsCommutative
    -- it "is associative" $
    --   property testRegisterAdditionIsAssociative

