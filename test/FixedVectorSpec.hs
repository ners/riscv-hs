module FixedVectorSpec where

import Prelude hiding (not)

import Bit
import BitSpec ()
import FixedVector
import Test.Hspec
import Test.QuickCheck

-- TODO: instance Arbitrary Vector

testSingletonsAreEqual :: Bit -> Bit -> Expectation
testSingletonsAreEqual v w
  | v == w = singleton v `shouldBe` singleton w
  | otherwise = singleton v `shouldNotBe` singleton w

spec :: Spec
spec = do
  describe "Vector construction" $ do
    it "empty vectors should be equal" $
      empty @Bit `shouldBe` empty @Bit
    it "singleton vectors with equal elements are equal" $
      property testSingletonsAreEqual

-- TODO: test that prepending to empty returns singleton
-- TODO: test appending
