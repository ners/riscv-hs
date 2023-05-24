{-# LANGUAGE DataKinds #-}

module RegisterSpec where

import Bit
import FixedVector (toList)
import GHC.TypeNats (KnownNat)
import Register
import Test.Hspec
import Data.Foldable (forM_)

spec :: Spec
spec = do
    describe "register2num" $ do
        it "respects the bit order" $ forM_ [0..3] $ \i ->
            register2num @4 @Int (fromList $ replicate i O <> [I]) `shouldBe` 2 ^ i

-- TODO(jonas): test add
bit2num :: Num a => Bit -> a
bit2num O = 0
bit2num I = 1

register2num :: (KnownNat n, Num a) => Register n -> a
register2num = foldr (\bit acc -> acc * 2 + bit2num bit) 0 . toList
