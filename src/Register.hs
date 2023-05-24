{-# LANGUAGE DataKinds #-}

module Register where

import Bit (Bit (..))
import Bit qualified
import Adders
import FixedVector
    ( FixedVector (..)
    , fromListWithDefault
    , toList
    , zipWith
    , unzip
    )
import GHC.TypeLits
import Prelude hiding (unzip, zipWith)
import Data.Proxy (Proxy(Proxy))

type Register n = FixedVector n Bit

-- | A register of all Os
zeros :: KnownNat n => Register n
zeros = fromList []

ones :: KnownNat n => Register n
ones = fromListWithDefault I []

-- create a Register n from a list of Bits, padding missing elements with 0 and ignoring surplus elements
fromList :: KnownNat n => [Bit] -> Register n
fromList = fromListWithDefault O

not :: KnownNat n => Register n -> Register n
not = fmap Bit.not

and :: KnownNat n => Register n -> Register n -> Register n
and = FixedVector.zipWith Bit.and

or :: KnownNat n => Register n -> Register n -> Register n
or = FixedVector.zipWith Bit.or

xor :: KnownNat n => Register n -> Register n -> Register n
xor = FixedVector.zipWith Bit.xor

nand :: KnownNat n => Register n -> Register n -> Register n
nand = FixedVector.zipWith Bit.nand

nor :: KnownNat n => Register n -> Register n -> Register n
nor = FixedVector.zipWith Bit.nor

xnor :: KnownNat n => Register n -> Register n -> Register n
xnor = FixedVector.zipWith Bit.xnor

implies :: KnownNat n => Register n -> Register n -> Register n
implies = FixedVector.zipWith Bit.implies

isZero :: KnownNat n => Register n -> Bool
isZero = all (== O) . toList

shift :: forall n. KnownNat n => Int -> Register n -> Register n
shift k r
    | k < 0 = fromListWithDefault O $ drop (abs k) (toList r)
    | k > 0 = fromListWithDefault O $ replicate k O ++ take l (toList r)
    | otherwise = r
    where
        n = fromIntegral $ natVal (Proxy @n)
        l = n - abs k

add :: forall n. KnownNat n => Register n -> Register n -> (Register n, Bit)
add a b
    | isZero a = (b, O)
    | isZero b = (a, O)
    | otherwise = add values (shift (-1) carries)
    where
      (values, carries) = unzip $ zipWith halfAdder a b
