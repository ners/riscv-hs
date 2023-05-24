{-# LANGUAGE DataKinds #-}

module Register where

import Bit (Bit (..))
import Bit qualified
import FixedVector
    ( FixedVector (..)
    , fromListWithDefault
    )
import FixedVector qualified
import GHC.TypeLits

type Register n = FixedVector n Bit

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
