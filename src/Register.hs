{-# LANGUAGE DataKinds #-}

module Register where

import Bit (Bit (..))
import Bit qualified
import FixedVector
    ( FixedVector (..)
    , fromListWithDefault
    )
import FixedVector qualified

type Register = FixedVector 32 Bit

-- create a Register from a list of Bits, padding missing elements with 0 and ignoring surplus elements
fromList :: [Bit] -> Register
fromList = fromListWithDefault O

not :: Register -> Register
not = fmap Bit.not

and :: Register -> Register -> Register
and = FixedVector.zipWith Bit.and

or :: Register -> Register -> Register
or = FixedVector.zipWith Bit.or

xor :: Register -> Register -> Register
xor = FixedVector.zipWith Bit.xor

nand :: Register -> Register -> Register
nand = FixedVector.zipWith Bit.nand

nor :: Register -> Register -> Register
nor = FixedVector.zipWith Bit.nor

xnor :: Register -> Register -> Register
xnor = FixedVector.zipWith Bit.xnor

implies :: Register -> Register -> Register
implies = FixedVector.zipWith Bit.implies
