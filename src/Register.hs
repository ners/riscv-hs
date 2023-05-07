{-# LANGUAGE DataKinds #-}

module Register where

import Bit (Bit (..))
import Bit qualified
import FixedVector (FixedVector (..)
                    , fromListWithDefault)
import FixedVector qualified
import Prelude hiding (zipWith)

type Register = FixedVector 32 Bit

-- create a Register from a list of Bits, padding missing elements with 0 and ignoring surplus elements
fromList :: [Bit] -> Register
fromList = fromListWithDefault O

zipWith :: (Bit -> Bit -> Bit) -> (Register -> Register -> Register)
zipWith f = FixedVector.zipWith f O

not :: Register -> Register
not = fmap Bit.not

and :: Register -> Register -> Register
and = zipWith Bit.and

or :: Register -> Register -> Register
or = zipWith Bit.or

xor :: Register -> Register -> Register
xor = zipWith Bit.xor

nand :: Register -> Register -> Register
nand = zipWith Bit.nand

nor :: Register -> Register -> Register
nor = zipWith Bit.nor

xnor :: Register -> Register -> Register
xnor = zipWith Bit.xnor

implies :: Register -> Register -> Register
implies = zipWith Bit.implies
