{-# LANGUAGE DataKinds #-}

module Register where

import Bit (Bit (..))
import Bit qualified
import FixedVector
import Prelude hiding (sum)

type Register = FixedVector 32 Bit

-- create a Register from a list of Bits, padding missing elements with 0 and ignoring surplus elements
fromList :: [Bit] -> Register
fromList = fromListWithDefault O

not :: Register -> Register
not = fmap Bit.not

and :: Register -> Register -> Register
and x y = fromList (uncurry Bit.and `fmap` zip (toList x) (toList y))

or :: Register -> Register -> Register
or x y = fromList (uncurry Bit.or `fmap` zip (toList x) (toList y))

xor :: Register -> Register -> Register
xor x y = fromList (uncurry Bit.xor `fmap` zip (toList x) (toList y))

nand :: Register -> Register -> Register
nand x y = fromList (uncurry Bit.nand `fmap` zip (toList x) (toList y))

nor :: Register -> Register -> Register
nor x y = fromList (uncurry Bit.nor `fmap` zip (toList x) (toList y))

xnor :: Register -> Register -> Register
xnor x y = fromList (uncurry Bit.xnor `fmap` zip (toList x) (toList y))

implies :: Register -> Register -> Register
implies x y = fromList (uncurry Bit.implies `fmap` zip (toList x) (toList y))
