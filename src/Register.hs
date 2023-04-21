{-# LANGUAGE DataKinds #-}

module Register where
import FixedVector
import Bit (Bit(..))
import Adders (fullAdder)
import Prelude hiding (sum)

type Register = FixedVector 32 Bit

-- create a Register from a list of Bits, padding missing elements with 0 and ignoring surplus elements
fromList :: [Bit] -> Register
fromList = fromListWithDefault O

(+) :: Register -> Register -> (Register, Bit)
r1 + r2 = (FixedVector{ elements = sum }, overflow)
  where (overflow:sum) = reverse $ addBits O (toList r1) (toList r2)

addBits :: Bit -> [Bit] -> [Bit] -> [Bit]
addBits c [] [] = [c]
addBits c as [] = addBits O as [c]
addBits c [] bs = addBits O [c] bs
addBits c (a:as) (b:bs) = sum : addBits carry as bs
  where
    (sum,carry) = fullAdder a b c
