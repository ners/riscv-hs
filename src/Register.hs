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
and x y = fromList (fmap (uncurry Bit.and) (zip (toList x) (toList y)))

-- or :: Register -> Register -> Register
--
-- xor :: Register -> Register -> Register
--
-- nand :: Register -> Register -> Register
--
-- nor :: Register -> Register -> Register
--
-- xnor :: Register -> Register -> Register
--
-- implies :: Register -> Register -> Register
