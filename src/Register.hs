{-# LANGUAGE DataKinds #-}

module Register where

import Bit (Bit (..))
import FixedVector
import Prelude hiding (sum)

type Register = FixedVector 32 Bit

-- create a Register from a list of Bits, padding missing elements with 0 and ignoring surplus elements
fromList :: [Bit] -> Register
fromList = fromListWithDefault O
