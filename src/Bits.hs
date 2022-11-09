module Bits where

data Bit = O | I

not :: Bit -> Bit
not O = I
not I = O
