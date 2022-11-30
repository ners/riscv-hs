{-# LANGUAGE NoImplicitPrelude #-}

module Bits where

import Prelude (Eq, Show, otherwise, (==))

data Bit = O | I deriving (Eq, Show)

not :: Bit -> Bit
not O = I
not I = O

and :: Bit -> Bit -> Bit
and I I = I
and _ _ = O

or :: Bit -> Bit -> Bit
or O O = O
or _ _ = I

xor :: Bit -> Bit -> Bit
xor x y
  | x == y = O
  | otherwise = I

nand :: Bit -> Bit -> Bit
nand x y = not (and x y)

nor :: Bit -> Bit -> Bit
nor x y = not (or x y)

xnor :: Bit -> Bit -> Bit
xnor x y = not (xor x y)
