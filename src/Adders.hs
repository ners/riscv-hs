{-# LANGUAGE NoImplicitPrelude #-}

module Adders where

import Bit

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder a b =
  let sum = xor a b
      carryOut = and a b
   in (sum, carryOut)

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b carryIn =
  let (sum1, carryOut1) = halfAdder a b
      (sum2, carryOut2) = halfAdder carryIn sum1
   in (sum2, or carryOut1 carryOut2)
