{-# LANGUAGE NoImplicitPrelude #-}

module Adders where

import Bit

-- Add two binary digits and output the least significant digit of the sum, and the carry-over value
-- Examples:
--    add O O
--    => (O, O) -- binary 00
--    add O I
--    => (I, O) -- binary 01
--    add I O
--    => (I, O) -- binary 01
--    add I I
--    => (O, I) -- binary 10
halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder a b =
  let sum = xor a b
      carry = and a b
   in (sum, carry)

-- Add two binary digits and a carry-over value, output the least significant bit of the sum and the carry-over value from the addition
-- Examples:
--    add I I I
--    => (I, I)
fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder a b carryIn =
  let (sum1, carryOut1) = halfAdder a b
      (sum2, carryOut2) = halfAdder carryIn sum1
   in (sum2, or carryOut1 carryOut2)

