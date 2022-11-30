{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (IO, print, (==))

import Bits

main :: IO ()
main = do
  print (nor I I == O) -- should print true
