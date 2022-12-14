{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (IO, print, (==))

import Bit

main :: IO ()
main = print (nor I I == O) -- should print true
