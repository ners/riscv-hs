{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (IO, print)

import Bits

main :: IO ()
main = print (nor I I)
