{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Bit
import Prelude (IO, print, (==))

main :: IO ()
main = do
    print (nor I I == O) -- should print true
