module Main (main) where

import Prelude hiding (and, not)
import Bits
import Control.Monad

main :: IO ()
main = do
   unless (nor I I == O) (fail "nor test failed")
   unless (and I I == I) (fail "and test failed")
   unless (and O I == O) (fail "and test failed")
   unless (not O == I) (fail "not test failed")
   unless (not I == O) (fail "not test failed")
