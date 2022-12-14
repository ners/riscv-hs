{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module FixedVector where

import Prelude ((++))

import GHC.TypeLits

data FixedVector t (size :: Nat) = FixedVector
  { _data :: [t]
  }

empty :: FixedVector t 0
empty = FixedVector { _data = [] }

singleton :: t -> FixedVector t 1
singleton v = FixedVector { _data = [v] }

(++#) :: FixedVector t m -> FixedVector t n -> FixedVector t (m+n)
(++#) a b = FixedVector { _data = _data a ++ _data b }

