{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}

module FixedVector.Type where

import GHC.TypeLits

newtype KnownNat size => FixedVector t size = FixedVector
  { _data :: [t]
  } deriving (Show)

empty :: FixedVector t 0
empty = FixedVector { _data = [] }

singleton :: t -> FixedVector t 1
singleton v = FixedVector { _data = [v] }

prepend :: KnownNat n => t -> FixedVector t n -> FixedVector t (1 + n)
prepend a b = singleton a ++# b

(++#)
  :: KnownNat m
  => KnownNat n
  => FixedVector t m
  -> FixedVector t n
  -> FixedVector t (m+n)
(++#) a b = FixedVector { _data = _data a ++ _data b }
