{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}

module FixedVector.Type where

import GHC.TypeLits

-- TODO thies: write some Haddock documentation for these files / datatypes / functions
-- it's a special format for writing documentation in code
-- TODO gennadi: implement Functor
newtype KnownNat size => FixedVector t size = FixedVector
  { _data :: [t]
  } deriving (Show, Eq)

-- TODO: fromList? IsList?

toList :: KnownNat n => FixedVector t n -> [t]
toList = _data

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
