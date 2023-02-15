{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}

module FixedVector.Type where

import GHC.TypeLits

-- TODO thies: write some Haddock documentation for these files / datatypes / functions
-- it's a special format for writing documentation in code
-- TODO gennadi: implement Functor
-- |A FixedVector is a list of size elements, stored as _data
newtype KnownNat size => FixedVector size t = FixedVector
  { _data :: [t]
  } deriving (Show, Eq)

-- TODO: fromList? IsList?
-- |toList creates a list with the same elements as a given FixedVector
toList :: KnownNat n => FixedVector n t -> [t]
toList = _data

-- |empty creates a FixedVector of 0 elements
empty :: FixedVector 0 t
empty = FixedVector { _data = [] }

-- |singleton creates a FixedVector with a single element
singleton :: t -> FixedVector 1 t
singleton v = FixedVector { _data = [v] }

-- |prepend a b adds a as an element at the head of b and increases the size of the vector by 1.
prepend :: KnownNat n => t -> FixedVector n t -> FixedVector (1 + n) t
prepend a b = singleton a ++# b

-- |(++#) a b creates a FixedVector of length m+n where m and n are the lengths of the inputs.
-- The elements of the new vector are the elements of a followed by the elements of b
(++#)
  :: KnownNat m
  => KnownNat n
  => FixedVector m t
  -> FixedVector n t
  -> FixedVector (m+n) t
(++#) a b = FixedVector { _data = _data a ++ _data b }
