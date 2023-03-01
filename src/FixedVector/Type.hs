{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: hide the constructor of FixedVector
module FixedVector.Type where

import GHC.TypeLits
import GHC.Generics (Generic)

-- |A FixedVector is a list with a type-level size parameter.
newtype KnownNat size => FixedVector size t = FixedVector
  { elements :: [t]
  }
  deriving newtype (Generic, Show, Eq)

toList :: FixedVector size t -> [t]
toList = elements

instance Functor (FixedVector n) where
  f `fmap` v = FixedVector { elements = f `fmap` elements v }

-- TODO fricklerhandwerk
--instance Foldable (FixedVector n)

-- TODO gennadi
--instance Traversable (FixedVector n)

-- TODO can we do Applicative?
-- for Applicative we would need `pure`, `<*>`

-- |empty creates a FixedVector of 0 elements
empty :: FixedVector 0 t
empty = FixedVector { elements = [] }

-- |singleton creates a FixedVector with a single element
singleton :: t -> FixedVector 1 t
singleton v = FixedVector { elements = [v] }

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
(++#) a b = FixedVector { elements = elements a <> elements b }
