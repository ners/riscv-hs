{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: hide the constructor of FixedVector
module FixedVector.Type where

import GHC.Generics (Generic)
import GHC.TypeLits

{- |A FixedVector is a list with a type-level size parameter.
 We cannot derive Applicative or Monad because that would not respect the size parameter.
-}
newtype FixedVector size t = FixedVector
    { elements :: [t]
    }
    deriving newtype (Generic, Show, Eq, Foldable, Functor)

-- |empty creates a FixedVector of 0 elements
empty :: FixedVector 0 t
empty = FixedVector{elements = []}

toList :: FixedVector n a -> [a]
toList = elements

-- |singleton creates a FixedVector with a single element
singleton :: t -> FixedVector 1 t
singleton v = FixedVector{elements = [v]}

-- |prepend a b adds a as an element at the head of b and increases the size of the vector by 1.
prepend :: KnownNat n => t -> FixedVector n t -> FixedVector (1 + n) t
prepend a b = singleton a ++# b

{- |(++#) a b creates a FixedVector of length m+n where m and n are the lengths of the inputs.
 The elements of the new vector are the elements of a followed by the elements of b
-}
(++#)
    :: KnownNat m
    => KnownNat n
    => FixedVector m t
    -> FixedVector n t
    -> FixedVector (m + n) t
(++#) a b = FixedVector{elements = elements a <> elements b}
