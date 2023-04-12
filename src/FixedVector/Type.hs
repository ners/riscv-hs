{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module FixedVector.Type
    ( FixedVector
    , empty
    , toList
    , singleton
    , prepend
    , (++#)
    )
where

import Data.Data (Proxy (Proxy))
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

-- | create a FixedVector from a list, if the length of the list matches the desired length of FixedVector.
tryFromList :: forall a n. (KnownNat n) => [a] -> Maybe (FixedVector n a)
tryFromList xs
  | length xs == n = Just FixedVector{ elements = xs }
  | otherwise = Nothing
  where
    n = fromIntegral $ natVal (Proxy @n)

fromNumList :: forall a n. (Num a, KnownNat n) => [a] -> FixedVector n a
fromNumList xs = FixedVector{elements = take n (xs <> repeat 0)}
  where
    n = fromIntegral $ natVal (Proxy @n)

-- fromSemigroupList :: Semigroup a => [a] -> FixedVector n a

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
