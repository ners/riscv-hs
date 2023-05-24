{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module FixedVector
    ( FixedVector (..)
    , empty
    , toList
    , tryFromList
    , fromNumList
    , fromListWithDefault
    , unsafeFromList
    , singleton
    , prepend
    , (++#)
    , FixedVector.zipWith
    , FixedVector.unzip
    , FixedVector.zip
    )
where

import Data.Data (Proxy (Proxy))
import Data.Maybe (fromMaybe)
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

-- | retrieve the contents of a FixedVector as a list
toList :: FixedVector n a -> [a]
toList = elements

-- | create a FixedVector from a list, if the length of the list matches the desired length of FixedVector.
tryFromList :: forall a n. (KnownNat n) => [a] -> Maybe (FixedVector n a)
tryFromList xs
    | length xs == n = Just FixedVector{elements = xs}
    | otherwise = Nothing
  where
    n = fromIntegral $ natVal (Proxy @n)

unsafeFromList :: forall a n. (KnownNat n) => [a] -> FixedVector n a
unsafeFromList = fromMaybe (error "unsafeFromList: wrong length of list given") . tryFromList

{- | create a FixedVector from a list,
 | padding missing elements with the given default value, and ignoring surplus elements
-}
fromListWithDefault :: forall a n. KnownNat n => a -> [a] -> FixedVector n a
fromListWithDefault x xs = FixedVector{elements = take n (xs <> repeat x)}
  where
    n = fromIntegral $ natVal (Proxy @n)

{- | create a FixedVector from a list of numeric values,
 | padding missing elements with zeros, and ignoring surplus elements
-}
fromNumList :: (Num a, KnownNat n) => [a] -> FixedVector n a
fromNumList = fromListWithDefault 0

{- | create a FixedVector from a list of monoid values,
 | padding missing elements with the identity element, and ignoring surplus elements
-}
fromMonoidList :: (Monoid a, KnownNat n) => [a] -> FixedVector n a
fromMonoidList = fromListWithDefault mempty

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

zipWith :: KnownNat n => (a -> b -> c) -> FixedVector n a -> FixedVector n b -> FixedVector n c
zipWith f as bs = unsafeFromList $ Prelude.zipWith f (toList as) (toList bs)

zip :: KnownNat n => FixedVector n a -> FixedVector n b -> FixedVector n (a, b)
zip = FixedVector.zipWith (,)

unzip :: KnownNat n => FixedVector n (a, b) -> (FixedVector n a, FixedVector n b)
unzip r = (fst <$> r, snd <$> r)

rotate :: KnownNat n => Int -> FixedVector n a -> FixedVector n a
rotate = error "TODO(gennadi): implement rotate"
