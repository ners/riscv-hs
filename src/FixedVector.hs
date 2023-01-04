{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module FixedVector where

import Prelude (Show, (++), ($), Int, pure, (<$>), error, show, Integral, fromIntegral)

import GHC.TypeLits
import Language.Haskell.TH.Lib
import Language.Haskell.TH

newtype KnownNat size => FixedVector t size = FixedVector
  { _data :: [t]
  } deriving (Show)

empty :: FixedVector t 0
empty = FixedVector { _data = [] }

singleton :: t -> FixedVector t 1
singleton v = FixedVector { _data = [v] }

(++#) :: (KnownNat m, KnownNat n) => FixedVector t m -> FixedVector t n -> FixedVector t (m+n)
(++#) a b = FixedVector { _data = _data a ++ _data b }

prepend :: KnownNat n => t -> FixedVector t n -> FixedVector t (1+n)
prepend a b = singleton a ++# b

createVectorN :: (Integral n, Show n) => n -> DecsQ
createVectorN n = pure
  [ SigD functionName type'
  , FunD functionName clauses
  ]
  where
    -- here we have access to n from `createVector``
    -- the name of the function we are creating
    functionName :: Name
    functionName = mkName $ "createVector" ++ show n

    -- The type of elements stored in our vector, which we denote as `t`
    vectorElementType :: Type
    vectorElementType = VarT $ mkName "t"

    -- ''FixedVector is the Name of the concrete type FixedVector
    -- t -> t -> ... -> t -> t -> FixedVector t n
    returnType :: Type
    returnType = ConT ''FixedVector `AppT` vectorElementType `AppT` LitT (NumTyLit $ fromIntegral n)
    -- alternatively, equivalent:
    -- returnType = AppT (AppT (ConT ''FixedVector) vectorElementType) (LitT $ NumTyLit n)
    -- returnType = foldr1 (AppT) [ConT ''FixedVector, vectorElementType, LitT (NumTyLit n)]

    type' :: Type
    type' = error "Not implemented yet"

    clauses :: [Clause]
    clauses = error "Not implemented yet"


createVector0 :: FixedVector t 0
createVector0 = empty

createVector1 :: t -> FixedVector t 1
createVector1 = singleton

createVector2 :: t -> t -> FixedVector t 2
createVector2 v1 v2 = FixedVector { _data = [v1, v2] }

createVector3 :: t -> t -> t -> FixedVector t 3
createVector3 v1 v2 v3 = prepend v1 $ createVector2 v2 v3

createVector4 :: t -> t -> t -> t -> FixedVector t 4
createVector4 v1 v2 v3 v4 = prepend v1 $ createVector3 v2 v3 v4

-- dec SIGNATURE: NAME TYPE(t -> t -> t -> t -> FixedVector t 4)
-- dec FUNCTION:  NAME PARAMETERS(v1 v2 v3 v4) = BODY(prepend v1 $ createVector3 v2 v3 v4)
