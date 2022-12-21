{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module FixedVector where

import Prelude (Show, (++), ($), Int, pure, (<$>))

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

createVector :: Int -> DecsQ
createVector n = pure
  [ FunD name [ Clause [ConP name [] (VarP v <$> [1..n])] (NormalB )]
  ]
  where
    name = mkName $ "createVector" ++ show n
    v n = mkName $ "v" ++ show n

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
