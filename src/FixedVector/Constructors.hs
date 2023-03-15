{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : FixedVector.Constructors
Description : Creates constructors as specified in FixedVector.TH
-}
module FixedVector.Constructors where

import FixedVector.TH
import FixedVector.Type

createVectorN 0

createVectorN 1

createVectorN 2

createVectorN 4

createVectorN 8

createVectorN 16

createVectorN 32
