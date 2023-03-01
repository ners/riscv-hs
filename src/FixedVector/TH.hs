{-# LANGUAGE TemplateHaskell #-}

module FixedVector.TH where

import FixedVector.Type
import Language.Haskell.TH
import Numeric.Natural (Natural)

-- import Language.Haskell.TH.Lib

-- create a function that we would have written as follows:
-- createVector<N> :: t -> t -> {N times} -> t -> FixedVector <N> t
-- createVector<N> v1 v2 {...} v<N> = FixedVector { _data = [ v1, v2, {...}, v<N> ] }
-- using Template Haskell: https://hackage.haskell.org/package/template-haskell-2.19.0.0/docs/Language-Haskell-TH.html
-- TODO: don't depend on internals of FixedVector and instead use exposed constructors
--       this might only be possible once we have e.g. createVectorUpToN, because then we can depend on the existence of these constructors
--       createVector<N> v1 v2 {...} v<N> = prepend v1 $ createVector<N-1> v2 ... v<N>
createVectorN :: Natural -> DecsQ
createVectorN n =
    pure
        [ SigD functionName functionType
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
    returnType :: Type
    returnType = ConT ''FixedVector `AppT` LitT (NumTyLit $ fromIntegral n) `AppT` vectorElementType

    -- create the following expression:
    -- t -> t -> ... -> t -> t -> FixedVector n t
    -- in terms of our intermediate functions:
    -- vectorElementType -> vectorElementType -> ... -> returnType
    functionType :: Type
    functionType = iterate (prependTerm vectorElementType) returnType !! fromIntegral n
      where
        -- 0th :: returnType
        -- 1st :: AppT vectorElementType (AppT ArrowT returnType)
        -- 2nd :: AppT vectorElementType (AppT ArrowT (AppT (vectorElementType) (AppT ArrowT returnType)))
        -- 3rd :: AppT vectorElementType (AppT ArrowT (AppT vectorElementType (AppT ArrowT (AppT (vectorElementType) (AppT ArrowT returnType)))))
        -- ... (really boring)
        -- nth is what we need!
        prependTerm t u = ArrowT `AppT` t `AppT` u

    -- Documentation:
    clauses :: [Clause]
    clauses = [Clause pats body decls]
      where
        -- function parameters
        -- v1 v2 ... v<N>
        pats = VarP <$> vars
        -- results in:
        -- FixedVector { _data = [ v1, v2, ..., v<N> ] }
        body = NormalB $ RecConE (mkName "FixedVector") [('elements, ListE $ VarE <$> vars)]
        -- `where` clauses, which we currently don't use
        decls = []
        vars :: [Name]
        vars = mkName . ("v" ++) . show <$> [1 .. n]
