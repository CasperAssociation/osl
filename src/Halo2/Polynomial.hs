{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Halo2.Polynomial
  ( plus,
    times,
    constant,
    var,
    var',
    multilinearMonomial,
    zero,
    one,
    negative,
    minus,
    sum,
    degree,
    eval,
    normalize,
  )
where

import qualified Algebra.Additive as Group
import qualified Algebra.Ring as Ring
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Halo2.PowerProduct as P
import Halo2.Prelude
import Halo2.Types.Coefficient (Coefficient (..))
import Halo2.Types.ColumnIndex (ColumnIndex)
import Halo2.Types.Polynomial (Polynomial (..))
import Halo2.Types.PolynomialVariable (PolynomialVariable (..))
import Halo2.Types.PowerProduct (PowerProduct (..))
import Stark.Types.Scalar (Scalar)
import qualified Stark.Types.Scalar as S

plus :: Polynomial -> Polynomial -> Polynomial
plus (Polynomial p) (Polynomial q) =
  Polynomial $ Map.unionWith (Group.+) p q

times :: Polynomial -> Polynomial -> Polynomial
times (Polynomial p) (Polynomial q) =
  Polynomial . sumMonomials $
    [ (P.times x y, a Ring.* b)
      | (x, a) <- Map.toList p,
        (y, b) <- Map.toList q
    ]

sumMonomials ::
  [(PowerProduct, Coefficient)] ->
  Map PowerProduct Coefficient
sumMonomials = foldl' g mempty
  where
    g ::
      Map PowerProduct Coefficient ->
      (PowerProduct, Coefficient) ->
      Map PowerProduct Coefficient
    g p (x, a) =
      case Map.lookup x p of
        Just b -> Map.insert x (a Group.+ b) p
        Nothing -> Map.insert x a p

constant :: Scalar -> Polynomial
constant = Polynomial . Map.singleton (PowerProduct mempty) . Coefficient

var :: PolynomialVariable -> Polynomial
var v =
  Polynomial
    (Map.singleton (PowerProduct (Map.singleton v 1)) (Coefficient S.one))

var' :: ColumnIndex -> Polynomial
var' i = var (PolynomialVariable i 0)

multilinearMonomial ::
  Coefficient ->
  [PolynomialVariable] ->
  Polynomial
multilinearMonomial a xs =
  Polynomial
    ( Map.singleton
        ( PowerProduct
            ( Map.fromList
                ((,1) <$> xs)
            )
        )
        a
    )

zero :: Polynomial
zero = constant S.zero

one :: Polynomial
one = constant S.one

minusOne :: Polynomial
minusOne = constant S.minusOne

negative :: Polynomial -> Polynomial
negative = (minusOne `times`)

minus :: Polynomial -> Polynomial -> Polynomial
minus a b = a `plus` negative b

sum :: [Polynomial] -> Polynomial
sum = foldl' plus zero

degree :: Polynomial -> Int
degree (Polynomial p) =
  foldl' max 0 (P.degree <$> Map.keys p)

eval :: Polynomial -> (PolynomialVariable -> Scalar) -> Scalar
eval (Polynomial p) f =
  foldl'
    (+)
    0
    [ c * foldl' (*) 1 [f x ^ e | (x, e) <- Map.toList pp]
      | (PowerProduct pp, Coefficient c) <- Map.toList p
    ]

normalize :: Polynomial -> Polynomial
normalize = Polynomial . Map.filter (/= Coefficient S.zero) . (^. #monos)
