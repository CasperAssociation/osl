{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Semicircuit.CountCases
  ( countCases,
  )
where

import Control.Lens ((^.))
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (foldl')
import Data.Bifunctor (second)
import qualified Data.Map as Map
import qualified Data.Set as Set
import OSL.Sigma11 (evalTerm)
import OSL.Types.Cardinality (cardinalityToInteger)
import OSL.Types.ErrorMessage (ErrorMessage (..))
import OSL.Types.Sigma11.EvaluationContext (EvaluationContextF (..))
import Semicircuit.Types.PNFFormula (Quantifiers (..), UniversalQuantifier (..))
import Semicircuit.Types.Semicircuit (FreeVariables (..), Semicircuit (..))
import Semicircuit.Types.Sigma11 (ExistentialQuantifier, InstanceQuantifier)
import Stark.Types.Scalar (Scalar, fromWord64, toWord64)
import Trace.Types (NumberOfCases (..))

countUniversal :: UniversalQuantifier -> Integer
countUniversal (All _ bound) = f evaluated
  where
    emptyContext = EvaluationContext Map.empty
    evaluated = evalTerm emptyContext bound
    f :: Either (ErrorMessage ()) Scalar -> Integer
    f (Left _) = 0 -- TODO: what do I really want here?
    f (Right s) = toInteger $ toWord64 s

countUniversals :: [UniversalQuantifier] -> Integer
countUniversals = foldl' (*) 1 . map countUniversal

countExistential :: ExistentialQuantifier -> Integer
countExistential = cardinalityToInteger . flip (^.) #cardinality

countExistentials :: [ExistentialQuantifier] -> Integer
countExistentials = foldl' (+) 0 . map countExistential

countInstance :: InstanceQuantifier -> Integer
countInstance = cardinalityToInteger . flip (^.) #cardinality

countInstances :: [InstanceQuantifier] -> Integer
countInstances = foldl' (+) 0 . map countInstance

countCases :: Semicircuit -> Either (ErrorMessage ()) NumberOfCases
countCases (Semicircuit (FreeVariables fv) f)
  | Set.null fv = second NumberOfCases $ maybeToRight (ErrorMessage () "Number of cases overflow") (fromWord64 numRows)
  | otherwise = Left $ ErrorMessage () "Nonempty free variables"
  where
    qs = f ^. #quantifiers
    x, y, z :: Quantifiers -> Integer
    x = countUniversals . flip (^.) #universalQuantifiers
    y = countExistentials . flip (^.) #existentialQuantifiers
    z = countInstances . flip (^.) #instanceQuantifiers
    numRows' = x qs + y qs + z qs
    numRows = toWord64 $ fromInteger numRows'
