{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Semicircuit.CountCases
  ( countCases,
  )
where

import Control.Lens ((^.))
import Data.Bifunctor (second)
import Data.Either (fromRight, partitionEithers)
import Data.Foldable (foldl')
import qualified Data.Map as Map
import qualified Data.Set as Set
import OSL.Sigma11 (evalTerm)
import OSL.Types.Cardinality (cardinalityToInteger)
import OSL.Types.ErrorMessage (ErrorMessage (..))
import OSL.Types.Sigma11.EvaluationContext (EvaluationContextF (..))
import OSL.Types.Sigma11.Value (Value (Value))
import Semicircuit.Types.PNFFormula (Quantifiers (..), UniversalQuantifier (..))
import Semicircuit.Types.Semicircuit (FreeVariables (..), Semicircuit (..))
import Semicircuit.Types.Sigma11 (EvaluationContext, ExistentialQuantifier, InstanceQuantifier)
import Stark.Types.Scalar (fromWord64, toWord64, zero)
import Trace.Types (NumberOfCases (..))

type CountResult = Either (ErrorMessage ()) Integer

countUniversal :: EvaluationContext -> UniversalQuantifier -> CountResult
countUniversal evalctx (All _ bound) = second (toInteger . toWord64) $ evalTerm evalctx bound

countUniversalInit :: UniversalQuantifier -> CountResult
countUniversalInit = countUniversal $ EvaluationContext Map.empty

countUniversals' :: EvaluationContext -> [UniversalQuantifier] -> [CountResult]
countUniversals' _ [] = []
countUniversals' _ [x] = [countUniversalInit x]
countUniversals' (EvaluationContext ctx) (x : xs) = countUniversal ctx' x : countUniversals' ctx' xs
  where
    ctx' = EvaluationContext $ Map.union ctx $ Map.fromList [(k, v)]
    k = Left $ x ^. #name
    v = Value $ Map.fromList [([], fromRight zero $ evalTerm (EvaluationContext ctx) (x ^. #bound))]

countUniversals :: [UniversalQuantifier] -> CountResult
countUniversals uqs
  | null errors = Right value
  | otherwise = Left e
  where
    results = countUniversals' (EvaluationContext Map.empty) uqs
    (errors, values) = partitionEithers results
    e = mconcat errors
    value = foldl' (*) 1 values

countExistential :: ExistentialQuantifier -> Integer
countExistential = cardinalityToInteger . flip (^.) #cardinality

countExistentials :: [ExistentialQuantifier] -> Integer
countExistentials = foldl' max 0 . map countExistential

countInstance :: InstanceQuantifier -> Integer
countInstance = cardinalityToInteger . flip (^.) #cardinality

countInstances :: [InstanceQuantifier] -> Integer
countInstances = foldl' max 0 . map countInstance

eithMaybToEith :: a -> Either a (Maybe b) -> Either a b
eithMaybToEith _ (Left x) = Left x
eithMaybToEith l (Right x) = case x of
  Nothing -> Left l
  Just x' -> Right x'

countCases :: Semicircuit -> Either (ErrorMessage ()) NumberOfCases
countCases (Semicircuit (FreeVariables fv) f)
  | Set.null fv = second NumberOfCases numRows
  | otherwise = Left $ ErrorMessage () "Nonempty free variables"
  where
    overflowErrMsg = ErrorMessage () "Number of cases overflowed"
    qs = f ^. #quantifiers
    x = countUniversals . flip (^.) #universalQuantifiers
    y, z :: Quantifiers -> Integer
    y = countExistentials . flip (^.) #existentialQuantifiers
    z = countInstances . flip (^.) #instanceQuantifiers
    numRows' = Right ((y qs `max` z qs) +) <*> x qs
    numRows = eithMaybToEith overflowErrMsg $ second (fromWord64 . toWord64 . fromInteger) numRows'
