{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OSL.Satisfaction (satisfies, satisfiesSimple) where

import Control.Lens ((^.))
import Data.Either.Combinators (mapLeft)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Tuple.Extra (curry3)
import OSL.Evaluation (evaluate)
import OSL.Types.Argument (Argument (Argument), Statement (Statement))
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))
import OSL.Types.EvaluationContext (EvaluationContext (EvaluationContext))
import OSL.Types.OSL (ContextType (Global, Local), Declaration (Defined, FreeVariable), Term (Lambda, NamedTerm), Type (F), ValidContext (ValidContext))
import OSL.Types.PreValue (PreValue (Value))
import OSL.Types.Value (Value (Bool, Pair'))
import OSL.ValidateContext (inferType)

satisfiesSimple :: (Ord ann, Show ann) => ValidContext 'Global ann -> Term ann -> Argument -> Either (ErrorMessage ann) Bool
satisfiesSimple c x arg = do
  xT <- inferType c x
  -- limiting the length of the error message prevents excessive computation,
  -- which is useful in order to be able to see the cause of the error
  mapLeft (\err -> ErrorMessage (err ^. #annotation) (Text.take 1000 (err ^. #message))) $
    satisfies c (ValidContext (c ^. #unValidContext)) mempty xT x arg

satisfies :: (Ord ann, Show ann) => ValidContext 'Global ann -> ValidContext 'Local ann -> EvaluationContext ann -> Type ann -> Term ann -> Argument -> Either (ErrorMessage ann) Bool
satisfies gc lc e = curry3 $
  \case
    ( F _ann _n a b,
      Lambda _ann' v _a' body,
      Argument (Statement (Pair' vs s')) w
      ) ->
        satisfies
          gc
          (lc <> ValidContext (Map.singleton v (FreeVariable a)))
          (e <> EvaluationContext (Map.singleton v (Value vs)))
          b
          body
          (Argument (Statement s') w)
    (F {}, Lambda ann _v _a _body, _) ->
      Left . ErrorMessage ann $
        "expected statement to be a pair but it was not"
    (_, Lambda ann _v _a _Body, _) ->
      Left . ErrorMessage ann $
        "expected lambda type to be a function type but it was not"
    (a, NamedTerm ann name, arg) ->
      case Map.lookup name (lc ^. #unValidContext) of
        Just (Defined _ def) -> satisfies gc lc e a def arg
        _ ->
          Left . ErrorMessage ann $
            "expected the name of a defined predicate"
    (a, x, Argument _ w) -> do
      (== Bool True) <$> evaluate gc lc a x w e
