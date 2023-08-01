{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Trace.Semantics
  ( evalTrace,
    getGlobalEvaluationContext,
    getSubexpressionEvaluationContext,
  )
where

import qualified Algebra.Additive as Group
import qualified Algebra.Ring as Ring
import Control.Lens ((<&>), (^.))
import Control.Monad (forM, forM_, unless, when)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Halo2.Circuit (getCellMapRows, getRowSet, fixedValuesToCellMap)
import Halo2.Types.CellReference (CellReference (CellReference))
import Halo2.Types.Coefficient (Coefficient)
import Halo2.Types.ColumnIndex (ColumnIndex)
import Halo2.Types.FixedValues (FixedValues)
import Halo2.Types.InputExpression (InputExpression)
import Halo2.Types.LookupArgument (LookupArgument)
import Halo2.Types.LookupArguments (LookupArguments)
import Halo2.Types.LookupTableColumn (LookupTableColumn (LookupTableColumn))
import Halo2.Types.Polynomial (Polynomial)
import Halo2.Types.PolynomialConstraints (PolynomialConstraints)
import Halo2.Types.PolynomialVariable (PolynomialVariable)
import Halo2.Types.PowerProduct (PowerProduct)
import Halo2.Types.RowIndex (RowIndex, RowIndexType (Absolute))
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))
import Stark.Types.Scalar (Scalar, one, zero)
import Trace (getCaseRows)
import Trace.Types (Case, OutputSubexpressionId (..), ResultExpressionId (ResultExpressionId), StepType, SubexpressionId, SubexpressionTrace, Trace, TraceType)
import Trace.Types.EvaluationContext (ContextType (Global, Local), EvaluationContext (EvaluationContext))

evalTrace ::
  ann ->
  TraceType ->
  Trace ->
  Either (ErrorMessage ann) ()
evalTrace ann tt t = do
  checkAllResultsArePresentForUsedCases ann tt t
  checkAllStepConstraintsAreSatisfied ann tt t
  -- TODO: check that all step type selection vectors have the correct number of ones
  checkAllEqualityConstraintsAreSatisfied ann tt t

getUsedCases :: Trace -> Set Case
getUsedCases = Map.keysSet . (^. #subexpressions)

checkAllResultsArePresentForUsedCases ::
  ann ->
  TraceType ->
  Trace ->
  Either (ErrorMessage ann) ()
checkAllResultsArePresentForUsedCases ann tt t =
  forM_ (getUsedCases t) $ \c ->
    forM_ (tt ^. #results) $ \(ResultExpressionId sId) ->
      maybe
        ( Left
            ( ErrorMessage
                ann
                ( "result is not present: "
                    <> pack (show (c, sId))
                )
            )
        )
        (const (pure ()))
        (Map.lookup sId =<< Map.lookup c (t ^. #subexpressions))

checkAllStepConstraintsAreSatisfied ::
  ann ->
  TraceType ->
  Trace ->
  Either (ErrorMessage ann) ()
checkAllStepConstraintsAreSatisfied ann tt t = do
  gc <- getGlobalEvaluationContext ann tt t
  forM_ (Map.toList (t ^. #subexpressions)) $
    \(c, ss) ->
      forM_ (Map.toList ss) $
        \(sId, (ri, sT)) -> do
          lc <- getSubexpressionEvaluationContext ann tt t gc (c, sId, sT)
          checkStepConstraintsAreSatisfied ann tt ri sT sId lc

checkStepConstraintsAreSatisfied ::
  ann ->
  TraceType ->
  RowIndex Absolute ->
  SubexpressionTrace ->
  SubexpressionId ->
  EvaluationContext 'Local ->
  Either (ErrorMessage ann) ()
checkStepConstraintsAreSatisfied ann tt ri sT sId ec =
  case Map.lookup (sT ^. #stepType) (tt ^. #stepTypes) of
    Just st -> do
      checkPolynomialConstraints ann ri ec (st ^. #gateConstraints) sT sId
      checkLookupArguments ann ri ec (st ^. #lookupArguments)
    Nothing -> Left (ErrorMessage ann "step type id not defined in trace type")

checkPolynomialConstraints ::
  ann ->
  RowIndex Absolute ->
  EvaluationContext 'Local ->
  PolynomialConstraints ->
  SubexpressionTrace ->
  SubexpressionId ->
  Either (ErrorMessage ann) ()
checkPolynomialConstraints ann c ec cs sT sId =
  sequence_
    [ do
        z <- evalPolynomial ann c ec c'
        unless (z == zero) . Left . ErrorMessage ann $
          "polynomial constraint not satisfied: " <> pack (show (l, c', c, sT, sId, ec ^. #localMappings))
      | (l, c') <- cs ^. #constraints
    ]

evalPolynomial ::
  ann ->
  RowIndex Absolute ->
  EvaluationContext 'Local ->
  Polynomial ->
  Either (ErrorMessage ann) Scalar
evalPolynomial ann c ec p =
  foldr (Group.+) zero
    <$> mapM (evalMonomial ann c ec) (Map.toList (p ^. #monos))

evalMonomial ::
  ann ->
  RowIndex Absolute ->
  EvaluationContext 'Local ->
  (PowerProduct, Coefficient) ->
  Either (ErrorMessage ann) Scalar
evalMonomial ann c ec (pp, c') =
  (Ring.* (c' ^. #getCoefficient)) <$> evalPowerProduct ann c ec pp

evalPowerProduct ::
  ann ->
  RowIndex Absolute ->
  EvaluationContext 'Local ->
  PowerProduct ->
  Either (ErrorMessage ann) Scalar
evalPowerProduct ann c ec pp =
  foldr (Ring.*) one
    <$> mapM
      (\(v, e) -> (^ e) <$> evalPolynomialVariable ann c ec v)
      (Map.toList (pp ^. #getPowerProduct))

evalPolynomialVariable ::
  ann ->
  RowIndex Absolute ->
  EvaluationContext 'Local ->
  PolynomialVariable ->
  Either (ErrorMessage ann) Scalar
evalPolynomialVariable ann c ec v =
  case v ^. #rowIndex of
    0 ->
      maybe
        ( maybe
            (Left (ErrorMessage ann "variable not defined in global or local mappings"))
            pure
            (Map.lookup (c, v ^. #colIndex) (ec ^. #globalMappings))
        )
        pure
        (Map.lookup (v ^. #colIndex) (ec ^. #localMappings))
    _ ->
      Left (ErrorMessage ann "unsupported: nonzero relative row index in polynomial variable in step constraint")

checkLookupArguments ::
  ann ->
  RowIndex Absolute ->
  EvaluationContext 'Local ->
  LookupArguments Polynomial ->
  Either (ErrorMessage ann) ()
checkLookupArguments ann ri ec args =
  mapM_ (checkLookupArgument ann ri ec) (args ^. #getLookupArguments)

checkLookupArgument ::
  ann ->
  RowIndex Absolute ->
  EvaluationContext 'Local ->
  LookupArgument Polynomial ->
  Either (ErrorMessage ann) ()
checkLookupArgument ann ri ec arg = do
  g <- evalPolynomial ann ri ec (arg ^. #gate)
  when (g == zero) $ do
    is <- evalInputExpressions ann ri ec (arg ^. #tableMap)
    case Map.lookup
      (Set.fromList (snd <$> arg ^. #tableMap))
      (ec ^. #lookupTables) of
      Just t ->
        unless
          (is `Set.member` t)
          ( Left
              ( ErrorMessage
                  ann
                  ( "lookup argument is not satisfied: "
                      <> pack (show (arg, ri, is, ec ^. #localMappings))
                  )
              )
          )
      Nothing ->
        Left (ErrorMessage ann "lookup table is not cached in the context")

evalInputExpressions ::
  ann ->
  RowIndex Absolute ->
  EvaluationContext 'Local ->
  [(InputExpression Polynomial, LookupTableColumn)] ->
  Either (ErrorMessage ann) (Map LookupTableColumn Scalar)
evalInputExpressions ann c ec is =
  Map.fromList
    <$> sequence
      [ (col,) <$> evalPolynomial ann c ec (e ^. #getInputExpression)
        | (e, col) <- is
      ]

checkAllEqualityConstraintsAreSatisfied ::
  ann ->
  TraceType ->
  Trace ->
  Either (ErrorMessage ann) ()
checkAllEqualityConstraintsAreSatisfied ann tt t = do
  cellMap <- getGlobalCellMap <$> getGlobalEvaluationContext ann tt t
  forM_ (tt ^. #equalityConstraints . #getEqualityConstraints) $ \eq -> do
    vs <- mapM (lookupCellReference ann cellMap) (Set.toList (eq ^. #getEqualityConstraint))
    case vs of
      [] -> pure ()
      (v : _) ->
        unless (all (== v) vs)
          (Left (ErrorMessage ann "equality constraint not satisifed"))

lookupCellReference ::
  ann ->
  Map CellReference Scalar ->
  CellReference ->
  Either (ErrorMessage ann) Scalar
lookupCellReference ann m r =
  maybe
    (Left (ErrorMessage ann "cell reference lookup failed"))
    pure
    (Map.lookup r m)

getGlobalCellMap ::
  EvaluationContext t ->
  Map CellReference Scalar
getGlobalCellMap ec =
  Map.fromList
    [ (CellReference ci ri, x)
      | ((ri, ci), x) <- Map.toList (ec ^. #globalMappings)
    ]

addFixedValuesToEvaluationContext ::
  EvaluationContext 'Global ->
  FixedValues (RowIndex Absolute) ->
  EvaluationContext 'Global
addFixedValuesToEvaluationContext ec vs =
  ec' <> ec
  where
    ec' =
      EvaluationContext
        ( Map.fromList
            [ ((i, col), x)
              | (col, vs') <- Map.toList (vs ^. #getFixedValues),
                (i, x) <- Map.toList $ vs' ^. #unFixedColumn
            ]
        )
        mempty
        mempty

getGlobalEvaluationContext ::
  ann ->
  TraceType ->
  Trace ->
  Either (ErrorMessage ann) (EvaluationContext 'Global)
getGlobalEvaluationContext ann tt t = do
  let gms = getGlobalMappings tt t
      ec =
        foldl'
          addFixedValuesToEvaluationContext
          (EvaluationContext gms mempty mempty)
          (Map.elems (tt ^. #stepTypes) <&> (^. #fixedValues))
  EvaluationContext gms mempty
    <$> ( Map.fromList
            <$> sequence
              [ (cs,) . Set.fromList
                  <$> getLookupTable
                    ann
                    tt
                    t
                    ec
                    cs -- (trace ("getLookupTable: " <> show cs) cs)
                | cs <- Set.toList . Set.fromList $ traceTypeLookupTables tt
              ]
        )

getGlobalMappings ::
  TraceType ->
  Trace ->
  Map (RowIndex Absolute, ColumnIndex) Scalar
getGlobalMappings tt t =
  mconcat
    [ t ^. #statement . #unStatement,
      t ^. #witness . #unWitness,
      getCaseNumberColumnMappings tt t
    ]

getCaseNumberColumnMappings ::
  TraceType ->
  Trace ->
  Map (RowIndex Absolute, ColumnIndex) Scalar
getCaseNumberColumnMappings tt t =
  Map.fromList
    [((ri, col), c ^. #unCase)
      | c  <- Set.toList (getUsedCases t),
        ri <- Set.toList (getCaseRows (tt ^. #maxStepsPerCase) c)
    ]
  where
    col = tt ^. #caseNumberColumnIndex . #unCaseNumberColumnIndex

traceTypeLookupTables ::
  TraceType ->
  [Set LookupTableColumn]
traceTypeLookupTables tt =
  mconcat (stepTypeLookupTables <$> Map.elems (tt ^. #stepTypes))

stepTypeLookupTables ::
  StepType ->
  [Set LookupTableColumn]
stepTypeLookupTables st =
  lookupArgumentTable <$> Set.toList (st ^. #lookupArguments . #getLookupArguments)

lookupArgumentTable ::
  LookupArgument Polynomial ->
  Set LookupTableColumn
lookupArgumentTable arg =
  Set.fromList ((arg ^. #tableMap) <&> snd)

getSubexpressionEvaluationContext ::
  ann ->
  TraceType ->
  Trace ->
  EvaluationContext 'Global ->
  (Case, SubexpressionId, SubexpressionTrace) ->
  Either (ErrorMessage ann) (EvaluationContext 'Local)
getSubexpressionEvaluationContext ann tt t gc (c, sId, sT) =
  EvaluationContext
    (gc ^. #globalMappings)
    <$> localMappings
    <*> pure (gc ^. #lookupTables)
  where
    localMappings =
      mconcat
        <$> sequence
          [ inputMappings,
            pure outputMapping,
            pure caseNumberMapping,
            pure stepTypeMapping,
            pure stepIndicatorMapping,
            pure (sT ^. #adviceValues)
          ]

    inputMappings =
      case Map.lookup (sT ^. #stepType, OutputSubexpressionId sId) (tt ^. #links) of
        Just is ->
          Map.fromList
            <$> sequence
              -- Special case to consider: when the input is supposed to be the
              -- output of a lookup and this is the subexpr of that lookup,
              -- what happens? Then there is (supposed to be) a subexpression id
              -- of that lookup, and the trace generation should put the correct
              -- value in its output, and it should be an input to the bare lookup,
              -- and its value should be set here. Seems like something nonsensical
              -- is going on here and we should replace the bare lookup step type
              -- maybe with just a (functional) lookup step type.
              [ case Map.lookup iId =<< Map.lookup c (t ^. #subexpressions) of
                  Just (_ri, sT') -> pure (col, sT' ^. #value)
                  Nothing ->
                    -- trace (show (t ^. #subexpressions)) $
                    Left (ErrorMessage ann ("expected input not present: " <> pack (show (c, iId, is, sT))))
                | (col, iId) <-
                    zip
                      ((tt ^. #inputColumnIndices) <&> (^. #unInputColumnIndex))
                      (is <&> (^. #unInputSubexpressionId))
              ]
        Nothing ->
          Left . ErrorMessage ann $
            "no links found for this subexpression's step type and id: "
              <> pack (show (c, sId, sT))

    outputMapping :: Map ColumnIndex Scalar
    outputMapping =
      Map.singleton (tt ^. #outputColumnIndex . #unOutputColumnIndex) (sT ^. #value)

    caseNumberMapping :: Map ColumnIndex Scalar
    caseNumberMapping =
      Map.singleton
        (tt ^. #caseNumberColumnIndex . #unCaseNumberColumnIndex)
        (c ^. #unCase)

    stepTypeMapping :: Map ColumnIndex Scalar
    stepTypeMapping =
      Map.fromList
        [ (ci, if stId == sT ^. #stepType then one else zero)
          | (stId, ci) <-
              Map.toList
                ( tt
                    ^. #stepTypeIdColumnIndices
                      . #unStepTypeIdSelectionVector
                )
        ]

    stepIndicatorMapping :: Map ColumnIndex Scalar
    stepIndicatorMapping =
      Map.singleton (tt ^. #stepIndicatorColumnIndex . #unStepIndicatorColumnIndex) zero

-- We could speed this up by doing the full loop over all subexpressions only when
-- the lookup table columns are not entirely contained by the statement and witness.
getLookupTable ::
  ann ->
  TraceType ->
  Trace ->
  EvaluationContext 'Global ->
  Set LookupTableColumn ->
  Either (ErrorMessage ann) [Map LookupTableColumn Scalar]
getLookupTable ann tt t gc cs =
  let rowSet :: Set (RowIndex Absolute)
      rowSet = getRowSet (tt ^. #rowCount) Nothing
      traceTypeFixedValues :: FixedValues (RowIndex Absolute)
      traceTypeFixedValues = mconcat . Map.elems
                               $ (tt ^. #stepTypes) <&> (^. #fixedValues)
      traceTypeFixedRows :: Map (RowIndex Absolute) (Map ColumnIndex Scalar)
      traceTypeFixedRows = getCellMapRows rowSet
                             (fixedValuesToCellMap traceTypeFixedValues)
      lookupTableFixedRows :: [Map LookupTableColumn Scalar]
      lookupTableFixedRows =
        Map.elems $
          Map.mapKeys LookupTableColumn
            . flip Map.restrictKeys (Set.map (^. #unLookupTableColumn) cs)
            <$> traceTypeFixedRows
  in fmap (zipWith (<>) lookupTableFixedRows . mconcat)
       . forM (Map.toList (t ^. #subexpressions)) $ \(c, ss) ->
         forM (Map.toList ss) $ \(sId, (ri, sT)) -> do
           lc <- getSubexpressionEvaluationContext ann tt t gc (c, sId, sT)
           Map.fromList
             <$> sequence
               [ maybe
                   ( maybe
                       ( Left
                           ( ErrorMessage
                               ann
                               ( "lookup table has a hole: "
                                   <> pack (show (c, col))
                               )
                           )
                       )
                       (pure . (col,))
                       (Map.lookup (ri, col ^. #unLookupTableColumn)
                         (lc ^. #globalMappings))
                   )
                   (pure . (col,))
                   (Map.lookup (col ^. #unLookupTableColumn) (lc ^. #localMappings))
                 | col <- Set.toList cs
               ]
