{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Trace.ToArithmeticAIR
  ( traceTypeToArithmeticAIR,
    Mapping (Mapping),
    Mappings (Mappings),
    FixedMappings (FixedMappings),
    mappings,
    traceToArgument,
  )
where

import Cast (intToInteger, integerToInt)
import Control.Arrow (second)
import Control.Lens ((<&>), _2)
import Data.List.Extra (mconcatMap, (!?))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as Set
import Data.Text (pack)
import Die (die)
import Halo2.Circuit (fixedValuesToCellMap, getRowSet)
import qualified Halo2.Polynomial as P
import Halo2.Prelude
import Halo2.Types.AIR (AIR (AIR), ArithmeticAIR)
import Halo2.Types.Argument (Argument (Argument), Statement (Statement), Witness (Witness))
import Halo2.Types.CellReference (CellReference (CellReference))
import Halo2.Types.ColumnIndex (ColumnIndex (ColumnIndex))
import Halo2.Types.ColumnType (ColumnType (Advice, Fixed, Instance))
import Halo2.Types.ColumnTypes (ColumnTypes (ColumnTypes))
import Halo2.Types.FixedColumn (FixedColumn (FixedColumn))
import Halo2.Types.FixedValues (FixedValues (FixedValues))
import Halo2.Types.Polynomial (Polynomial)
import Halo2.Types.PolynomialConstraints (PolynomialConstraints (PolynomialConstraints))
import Halo2.Types.RowIndex (RowIndex (RowIndex), RowIndexType (Absolute))
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))
import Safe (atMay)
import Stark.Types.Scalar (Scalar, integerToScalar, one, scalarToInt, scalarToInteger, zero)
import Trace (getTraceTypeFixedValues)
import Trace.FromLogicCircuit (getDefaultAdvice)
import qualified Trace.FromLogicCircuit as LC
import Trace.Types (Case (Case), InputColumnIndex (InputColumnIndex), InputSubexpressionId (InputSubexpressionId), OutputSubexpressionId (OutputSubexpressionId), ResultExpressionId, StepType, StepTypeId, StepTypeIdSelectionVector, SubexpressionId (SubexpressionId), SubexpressionLink (SubexpressionLink), SubexpressionTrace (SubexpressionTrace), Trace, TraceType)

-- Trace type arithmetic AIRs have the columnar structure
-- of the trace type, with additional fixed columns for:
--  * the table of links, and
--  * the table {(i,1) | i < numCases}.
--
-- Trace type arithmetic AIR gate constraints entail that
-- for each step of each case, the gate constraints of
-- its step type are satisfied.
traceTypeToArithmeticAIR :: TraceType -> LC.Mapping -> ArithmeticAIR
traceTypeToArithmeticAIR t lcM =
  AIR
    colTypes
    (gateConstraints t)
    (t ^. #rowCount)
    ((getTraceTypeFixedValues t
      <> additionalFixedValues t (m ^. #fixed))
      <> zeroFixedValues) -- fill in any gaps with zeroes
  where
    m = mappings t lcM
    colTypes = columnTypes t
    zeroFixedValues =
      FixedValues . Map.fromList $
        [ (ci, FixedColumn . Map.fromList $
                 [ (ri, zero)
                   | ri <- Set.toList (getRowSet (t ^. #rowCount) Nothing)
                 ])
          | (ci, ct) <- Map.toList (colTypes ^. #getColumnTypes),
            ct == Fixed
        ]

columnTypes :: TraceType -> ColumnTypes
columnTypes t =
  (t ^. #columnTypes)
    <> ColumnTypes
      ( Map.fromList
          ( zip
              [i ..]
              (replicate (3 + n) Fixed)
          )
      )
    <> ColumnTypes
      ( Map.fromList
          (zip [j ..] (replicate (n + 2) Advice))
      )
  where
    i :: ColumnIndex
    i =
      ColumnIndex . length . Map.keys $
        t ^. #columnTypes . #getColumnTypes

    j :: ColumnIndex
    j = ColumnIndex $ (i ^. #getColumnIndex) + 3 + n

    n :: Int
    n = length (t ^. #inputColumnIndices)

gateConstraints :: TraceType -> PolynomialConstraints
gateConstraints t =
  mconcatMap
    (stepTypeGateConstraints (t ^. #stepTypeIdColumnIndices))
    (Map.toList (t ^. #stepTypes))

stepTypeGateConstraints :: StepTypeIdSelectionVector -> (StepTypeId, StepType) -> PolynomialConstraints
stepTypeGateConstraints i (tId, t) =
  PolynomialConstraints
    (second (gateOnStepType i tId) <$> (t ^. #gateConstraints . #constraints))
    (t ^. #gateConstraints . #degreeBound + 1)

gateOnStepType :: StepTypeIdSelectionVector -> StepTypeId -> Polynomial -> Polynomial
gateOnStepType m stId =
  P.times $
    maybe
      (die "gateOnStepType: step type id column mapping lookup failed")
      P.var'
      (Map.lookup stId (m ^. #unStepTypeIdSelectionVector))

data CaseUsed = CaseIsUsed | CaseIsNotUsed
  deriving (Eq)

newtype Mapping a = Mapping {unMapping :: ColumnIndex}
  deriving (Generic, Show)

data Mappings = Mappings
  { fixed :: FixedMappings,
    advice :: AdviceMappings,
    logicCircuit :: LC.Mapping
  }
  deriving (Generic, Show)

data FixedMappings = FixedMappings
  { stepType :: Mapping StepTypeId,
    inputs :: [Mapping InputSubexpressionId],
    output :: Mapping OutputSubexpressionId,
    result :: Mapping ResultExpressionId
  }
  deriving (Generic, Show)

data AdviceMappings = AdviceMappings
  { inputs :: [Mapping InputSubexpressionId],
    output :: Mapping OutputSubexpressionId,
    -- The actual number of cases may be less than the maximum
    -- number of cases, so we need to track whether each case
    -- is used or not.
    caseUsed :: Mapping CaseUsed
  }
  deriving (Generic, Show)

newtype AIRFixedValues = AIRFixedValues
  { unAIRFixedValues ::
      Map (RowIndex 'Absolute) (Map ColumnIndex Scalar)
  }
  deriving (Generic)

getAIRFixedValues ::
  TraceType ->
  Mappings ->
  AIRFixedValues
getAIRFixedValues tt m =
  fixedValuesToAIRFixedValues
    tt
    (additionalFixedValues tt (m ^. #fixed))

fixedValuesToAIRFixedValues ::
  TraceType ->
  FixedValues (RowIndex 'Absolute) ->
  AIRFixedValues
fixedValuesToAIRFixedValues tt fvs =
  AIRFixedValues $
    Map.unionsWith
      (<>)
      [ Map.singleton ri (Map.singleton ci y)
        | (ci, col) <-
            Map.toList (fvs ^. #getFixedValues),
          (ri, y) <-
            zip [0 .. n] (padInfinitely (Map.elems (col ^. #unFixedColumn)))
      ]
  where
    n = maxRowIndex tt

maxRowIndex :: TraceType -> RowIndex 'Absolute
maxRowIndex tt =
  RowIndex (c * n - 1)
  where
    c = scalarToInt $ tt ^. #numCases . #unNumberOfCases
    n = Set.size (tt ^. #subexpressions)

mappings :: TraceType -> LC.Mapping -> Mappings
mappings t =
  Mappings
    ( FixedMappings
        (Mapping i :: Mapping StepTypeId)
        (Mapping <$> [j .. k] :: [Mapping InputSubexpressionId])
        (Mapping (k + 1) :: Mapping OutputSubexpressionId)
        (Mapping (k + 2) :: Mapping ResultExpressionId)
    )
    ( AdviceMappings
        (Mapping <$> [k + 3 .. l] :: [Mapping InputSubexpressionId])
        (Mapping (l + 1) :: Mapping OutputSubexpressionId)
        (Mapping (l + 2) :: Mapping CaseUsed)
    )
  where
    i :: ColumnIndex
    i = ColumnIndex (Map.size (t ^. #columnTypes . #getColumnTypes))

    j :: ColumnIndex
    j = i + 1

    k :: ColumnIndex
    k = j + ColumnIndex (n - 1)

    l :: ColumnIndex
    l = k + 3 + ColumnIndex (n - 1)

    n :: Int
    n = length (t ^. #inputColumnIndices)

additionalFixedValues ::
  TraceType ->
  FixedMappings ->
  FixedValues (RowIndex 'Absolute)
additionalFixedValues t m =
  linksTableFixedColumns t (linksTable t) m
    <> caseAndResultFixedColumns t m

newtype LinksTable = LinksTable
  {unLinksTable :: [SubexpressionLink]}
  deriving (Generic)

linksTable ::
  TraceType ->
  LinksTable
linksTable =
  LinksTable . fmap (\((st, o), is) -> SubexpressionLink st is o) . Map.toList . (^. #links)

linksTableFixedColumns ::
  TraceType ->
  LinksTable ->
  FixedMappings ->
  FixedValues (RowIndex 'Absolute)
linksTableFixedColumns tt (LinksTable ls) m =
  FixedValues . Map.fromList $
    [ ( m ^. #stepType . #unMapping,
        FixedColumn . Map.fromList . zip [0 ..] $
          f (ls <&> (^. #stepType . #unStepTypeId))
      ),
      ( m ^. #output . #unMapping,
        FixedColumn . Map.fromList . zip [0 ..] $
          f (ls <&> (^. #output . #unOutputSubexpressionId . #unSubexpressionId))
      )
    ]
      <> zip
        ((m ^. #inputs) <&> (^. #unMapping))
        [ FixedColumn . Map.fromList . zip [0 ..] $
            maybe
              (replicate nRows (InputSubexpressionId (SubexpressionId zero)))
              f
              ((ls <&> (^. #inputs)) !? i)
              <&> (^. #unInputSubexpressionId . #unSubexpressionId)
          | i <- [0 .. length (m ^. #inputs) - 1]
        ]
  where
    f = take nRows . padInfinitely
    nRows = scalarToInt $ tt ^. #rowCount . #getRowCount

caseAndResultFixedColumns ::
  TraceType ->
  FixedMappings ->
  FixedValues (RowIndex 'Absolute)
caseAndResultFixedColumns t m =
  FixedValues $
    Map.fromList
      [ ( t ^. #caseNumberColumnIndex . #unCaseNumberColumnIndex,
          FixedColumn . Map.fromList . zip [0 .. nRows - 1] $
            concatMap
              (replicate nSubs . g)
              [0 .. nCases - 1]
        ),
        ( m ^. #result . #unMapping,
          FixedColumn . Map.fromList . zip [0 .. nRows - 1] . fmap f
            . concat
            . replicate nCases
            . take nSubs
            . padInfinitely
            $ [0 .. nResults - 1]
        )
      ]
  where
    nResults = Set.size (t ^. #results)
    nCases = scalarToInt (t ^. #numCases . #unNumberOfCases)
    nRows = RowIndex . scalarToInt $ t ^. #rowCount . #getRowCount
    nSubs = Set.size (t ^. #subexpressions)

    g :: Int -> Scalar
    g =
      fromMaybe
        (die "caseAndResultFixedColumns: case number out of range of scalar (this is a compiler bug)")
        . integerToScalar
        . intToInteger

    f :: Int -> Scalar
    f =
      maybe
        (die "caseAndResultFixedColumns: result index out of range (this is a compiler bug)")
        (^. #unResultExpressionId . #unSubexpressionId)
        . (Set.toList (t ^. #results) `atMay`)

traceToArgument ::
  ann ->
  TraceType ->
  LC.Mapping ->
  Trace ->
  Either (ErrorMessage ann) Argument
traceToArgument ann tt lcM t = do
  casesArg <- 
    mconcat <$> sequence
      [ caseArgument ann tt t m fvs airFvs c' ri
        | c <- [0 .. scalarToInteger (tt ^. #numCases . #unNumberOfCases) - 1],
          c' <- maybeToList (Case <$> integerToScalar c),
          ri <- caseRowIndices tt c'
      ]
  pure (casesArg <> traceTypeFixedValsArg
                 <> additionalFixedValsArg
                 <> (Argument (t ^. #statement) (t ^. #witness)
                 <> zeroArg))
  where
    traceTypeFixedValsArg =
      Argument mempty . Witness
        $ fixedValuesToCellMap (getTraceTypeFixedValues tt)
    additionalFixedValsArg =
      Argument mempty . Witness . fixedValuesToCellMap
        $ additionalFixedValues tt (mappings tt lcM ^. #fixed)
    n = maybe
          (die "Trace.ToArithmeticAIR.traceToArgument: row count exceeds max Int")
          RowIndex
          (integerToInt (scalarToInteger (tt ^. #rowCount . #getRowCount)))
    zeroMap preferredColType = Map.fromList
      [ (CellReference ci ri, zero)
        | (ci, colType) <- Map.toList $ columnTypes tt ^. #getColumnTypes,
          colType == preferredColType,
          ri <- [0 .. n - 1]
      ]
    zeroArg = Argument (Statement (zeroMap Instance))
                       (Witness (zeroMap Advice <> zeroMap Fixed))
    m = mappings tt lcM
    air = traceTypeToArithmeticAIR tt lcM
    fvs = air ^. #fixedValues
    airFvs = getAIRFixedValues tt m

caseArgument ::
  ann ->
  TraceType ->
  Trace ->
  Mappings ->
  FixedValues (RowIndex 'Absolute) ->
  AIRFixedValues ->
  Case ->
  RowIndex Absolute ->
  Either (ErrorMessage ann) Argument
caseArgument ann tt t m fvs airFvs c ri =
  case Map.lookup c (t ^. #subexpressions) of
    Nothing -> unusedCaseArgument ann tt t m fvs airFvs c ri
    Just es ->
      if Map.null es
        then unusedCaseArgument ann tt t m fvs airFvs c ri
        else usedCaseArgument ann tt t m fvs airFvs c ri es

caseRowIndices ::
  TraceType ->
  Case ->
  [RowIndex 'Absolute]
caseRowIndices tt (Case (scalarToInteger -> c)) =
  [start .. end]
  where
    c' =
      fromMaybe
        (die "caseRowIndices: case number out of range of Int")
        (integerToInt c)
    start = RowIndex (n * c')
    end = start + RowIndex n - 1
    -- TODO: n can sometimes be less if we can show that every evaluation will only
    -- require a subset of the subexpressions due to short circuiting, and this would
    -- in turn allow us to decrease the row count
    n = Set.size (tt ^. #subexpressions)

usedCaseArgument ::
  ann ->
  TraceType ->
  Trace ->
  Mappings ->
  FixedValues (RowIndex 'Absolute) ->
  AIRFixedValues ->
  Case ->
  RowIndex Absolute ->
  Map SubexpressionId (RowIndex Absolute, SubexpressionTrace) ->
  Either (ErrorMessage ann) Argument
usedCaseArgument ann tt t m fvs airFvs c ri es = do
  arg0 <- emptyCaseArgument ann tt t m fvs airFvs c ri CaseIsUsed
  args <-
    mapM
      (\(_ri, (sId, (_ri', sT))) ->
        subexpressionArgument ann tt t m fvs airFvs c ri CaseIsUsed sId sT)
      (filter ((== ri) . fst)
        (zip (caseRowIndices tt c) (Map.toList es)))
  pure $ mconcat args <> arg0

unusedCaseArgument ::
  ann ->
  TraceType ->
  Trace ->
  Mappings ->
  FixedValues (RowIndex 'Absolute) ->
  AIRFixedValues ->
  Case ->
  RowIndex Absolute ->
  Either (ErrorMessage ann) Argument
unusedCaseArgument ann tt t m fvs airFvs c ri =
  emptyCaseArgument ann tt t m fvs airFvs c ri CaseIsNotUsed

emptyCaseArgument ::
  ann ->
  TraceType ->
  Trace ->
  Mappings ->
  FixedValues (RowIndex 'Absolute) ->
  AIRFixedValues ->
  Case ->
  RowIndex Absolute ->
  CaseUsed ->
  Either (ErrorMessage ann) Argument
emptyCaseArgument = voidRow

voidRow ::
  ann ->
  TraceType ->
  Trace ->
  Mappings ->
  FixedValues (RowIndex 'Absolute) ->
  AIRFixedValues ->
  Case ->
  RowIndex 'Absolute ->
  CaseUsed ->
  Either (ErrorMessage ann) Argument
voidRow ann tt t m fvs airFvs c ri used =
  subexpressionArgument
    ann
    tt
    t
    m
    fvs
    airFvs
    c
    ri
    used
    voidEid
    (SubexpressionTrace zero voidStepType defaultAdvice)
  where
    voidEid = 0 -- TODO: is it better not to assume void has subexpression id 0 and step type 0?
    voidStepType = 0
    defaultAdvice = getDefaultAdvice (m ^. #logicCircuit)

subexpressionArgument ::
  ann ->
  TraceType ->
  Trace ->
  Mappings ->
  FixedValues (RowIndex 'Absolute) ->
  AIRFixedValues ->
  Case ->
  RowIndex Absolute ->
  CaseUsed ->
  SubexpressionId ->
  SubexpressionTrace ->
  Either (ErrorMessage ann) Argument
subexpressionArgument ann tt t m fvs airFvs c ri used sId sT = do
  mconcat
    <$> sequence
      [ pure $ traceTypeFixedValuesArgument tt fvs ri,
        airFixedValuesArgument ann airFvs ri,
        traceStatementValuesArgument ann tt t c ri,
        traceWitnessValuesArgument ann tt t c ri,
        subexpressionTraceValuesArgument ann tt t m c used sId sT ri
      ]

traceTypeFixedValuesArgument ::
  TraceType ->
  FixedValues (RowIndex 'Absolute) ->
  RowIndex 'Absolute ->
  Argument
traceTypeFixedValuesArgument tt fvs ri =
  mconcat $
    [ Argument mempty . Witness
        $ Map.singleton (CellReference ci ri) x
      | ci <-
          Map.keys $
            Map.filter
              (== Fixed)
              (tt ^. #columnTypes . #getColumnTypes),
        x <- maybeToList $
          Map.lookup ri
              =<< ( Map.lookup ci (fvs ^. #getFixedValues)
                      <&> (^. #unFixedColumn) )
    ]

airFixedValuesArgument ::
  ann ->
  AIRFixedValues ->
  RowIndex 'Absolute ->
  Either (ErrorMessage ann) Argument
airFixedValuesArgument ann airFvs ri =
  maybe
    ( Left
        ( ErrorMessage
            ann
            ("airFixedValuesArgument: failed row lookup: " <> pack (show ri))
        )
    )
    ( pure . Argument mempty . Witness
        . Map.mapKeys (`CellReference` ri)
    )
    (Map.lookup ri (airFvs ^. #unAIRFixedValues))

traceStatementValuesArgument ::
  ann ->
  TraceType ->
  Trace ->
  Case ->
  RowIndex 'Absolute ->
  Either (ErrorMessage ann) Argument
traceStatementValuesArgument ann tt t _c ri =
  (`Argument` Witness mempty) . Statement
    . Map.fromList
    <$> sequence
      [ (CellReference ci ri,)
          <$> maybe
            (Left (ErrorMessage ann "traceStatementValuesArgument"))
            pure
            ( Map.lookup
                (CellReference ci ri)
                (t ^. #statement . #unStatement)
            )
        | ci <-
            Map.keys
              ( Map.filter
                  (== Instance)
                  (tt ^. #columnTypes . #getColumnTypes)
              )
      ]

traceWitnessValuesArgument ::
  ann ->
  TraceType ->
  Trace ->
  Case ->
  RowIndex 'Absolute ->
  Either (ErrorMessage ann) Argument
traceWitnessValuesArgument ann tt t c ri =
  Argument mempty . Witness . Map.fromList
    <$> sequence
      [ (CellReference ci ri,)
          <$> maybe
            ( Left
                ( ErrorMessage
                    ann
                    ("traceWitnessValuesArgument: value lookup failed: " <> pack (show (c, ci)))
                )
            )
            pure
            ( Map.lookup
                (CellReference ci ri)
                (t ^. #witness . #unWitness)
            )
        | ci <-
            Map.keys
              ( Map.filter
                  (\ct -> ct == Advice || ct == Fixed)
                  (tt ^. #columnTypes . #getColumnTypes)
              ),
          -- TODO: make this less brittle; we need to select only the advice columns
          -- which came from the logic circuit, and this is a way to do it
          ci < tt ^. #caseNumberColumnIndex . #unCaseNumberColumnIndex
      ]

subexpressionTraceValuesArgument ::
  ann ->
  TraceType ->
  Trace ->
  Mappings ->
  Case ->
  CaseUsed ->
  SubexpressionId ->
  SubexpressionTrace ->
  RowIndex 'Absolute ->
  Either (ErrorMessage ann) Argument
subexpressionTraceValuesArgument ann tt t m c used sId sT ri =
  Argument mempty . Witness . mconcat
    <$> sequence
      [ -- case used
        pure $
          Map.singleton
            (CellReference (m ^. #advice . #caseUsed . #unMapping) ri)
            (if used == CaseIsUsed then one else zero),
        -- step type
        pure $
          Map.fromList
            [ ( CellReference ci ri,
                if sT ^. #stepType == stId then one else zero
              )
              | (stId, ci) <-
                  Map.toList (tt ^. #stepTypeIdColumnIndices . #unStepTypeIdSelectionVector)
            ],
        -- step indicator
        pure $
          Map.singleton
            (CellReference (tt ^. #stepIndicatorColumnIndex . #unStepIndicatorColumnIndex) ri)
            zero,
        -- output subexpression id
        pure $
          Map.singleton
            (CellReference (m ^. #advice . #output . #unMapping) ri)
            (sId ^. #unSubexpressionId),
        -- output value
        pure $
          Map.singleton
            (CellReference (tt ^. #outputColumnIndex . #unOutputColumnIndex) ri)
            (sT ^. #value),
        -- input subexpression ids and values, and advice from subexpression trace
        do
          inIds <-
            maybe
              ( Left . ErrorMessage ann $
                  "subexpressionTraceValuesArgument: link not found: "
                    <> pack (show (sT ^. #stepType, OutputSubexpressionId sId))
              )
              pure
              ( Map.lookup
                  (sT ^. #stepType, OutputSubexpressionId sId)
                  (tt ^. #links)
              )
          mconcat
            <$> sequence
              [ do
                  x0 <-
                    (CellReference inCol ri,)
                      <$> maybe
                        (Left (ErrorMessage ann "subexpressionTraceValuesArgument: input subexpression id"))
                        (pure . (^. _2 . #value))
                        ( Map.lookup c (t ^. #subexpressions)
                            >>= Map.lookup inId
                        )
                  let x1 = (CellReference sIdCol ri, inId ^. #unSubexpressionId)
                      xs = (\(ci, y) -> (CellReference ci ri, y)) <$> Map.toList (sT ^. #adviceValues)
                  pure (Map.fromList (x0 : x1 : xs))
                | (InputSubexpressionId inId, InputColumnIndex inCol, Mapping sIdCol) <-
                    zip3 inIds (tt ^. #inputColumnIndices) (m ^. #advice . #inputs)
              ]
      ]

padInfinitely :: [a] -> [a]
padInfinitely [] = []
padInfinitely [a] = repeat a
padInfinitely (x : xs) = x : padInfinitely xs
