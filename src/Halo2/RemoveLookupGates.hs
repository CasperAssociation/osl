-- This compiler stage takes an arithmetic circuit and removes the gates from the
-- lookup arguments (i.e., it sets them to zero).
--
-- CAUTION: THIS STAGE ASSUMES (WITHOUT CHECKING IT) THAT ALL LOOKUP ARGUMENT
-- GATE POLYNOMIALS CAN ONLY EVALUATE TO ZERO OR ONE IN A SATISFYING ASSIGNMENT
-- OF VALUES.
--
-- NOTICE: This stage assumes (and checks) that all polynomial variables have a
-- relative row offset of zero.
--
-- This stage works by adding to the circuit:
--  * One dummy row, which contains all zeroes in the pre-existing fixed columns.
--  * One dummy row indicator fixed column, which contains all zeroes in the pre-existing
--    rows and one in the dummy row.
--  * A term in each gate constraint which sets the gate value to zero in the dummy row
--    and does not change the gate value in any other row.
--  * A term in each lookup argument input expression which sets the value to zero in
--    the dummy row and does not change the value in any other row.
--  * To each lookup argument, an input whose value is the dummy row indicator
--    column and whose table column is the dummy row indicator column. This ensures
--    that we only look up non-dummy rows based on non-dummy input expressions.
--
-- Instances and witnesses are compiled into the instances and witnesses for the new
-- circuit by adding zeroes in the new dummy row.


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}


module Halo2.RemoveLookupGates
  ( removeLookupGates,
    removeLookupGatesArgumentConversion
  ) where


import Cast (integerToInt)
import Control.Arrow (first, second)
import Control.Lens ((^.))
import Data.Bool (bool)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Die (die)
import GHC.Generics (Generic)
import Halo2.Circuit (getPolynomialVariables)
import qualified Halo2.Polynomial as P
import Halo2.Types.Argument (Argument (Argument), Statement (Statement), Witness (Witness))
import Halo2.Types.CellReference (CellReference (CellReference))
import Halo2.Types.Circuit (Circuit (Circuit), ArithmeticCircuit)
import Halo2.Types.ColumnIndex (ColumnIndex)
import Halo2.Types.ColumnType (ColumnType (Advice, Instance, Fixed))
import Halo2.Types.ColumnTypes (ColumnTypes (ColumnTypes))
import Halo2.Types.EqualityConstraints (EqualityConstraints (EqualityConstraints))
import Halo2.Types.EqualityConstraint (EqualityConstraint (EqualityConstraint))
import Halo2.Types.EqualityConstrainableColumns (EqualityConstrainableColumns (EqualityConstrainableColumns))
import Halo2.Types.FixedColumn (FixedColumn (FixedColumn))
import Halo2.Types.FixedValues (FixedValues (FixedValues))
import Halo2.Types.InputExpression (InputExpression (InputExpression))
import Halo2.Types.LookupArgument (LookupArgument (LookupArgument))
import Halo2.Types.LookupArguments (LookupArguments (LookupArguments))
import Halo2.Types.LookupTableColumn (LookupTableColumn (LookupTableColumn))
import Halo2.Types.Polynomial (Polynomial)
import Halo2.Types.PolynomialConstraints (PolynomialConstraints (PolynomialConstraints))
import Halo2.Types.RowIndex (RowIndex (RowIndex), RowIndexType (Absolute))
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))
import Stark.Types.Scalar (one, zero, scalarToInteger)


newtype DummyRowIndex = DummyRowIndex { unDummyRowIndex :: RowIndex Absolute }
  deriving Generic


newtype DummyRowIndicatorColumnIndex =
  DummyRowIndicatorColumnIndex
    { unDummyRowIndicatorColumnIndex :: ColumnIndex }
  deriving Generic


getDummyRowIndex :: ArithmeticCircuit -> DummyRowIndex
getDummyRowIndex =
  maybe
    (die "getDummyRowIndex: row index out of range of int")
    (DummyRowIndex . RowIndex)
    . integerToInt . scalarToInteger
    . (^. #rowCount . #getRowCount)


getDummyRowIndicatorColumnIndex ::
  ArithmeticCircuit ->
  DummyRowIndicatorColumnIndex
getDummyRowIndicatorColumnIndex =
  maybe
    (DummyRowIndicatorColumnIndex 0)
    (DummyRowIndicatorColumnIndex . (+1) . fst)
    . Map.lookupMax
    . (^. #columnTypes . #getColumnTypes)


removeLookupGates ::
  ArithmeticCircuit ->
  Either (ErrorMessage ()) ArithmeticCircuit
removeLookupGates c = do
  checkVariableRowOffsetsAreZero c
  pure $ Circuit
    ((c ^. #columnTypes) <> ColumnTypes (Map.singleton (dci ^. #unDummyRowIndicatorColumnIndex) Advice))
    (EqualityConstrainableColumns (Map.keysSet (c ^. #columnTypes . #getColumnTypes)))
    (restrictGateConstraintsToNonDummyRows dci (c ^. #gateConstraints))
    (removeLookupArgumentsGates dci (c ^. #lookupArguments))
    ((c ^. #rowCount) + 1)
    ((c ^. #equalityConstraints) <> getDummyRowEqualityConstraints dri c)
    ((c ^. #fixedValues) <> dummyRowAndColFixedValues dri dci c)
  where
    dri = getDummyRowIndex c
    dci = getDummyRowIndicatorColumnIndex c


getDummyRowEqualityConstraints ::
  DummyRowIndex ->
  ArithmeticCircuit ->
  EqualityConstraints
getDummyRowEqualityConstraints dri c =
  EqualityConstraints . (:[]) . EqualityConstraint
    $ Set.fromList
        [ (CellReference ci (dri ^. #unDummyRowIndex))
          | ci <- Map.keys (c ^. #columnTypes . #getColumnTypes)
        ]


checkVariableRowOffsetsAreZero ::
  ArithmeticCircuit ->
  Either (ErrorMessage ()) ()
checkVariableRowOffsetsAreZero =
  bool
    (Left (ErrorMessage () "Halo2.RemoveLookupGates: not all variable row offsets are zero (this is a compiler bug)"))
    (pure ())
    . all ((== 0) . (^. #rowIndex))
    . getPolynomialVariables


restrictGateConstraintsToNonDummyRows ::
  DummyRowIndicatorColumnIndex ->
  PolynomialConstraints ->
  PolynomialConstraints
restrictGateConstraintsToNonDummyRows dci cs =
  PolynomialConstraints
    (second (restrictGateConstraintToNonDummyRows dci) <$> (cs ^. #constraints))
    ((cs ^. #degreeBound) + 1)


restrictGateConstraintToNonDummyRows ::
  DummyRowIndicatorColumnIndex ->
  Polynomial ->
  Polynomial
restrictGateConstraintToNonDummyRows dci p =
  (P.one `P.minus` P.var' (dci ^. #unDummyRowIndicatorColumnIndex))
    `P.times` p


removeLookupArgumentsGates ::
  DummyRowIndicatorColumnIndex ->
  LookupArguments Polynomial ->
  LookupArguments Polynomial
removeLookupArgumentsGates dci =
  LookupArguments
    . Set.map (removeLookupArgumentGates dci)
    . (^. #getLookupArguments)


removeLookupArgumentGates ::
  DummyRowIndicatorColumnIndex ->
  LookupArgument Polynomial ->
  LookupArgument Polynomial
removeLookupArgumentGates dci'@(DummyRowIndicatorColumnIndex dci) arg =
  LookupArgument
    (arg ^. #label)
    P.zero
    ([(InputExpression ((d `P.plus` g) `P.minus` (d `P.times` g)),
       LookupTableColumn dci)] <>
     (first (gateInputExpression dci' (arg ^. #gate)) <$>
      (arg ^. #tableMap)))
  where
    -- d = 1 iff this is the dummy row
    d = P.var' dci
    -- g = 1 iff the gate is not satisfied
    g = arg ^. #gate


gateInputExpression ::
  DummyRowIndicatorColumnIndex ->
  Polynomial ->
  InputExpression Polynomial ->
  InputExpression Polynomial
gateInputExpression (DummyRowIndicatorColumnIndex dci) p =
  InputExpression
    . (P.times (P.one `P.minus` P.var' dci))
    . (P.times (P.one `P.minus` p))
    . (^. #getInputExpression)


dummyRowAndColFixedValues ::
  DummyRowIndex ->
  DummyRowIndicatorColumnIndex ->
  ArithmeticCircuit ->
  FixedValues (RowIndex Absolute)
dummyRowAndColFixedValues dri dci c =
  FixedValues . Map.fromList
    $ [ (ci, FixedColumn $ Map.fromList [ (dri ^. #unDummyRowIndex, zero) ])
        | (ci, t) <- Map.toList (c ^. #columnTypes . #getColumnTypes),
          t == Fixed
      ]
      <>
      [ (dci ^. #unDummyRowIndicatorColumnIndex,
          FixedColumn $
            Map.singleton (dri ^. #unDummyRowIndex) one <>
              Map.fromList
                [ (ri, zero)
                  | ri <- [0 .. r']
                ]
        )
      ]
  where
    r' =
      maybe
        (die "dummyRowFixedValues: row count is out of range of scalar (this is a compiler bug")
        (RowIndex . subtract 1)
        (integerToInt (scalarToInteger (c ^. #rowCount . #getRowCount)))


removeLookupGatesArgumentConversion ::
  ArithmeticCircuit ->
  Argument ->
  Argument
removeLookupGatesArgumentConversion c arg =
  arg <> Argument (getDummyRowStatement c) (getDummyRowWitness c <> getDummyColWitness c)


getDummyRowStatement ::
  ArithmeticCircuit ->
  Statement
getDummyRowStatement c =
  Statement . Map.fromList $
    [ (CellReference ci ri, zero)
      | (ci, t) <- Map.toList (c ^. #columnTypes . #getColumnTypes),
        t == Instance,
        let ri = getDummyRowIndex c ^. #unDummyRowIndex
    ]


getDummyRowWitness ::
  ArithmeticCircuit ->
  Witness
getDummyRowWitness c =
  Witness . Map.fromList $
    [ (CellReference ci ri, zero)
      | (ci, t) <- Map.toList (c ^. #columnTypes . #getColumnTypes),
        t == Advice || t == Fixed
    ]
  where
    ri = getDummyRowIndex c ^. #unDummyRowIndex


getDummyColWitness ::
  ArithmeticCircuit ->
  Witness
getDummyColWitness c =
  Witness . Map.fromList $
    [ (CellReference ci ri, v)
      | ri <- [0 .. dri],
        let v = if ri == dri then one else zero
    ]
  where
    dri = getDummyRowIndex c
            ^. #unDummyRowIndex

    ci = getDummyRowIndicatorColumnIndex c
           ^. #unDummyRowIndicatorColumnIndex
