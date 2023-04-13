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
import Halo2.Types.FixedColumn (FixedColumn (FixedColumn))
import Halo2.Types.FixedValues (FixedValues (FixedValues))
import Halo2.Types.InputExpression (InputExpression (InputExpression))
import Halo2.Types.LookupArgument (LookupArgument (LookupArgument))
import Halo2.Types.LookupArguments (LookupArguments (LookupArguments))
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
    (c ^. #equalityConstrainableColumns)
    (restrictGateConstraintsToNonDummyRows dci (c ^. #gateConstraints))
    (removeLookupArgumentsGates (c ^. #lookupArguments))
    ((c ^. #rowCount) + 1)
    (c ^. #equalityConstraints)
    ((c ^. #fixedValues) <> dummyRowFixedValues dri dci c)
  where
    dri = getDummyRowIndex c
    dci = getDummyRowIndicatorColumnIndex c


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
  LookupArguments Polynomial ->
  LookupArguments Polynomial
removeLookupArgumentsGates =
  LookupArguments . Set.map removeLookupArgumentGates . (^. #getLookupArguments)


removeLookupArgumentGates ::
  LookupArgument Polynomial ->
  LookupArgument Polynomial
removeLookupArgumentGates arg =
  LookupArgument
    (arg ^. #label)
    P.zero
    (first (gateInputExpression (arg ^. #gate)) <$> (arg ^. #tableMap))


gateInputExpression ::
  Polynomial ->
  InputExpression Polynomial ->
  InputExpression Polynomial
gateInputExpression p =
  InputExpression
    . (P.times p)
    . (^. #getInputExpression)


dummyRowFixedValues ::
  DummyRowIndex ->
  DummyRowIndicatorColumnIndex ->
  ArithmeticCircuit ->
  FixedValues (RowIndex Absolute)
dummyRowFixedValues dri dci c =
  FixedValues . Map.fromList
    $ [ (ci, FixedColumn $ Map.fromList [ (ri, zero) | ri <- [0 .. r'] ])
        | ci <- Map.keys (c ^. #columnTypes . #getColumnTypes)
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
        (RowIndex . (subtract 1))
        (integerToInt (scalarToInteger (c ^. #rowCount . #getRowCount)))


removeLookupGatesArgumentConversion ::
  ArithmeticCircuit ->
  Argument ->
  Argument
removeLookupGatesArgumentConversion c arg =
  arg <> Argument (getDummyRowStatement c) (getDummyRowWitness c)


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
        t == Advice || t == Fixed,
        let ri = getDummyRowIndex c ^. #unDummyRowIndex
    ]
