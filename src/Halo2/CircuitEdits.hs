{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Halo2.CircuitEdits
  ( getCircuitEdits
  ) where

import Control.Arrow (second)
import Control.Lens ((^.))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Halo2.Types.Circuit (ArithmeticCircuit)
import Halo2.Types.CircuitEdit (CircuitEdit (AddColumn, EnableEquality, AddEqualityConstraint, AddFixedColumn, AddGate, AddLookupArgument, AddLookupTable))
import Halo2.Types.ColumnIndex (ColumnIndex (ColumnIndex))
import Halo2.Types.ColumnTypes (ColumnTypes (ColumnTypes))
import Halo2.Types.EqualityConstraints (EqualityConstraints)
import Halo2.Types.EqualityConstrainableColumns (EqualityConstrainableColumns (EqualityConstrainableColumns))
import Halo2.Types.FixedValues (FixedValues)
import Halo2.Types.Label (Label (Label))
import Halo2.Types.PolynomialConstraints (PolynomialConstraints (PolynomialConstraints))
import Halo2.Types.RowIndex (RowIndex, RowIndexType (Absolute))
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))

-- Get a list of edits which turns the empty circuit
-- into the given circuit.
getCircuitEdits :: ArithmeticCircuit -> Either (ErrorMessage ()) [CircuitEdit]
getCircuitEdits c =
  mconcat <$> sequence
    [ getColumnTypeEdits (c ^. #columnTypes),
      pure $ getEqualityConstrainableColumnsEdits (c ^. #equalityConstrainableColumns),
      pure $ getGateConstraintEdits (c ^. #gateConstraints),
      pure $ uncurry AddLookupTable <$>
        zip (Label . ("tab_" <>) . show <$> [0 :: Int ..])
          (Set.toList
            (Set.map
              (fmap snd . (^. #tableMap))
              (c ^. #lookupArguments . #getLookupArguments))),
      pure $ AddLookupArgument <$>
        Set.toList (c ^. #lookupArguments . #getLookupArguments),
      pure $ getEqualityConstraintsEdits (c ^. #equalityConstraints),
      pure $ getFixedColumnsEdits (c ^. #fixedValues)
    ]

getColumnTypeEdits :: ColumnTypes -> Either (ErrorMessage ()) [CircuitEdit]
getColumnTypeEdits (ColumnTypes colTypes) =
  sequence
    [ maybe
        (Left (ErrorMessage () "column indices in ColumnTypes map are not sequential"))
        (pure . AddColumn i)
        (Map.lookup i colTypes)
      | i <- ColumnIndex <$> [0 .. Map.size colTypes - 1]
    ]

getEqualityConstrainableColumnsEdits :: EqualityConstrainableColumns -> [CircuitEdit]
getEqualityConstrainableColumnsEdits (EqualityConstrainableColumns eqcs) =
  EnableEquality <$> Set.toList eqcs

getGateConstraintEdits :: PolynomialConstraints -> [CircuitEdit]
getGateConstraintEdits (PolynomialConstraints cs _degreeBound) =
  uncurry AddGate <$> cs

getEqualityConstraintsEdits :: EqualityConstraints -> [CircuitEdit]
getEqualityConstraintsEdits = fmap (AddEqualityConstraint . (^. #getEqualityConstraint)) . (^. #getEqualityConstraints)

getFixedColumnsEdits :: FixedValues (RowIndex Absolute) -> [CircuitEdit]
getFixedColumnsEdits = fmap (uncurry AddFixedColumn . second (^. #unFixedColumn)) . Map.toList . (^. #getFixedValues)
