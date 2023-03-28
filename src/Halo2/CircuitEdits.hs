{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Halo2.CircuitEdits
  ( getCircuitEdits
  ) where

import Control.Lens ((^.))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Halo2.Types.Circuit (ArithmeticCircuit)
import Halo2.Types.CircuitEdit (CircuitEdit (AddColumn, EnableEquality, AddGate, AddLookupArgument))
import Halo2.Types.ColumnIndex (ColumnIndex (ColumnIndex))
import Halo2.Types.ColumnTypes (ColumnTypes (ColumnTypes))
import Halo2.Types.EqualityConstrainableColumns (EqualityConstrainableColumns (EqualityConstrainableColumns))
import Halo2.Types.PolynomialConstraints (PolynomialConstraints (PolynomialConstraints))
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))

-- Get a list of edits which turns the empty circuit
-- into the given circuit.
getCircuitEdits :: ArithmeticCircuit -> Either (ErrorMessage ()) [CircuitEdit]
getCircuitEdits c =
  mconcat <$> sequence
    [ getColumnTypeEdits (c ^. #columnTypes),
      pure $ getEqualityConstrainableColumnsEdits (c ^. #equalityConstrainableColumns),
      pure $ getGateConstraintEdits (c ^. #gateConstraints),
      pure $ AddLookupArgument <$> Set.toList (c ^. #lookupArguments . #getLookupArguments)
      -- TODO
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
