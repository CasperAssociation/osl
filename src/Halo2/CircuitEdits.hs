{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Halo2.CircuitEdits
  ( getCircuitEdits
  ) where

import Control.Lens ((^.))
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import qualified Data.Set as Set
import Halo2.Circuit (getPolynomialVariables)
import Halo2.Types.Circuit (ArithmeticCircuit)
import Halo2.Types.CircuitEdit (CircuitEdit (AddColumn, EnableEquality, AddColumnRotation, AddGate))
import Halo2.Types.ColumnIndex (ColumnIndex (ColumnIndex))
import Halo2.Types.ColumnType (ColumnType)
import Halo2.Types.ColumnTypes (ColumnTypes (ColumnTypes))
import Halo2.Types.EqualityConstrainableColumns (EqualityConstrainableColumns (EqualityConstrainableColumns))
import Halo2.Types.Label (Label)
import Halo2.Types.Polynomial (Polynomial)
import Halo2.Types.PolynomialConstraints (PolynomialConstraints (PolynomialConstraints))
import Halo2.Types.PolynomialVariable (PolynomialVariable (PolynomialVariable))
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))

-- Get a list of edits which turns the empty circuit
-- into the given circuit.
getCircuitEdits :: ArithmeticCircuit -> Either (ErrorMessage ()) [CircuitEdit]
getCircuitEdits c =
  mconcat <$> sequence
    [ getColumnTypeEdits (c ^. #columnTypes),
      pure $ getEqualityConstrainableColumnsEdits (c ^. #equalityConstrainableColumns),
      pure $ getGateConstraintEdits (c ^. #columnTypes) (c ^. #gateConstraints)
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

getGateConstraintEdits :: ColumnTypes -> PolynomialConstraints -> [CircuitEdit]
getGateConstraintEdits cts (PolynomialConstraints cs _degreeBound) =
  Set.toList . mconcat $ uncurry (getPolyConstraintEdits cts) <$> cs

getPolyConstraintEdits :: ColumnTypes -> Label -> Polynomial -> Set.Set CircuitEdit
getPolyConstraintEdits cts l p =
  Set.singleton (AddGate l p)
    <> Set.fromList
         [ AddColumnRotation i t j
           | PolynomialVariable i j <- Set.toList $ getPolynomialVariables p,
             t <- maybeToList $ getColumnType cts i
         ]

getColumnType :: ColumnTypes -> ColumnIndex -> Maybe ColumnType
getColumnType cts i =
  Map.lookup i (cts ^. #getColumnTypes)
