{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Halo2.CircuitEdits
  ( getCircuitEdits
  ) where

import Control.Lens ((^.))
import qualified Data.Map as Map
import Halo2.Types.Circuit (ArithmeticCircuit)
import Halo2.Types.CircuitEdit (CircuitEdit (AddColumn))
import Halo2.Types.ColumnIndex (ColumnIndex (ColumnIndex))
import Halo2.Types.ColumnTypes (ColumnTypes (ColumnTypes))
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))

-- Get a list of edits which turns the empty circuit
-- into the given circuit.
getCircuitEdits :: ArithmeticCircuit -> Either (ErrorMessage ()) [CircuitEdit]
getCircuitEdits c =
  getColumnTypeEdits (c ^. #columnTypes)
  -- TODO

getColumnTypeEdits :: ColumnTypes -> Either (ErrorMessage ()) [CircuitEdit]
getColumnTypeEdits (ColumnTypes colTypes) =
  sequence
    [ maybe
        (Left (ErrorMessage () "column indices in ColumnTypes map are not sequential"))
        (pure . AddColumn)
        (Map.lookup i colTypes)
      | i <- ColumnIndex <$> [0 .. Map.size colTypes - 1]
    ]
