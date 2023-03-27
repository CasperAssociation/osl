{-# LANGUAGE DataKinds #-}

module Halo2.Types.CircuitEdit
  ( CircuitEdit (AddColumn, AddEqualityConstraint, AddColumnRotation, AddFixedColumn, AddGate, AddLookupArgument, EnableEquality)
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Halo2.Types.CellReference (CellReference)
import Halo2.Types.ColumnIndex (ColumnIndex)
import Halo2.Types.ColumnType (ColumnType)
import Halo2.Types.Label (Label)
import Halo2.Types.LookupArgument (LookupArgument)
import Halo2.Types.Polynomial (Polynomial)
import Halo2.Types.RowIndex (RowIndex, RowIndexType (Absolute, Relative))
import Stark.Types.Scalar (Scalar)

data CircuitEdit =
    AddColumn ColumnIndex ColumnType
  | EnableEquality ColumnIndex
  | AddColumnRotation ColumnIndex ColumnType (RowIndex Relative)
  | AddGate Label Polynomial
  | AddLookupArgument (LookupArgument Polynomial)
  | AddEqualityConstraint (Set CellReference)
  | AddFixedColumn ColumnIndex (Map (RowIndex Absolute) Scalar)
  deriving (Eq, Ord)
