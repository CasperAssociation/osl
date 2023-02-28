{-# LANGUAGE DataKinds #-}

module Halo2.Types.CircuitEdit
  ( CircuitEdit (AddColumn, AddEquality, AddFixedColumn, AddGate, AddLookupArgument, EnableEquality)
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Halo2.Types.CellReference (CellReference)
import Halo2.Types.ColumnIndex (ColumnIndex)
import Halo2.Types.ColumnType (ColumnType)
import Halo2.Types.LookupArgument (LookupArgument)
import Halo2.Types.Polynomial (Polynomial)
import Halo2.Types.RowIndex (RowIndex, RowIndexType (Absolute))
import Stark.Types.Scalar (Scalar)

data CircuitEdit =
    AddColumn ColumnIndex ColumnType
  | EnableEquality ColumnIndex
  | AddGate Polynomial
  | AddLookupArgument (LookupArgument Polynomial)
  | AddEquality (Set CellReference)
  | AddFixedColumn ColumnIndex (Map (RowIndex Absolute) Scalar)
