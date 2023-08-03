{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Trace (getCaseRows, getTraceTypeFixedValues) where

import Cast (integerToInt)
import Control.Lens ((^.))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Die (die)
import Halo2.Types.FixedValues (FixedValues)
import Halo2.Types.RowIndex (RowIndex (RowIndex), RowIndexType (Absolute))
import Stark.Types.Scalar (scalarToInteger)
import Trace.Types (MaxStepsPerCase (MaxStepsPerCase), Case (Case), TraceType)

getCaseRows ::
  MaxStepsPerCase ->
  Case ->
  Set (RowIndex Absolute)
getCaseRows (MaxStepsPerCase n) (Case m) =
  Set.fromList (RowIndex <$> [m' * n' .. (m' * n') + (n' - 1)])
  where
    n' =
      fromMaybe (die "Trace.FromLogicCircuit.caseFixedValuesToRowFixedValues: max steps per case > max Int")
        . integerToInt
        . scalarToInteger $ n

    m' =
      fromMaybe (die "Trace.FromLogicCircuit.caseFixedValuesToRowFixedValues: case # > max Int")
        . integerToInt
        . scalarToInteger $ m

-- Converts the fixed values in the trace type from one per case to
-- one per row.
getTraceTypeFixedValues ::
  TraceType ->
  FixedValues (RowIndex 'Absolute)
getTraceTypeFixedValues tt =
  ((tt ^. #fixedValues) <>)
    . mconcat
    . fmap (^. #fixedValues)
    . Map.elems
    $ tt ^. #stepTypes
