{-# LANGUAGE OverloadedLabels #-}

module Trace.LogicToArithmetic
  ( logicCircuitToArithmeticCircuit
  ) where

import Halo2.Types.BitsPerByte (BitsPerByte)
import Halo2.Types.Circuit (LogicCircuit, ArithmeticCircuit)
import Trace.FromLogicCircuit (logicCircuitToTraceType)
import Trace.ToArithmeticCircuit (traceTypeToArithmeticCircuit)

logicCircuitToArithmeticCircuit
  :: BitsPerByte
  -> LogicCircuit
  -> ArithmeticCircuit
logicCircuitToArithmeticCircuit bitsPerByte =
  traceTypeToArithmeticCircuit . logicCircuitToTraceType bitsPerByte
