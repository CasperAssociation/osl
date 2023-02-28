{-# LANGUAGE DeriveGeneric #-}

module OSL.Types.Stages (Stages (Stages)) where

import GHC.Generics (Generic)
import Halo2.Types.Circuit (ArithmeticCircuit, LogicCircuit)
import Halo2.Types.CircuitMetrics (CircuitMetrics)
import OSL.Types.Sigma11 (Formula, AuxTables)
import qualified Semicircuit.Types.Sigma11 as Semi
import qualified Semicircuit.Types.PNFFormula as PNF
import Semicircuit.Types.Semicircuit (Semicircuit)
import Semicircuit.Types.SemicircuitToLogicCircuitColumnLayout (SemicircuitToLogicCircuitColumnLayout)
import Trace.FromLogicCircuit (Mapping)
import Trace.ToArithmeticAIR (Mappings)
import Trace.Types (TraceType)
import Trace.Types.Metrics (TraceTypeMetrics)

data Stages =
  Stages
    { translation :: Formula,
      auxTables :: AuxTables,
      pnf :: Maybe ([Semi.Quantifier], Semi.Formula),
      spnf :: Maybe ([Semi.Quantifier], Semi.Formula),
      pnff :: Maybe PNF.Formula,
      semi :: Maybe Semicircuit,
      logic :: Maybe LogicCircuit,
      layout :: Maybe SemicircuitToLogicCircuitColumnLayout,
      traceLayout :: Maybe Mapping,
      traceType :: Maybe TraceType,
      circuitLayout :: Maybe Mappings,
      circuit :: Maybe ArithmeticCircuit,
      circuitMetrics :: Maybe CircuitMetrics,
      traceTypeMetrics :: Maybe TraceTypeMetrics
    }
  deriving Generic

