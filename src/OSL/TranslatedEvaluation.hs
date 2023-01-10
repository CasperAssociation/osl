{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OSL.TranslatedEvaluation
  ( evalTranslatedFormula1,
    evalTranslatedFormula2,
    evalTranslatedFormula3,
    evalTranslatedFormula4,
    evalTranslatedFormula5,
    evalTranslatedFormula6,
  )
where

import Control.Lens ((^.))
import Data.Either.Extra (mapLeft)
import Halo2.Circuit (HasEvaluate (evaluate))
import Halo2.Types.RowCount (RowCount (RowCount))
import OSL.Argument (toSigma11Argument)
import qualified OSL.Sigma11 as S11
import OSL.Term (dropTermAnnotations)
import OSL.Translate (translateToFormulaSimple)
import OSL.Types.Argument (Argument)
import OSL.Types.ArgumentForm (ArgumentForm)
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))
import OSL.Types.OSL (Name, ValidContext)
import qualified OSL.Types.Sigma11.Argument as S11
import Semicircuit.Gensyms (deBruijnToGensyms, deBruijnToGensymsEvalContext)
import Semicircuit.PNFFormula (toPNFFormula, toSemicircuit)
import Semicircuit.PrenexNormalForm (statementToSuperStrongPrenexNormalForm, toPrenexNormalForm, toStrongPrenexNormalForm, toSuperStrongPrenexNormalForm, witnessToPrenexNormalForm, witnessToStrongPrenexNormalForm, witnessToSuperStrongPrenexNormalForm)
import Semicircuit.ToLogicCircuit (semicircuitArgumentToLogicCircuitArgument, semicircuitToLogicCircuit)

-- First codegen pass: OSL -> OSL.Sigma11
evalTranslatedFormula1 ::
  Show ann =>
  ValidContext t ann ->
  Name ->
  ArgumentForm ->
  Argument ->
  Either (ErrorMessage (Maybe ann)) Bool
evalTranslatedFormula1 c name argumentForm argument = do
  (def, translated, aux) <- translateToFormulaSimple c name
  ec <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("auxTablesToEvalContext: " <> msg))
      (S11.auxTablesToEvalContext aux)
  s11arg <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("toSigma11Argument: " <> msg))
      (toSigma11Argument c argumentForm argument (dropTermAnnotations def))
  mapLeft
    (\(ErrorMessage () msg) -> ErrorMessage Nothing ("evalFormula: " <> msg))
    (S11.evalFormula ec s11arg translated)

-- Second codegen pass: OSL.Sigma11 -> Semicircuit.Sigma11
-- (replaces de Bruijn indices with gensyms)
evalTranslatedFormula2 ::
  Show ann =>
  ValidContext t ann ->
  Name ->
  ArgumentForm ->
  Argument ->
  Either (ErrorMessage (Maybe ann)) Bool
evalTranslatedFormula2 c name argumentForm argument = do
  (def, translated, aux) <- translateToFormulaSimple c name
  let (translated', mapping) = deBruijnToGensyms translated
  ec <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("auxTablesToEvalContext: " <> msg))
      (S11.auxTablesToEvalContext aux)
  let ec' = deBruijnToGensymsEvalContext mapping ec
  s11arg <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("toSigma11Argument: " <> msg))
      (toSigma11Argument c argumentForm argument (dropTermAnnotations def))
  mapLeft
    (\(ErrorMessage () msg) -> ErrorMessage Nothing ("evalFormula: " <> msg))
    (S11.evalFormula ec' s11arg translated')

-- Third codegen pass: Semicircuit.Sigma11 -> Semicircuit.Sigma11
-- (prenex normal form conversion)
evalTranslatedFormula3 ::
  Show ann =>
  ValidContext t ann ->
  Name ->
  ArgumentForm ->
  Argument ->
  Either (ErrorMessage (Maybe ann)) Bool
evalTranslatedFormula3 c name argumentForm argument = do
  (def, translated, aux) <- translateToFormulaSimple c name
  let (translated', mapping) = deBruijnToGensyms translated
  translated'' <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("toPrenexNormalForm: " <> msg))
      (uncurry S11.prependQuantifiers <$> toPrenexNormalForm () translated')
  ec <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("auxTablesToEvalContext: " <> msg))
      (S11.auxTablesToEvalContext aux)
  let ec' = deBruijnToGensymsEvalContext mapping ec
  s11arg <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("toSigma11Argument: " <> msg))
      (toSigma11Argument c argumentForm argument (dropTermAnnotations def))
  s11witness' <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("witnessToPrenexNormalForm: " <> msg))
      (witnessToPrenexNormalForm translated' (s11arg ^. #witness))
  let s11arg' = S11.Argument (s11arg ^. #statement) s11witness'
  mapLeft
    ( \(ErrorMessage () msg) ->
        ErrorMessage
          Nothing
          ("evalFormula: " <> msg)
    )
    (S11.evalFormula ec' s11arg' translated'')

-- Fourth codegen pass: Semicircuit.Sigma11 -> Semicircuit.Sigma11
-- (strong prenex normal form conversion)
evalTranslatedFormula4 ::
  Show ann =>
  ValidContext t ann ->
  Name ->
  ArgumentForm ->
  Argument ->
  Either (ErrorMessage (Maybe ann)) Bool
evalTranslatedFormula4 c name argumentForm argument = do
  (def, translated, aux) <- translateToFormulaSimple c name
  let (translated', mapping) = deBruijnToGensyms translated
  (qs, qff) <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("toPrenexNormalForm: " <> msg))
      (toPrenexNormalForm () translated')
  translated'' <-
    uncurry S11.prependQuantifiers
      <$> mapLeft
        (\(ErrorMessage _ msg) -> ErrorMessage Nothing ("toStrongPrenexNormalForm: " <> msg))
        (toStrongPrenexNormalForm Nothing qs qff)
  ec <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("auxTablesToEvalContext: " <> msg))
      (S11.auxTablesToEvalContext aux)
  let ec' = deBruijnToGensymsEvalContext mapping ec
  s11arg <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("toSigma11Argument: " <> msg))
      (toSigma11Argument c argumentForm argument (dropTermAnnotations def))
  s11witness' <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("witnessToPrenexNormalForm: " <> msg))
      (witnessToPrenexNormalForm translated' (s11arg ^. #witness))
  s11witness'' <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("witnessToStrongPrenexNormalForm: " <> msg))
      (witnessToStrongPrenexNormalForm () mempty qs s11witness')
  let s11arg' = S11.Argument (s11arg ^. #statement) s11witness''
  mapLeft
    ( \(ErrorMessage () msg) ->
        ErrorMessage
          Nothing
          ("evalFormula: " <> msg)
    )
    (S11.evalFormula ec' s11arg' translated'')

-- Fifth codegen pass: Semicircuit.Sigma11 -> Semicircuit.Sigma11
-- (super strong prenex normal form conversion)
evalTranslatedFormula5 ::
  Show ann =>
  ValidContext t ann ->
  Name ->
  ArgumentForm ->
  Argument ->
  Either (ErrorMessage (Maybe ann)) Bool
evalTranslatedFormula5 c name argumentForm argument = do
  (def, translated, aux) <- translateToFormulaSimple c name
  let (translated', mapping) = deBruijnToGensyms translated
  (qs, qff) <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("toPrenexNormalForm: " <> msg))
      (toPrenexNormalForm () translated')
  (qs', qff') <-
    mapLeft
      (\(ErrorMessage _ msg) -> ErrorMessage Nothing ("toStrongPrenexNormalForm: " <> msg))
      (toStrongPrenexNormalForm Nothing qs qff)
  let (qs'', qff'') = toSuperStrongPrenexNormalForm qs' qff'
      translated''' = S11.prependQuantifiers qs'' qff''
  ec <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("auxTablesToEvalContext: " <> msg))
      (S11.auxTablesToEvalContext aux)
  let ec' = deBruijnToGensymsEvalContext mapping ec
  s11arg <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("toSigma11Argument: " <> msg))
      (toSigma11Argument c argumentForm argument (dropTermAnnotations def))
  s11witness' <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("witnessToPrenexNormalForm: " <> msg))
      (witnessToPrenexNormalForm translated' (s11arg ^. #witness))
  s11witness'' <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("witnessToStrongPrenexNormalForm: " <> msg))
      (witnessToStrongPrenexNormalForm () mempty qs s11witness')
  s11witness''' <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("witnessToSuperStrongPrenexNormalForm: " <> msg))
      (witnessToSuperStrongPrenexNormalForm () qs' s11witness'')
  let s11statement' = statementToSuperStrongPrenexNormalForm (s11arg ^. #statement)
      s11arg' = S11.Argument s11statement' s11witness'''
  mapLeft
    ( \(ErrorMessage () msg) ->
        ErrorMessage
          Nothing
          ("evalFormula: " <> msg)
    )
    (S11.evalFormula ec' s11arg' translated''')

-- Sixth codegen pass: Semicircuit.Sigma11 -> LogicCircuit
evalTranslatedFormula6 ::
  Show ann =>
  ValidContext t ann ->
  Name ->
  ArgumentForm ->
  Argument ->
  Either (ErrorMessage (Maybe ann)) Bool
evalTranslatedFormula6 c name argumentForm argument = do
  (def, translated, _aux) <- translateToFormulaSimple c name
  let (translated', _mapping) = deBruijnToGensyms translated
  (qs, qff) <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("toPrenexNormalForm: " <> msg))
      (toPrenexNormalForm () translated')
  (qs', qff') <-
    mapLeft
      (\(ErrorMessage _ msg) -> ErrorMessage Nothing ("toStrongPrenexNormalForm: " <> msg))
      (toStrongPrenexNormalForm Nothing qs qff)
  let (qs'', qff'') = toSuperStrongPrenexNormalForm qs' qff'
      translated''' = S11.prependQuantifiers qs'' qff''
  pnff <-
    mapLeft
      (\(ErrorMessage _ msg) -> ErrorMessage Nothing ("toPNFFormula: " <> msg))
      (toPNFFormula () translated''')
  let semi = toSemicircuit pnff
      rowCount = RowCount 2000 -- TODO: calculate or pass this in
      (logic, layout) = semicircuitToLogicCircuit rowCount semi
  s11arg <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("toSigma11Argument: " <> msg))
      (toSigma11Argument c argumentForm argument (dropTermAnnotations def))
  s11witness' <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("witnessToPrenexNormalForm: " <> msg))
      (witnessToPrenexNormalForm translated' (s11arg ^. #witness))
  s11witness'' <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("witnessToStrongPrenexNormalForm: " <> msg))
      (witnessToStrongPrenexNormalForm () mempty qs s11witness')
  s11witness''' <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("witnessToSuperStrongPrenexNormalForm: " <> msg))
      (witnessToSuperStrongPrenexNormalForm () qs' s11witness'')
  let s11statement' = statementToSuperStrongPrenexNormalForm (s11arg ^. #statement)
      s11arg' = S11.Argument s11statement' s11witness'''
  lcArg <-
    mapLeft
      (\(ErrorMessage () msg) -> ErrorMessage Nothing ("semicircuitArgumentToLogicCircuit: " <> msg))
      (semicircuitArgumentToLogicCircuitArgument () rowCount semi layout s11arg')
  mapLeft
    ( \(ErrorMessage () msg) ->
        ErrorMessage
          Nothing
          ("evaluate: " <> msg)
    )
    (evaluate () lcArg logic)
