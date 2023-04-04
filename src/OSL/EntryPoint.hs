{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}

module OSL.EntryPoint
  ( main,
    runMain,
    FileName (..),
    TargetName (..),
    Output (..),
    CompileToCircuit (CompileToCircuit, DONTCompileToCircuit),
  )
where

import Control.Lens ((^.))
import Control.Monad.Trans.State.Strict (runStateT)
import Data.ByteString (readFile)
import Data.Either.Extra (mapLeft)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8')
import Halo2.CircuitMetrics (getCircuitMetrics)
import Halo2.Codegen (generateProject)
import Halo2.MungeLookupArguments (mungeLookupArguments)
import Halo2.Types.BitsPerByte (BitsPerByte (BitsPerByte))
import Halo2.Types.Circuit (ArithmeticCircuit)
import Halo2.Types.RowCount (RowCount (RowCount))
import Halo2.Types.TargetDirectory (TargetDirectory (TargetDirectory))
import OSL.ActusDictionary (actusDictionaryFormatted)
import OSL.BuildTranslationContext (buildTranslationContext)
import OSL.Parse (parseContext)
import OSL.Tokenize (tokenize)
import OSL.Translate (translateToFormula)
import OSL.TranslationContext (toLocalTranslationContext)
import OSL.Types.Stages (Stages (Stages))
import OSL.Types.FileName (FileName (FileName))
import OSL.Types.OSL (Declaration (Defined), Name (Sym))
import OSL.ValidContext (getDeclaration)
import OSL.ValidateContext (validateContext)
import Semicircuit.Gensyms (deBruijnToGensyms)
import Semicircuit.PNFFormula (toPNFFormula, toSemicircuit)
import Semicircuit.PrenexNormalForm (toPrenexNormalForm, toStrongPrenexNormalForm)
import Semicircuit.Sigma11 (prependQuantifiers)
import Semicircuit.ToLogicCircuit (semicircuitToLogicCircuit)
import System.Environment (getArgs)
import Trace.FromLogicCircuit (getMapping, logicCircuitToTraceType)
import Trace.Metrics (getTraceTypeMetrics)
import Trace.ToArithmeticAIR (mappings)
import Trace.ToArithmeticCircuit (traceTypeToArithmeticCircuit)
import Prelude hiding (readFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["actus-dictionary"] ->
      putStrLn . unOutput
        =<< genActusDictionary
    [fileName, targetName] ->
      putStrLn . unOutput
        =<< runMain (FileName fileName) (TargetName targetName) CompileToCircuit
    [fileName, targetName, "--test"] ->
      putStrLn . unOutput
        =<< runMain (FileName fileName) (TargetName targetName) DONTCompileToCircuit
    [fileName, targetName, "--output", outputDir] -> do
      sourceBs <- readFile fileName
      case decodeUtf8' sourceBs of
        Right source ->
          case toCircuit (FileName fileName) (TargetName targetName) (Source source) 8 81 of -- TODO: do not hard-code bits and row count
            Right c ->
              generateProject (TargetDirectory outputDir) c
            Left err ->
              putStrLn (unErrorMessage err)
        _ -> putStrLn "could not decode source file; is it not UTF-8?"
    _ -> do
      putStrLn "Usage: osl FILE NAME --output DIRECTORY"
      putStrLn "       osl FILE NAME [--test]"
      putStrLn "       osl actus-dictionary"

newtype TargetName = TargetName String

newtype ErrorMessage = ErrorMessage { unErrorMessage :: String }

newtype SuccessfulOutput = SuccessfulOutput String

newtype Source = Source Text

newtype Output = Output {unOutput :: String}
  deriving newtype (Eq, Show)

runMain :: FileName -> TargetName -> CompileToCircuit -> IO Output
runMain (FileName fileName) (TargetName targetName) compileToCircuit = do
  sourceBs <- readFile fileName
  case decodeUtf8' sourceBs of
    Right source ->
      case calcMain
        (FileName fileName)
        (TargetName targetName)
        (Source source) -- TODO: specify BitsPerByte and RowCount with options
        (BitsPerByte 8)
        (RowCount 81)
        compileToCircuit of
        Left (ErrorMessage err) -> pure (Output err)
        Right (SuccessfulOutput result) -> pure (Output result)
    _ -> pure (Output "could not decode source file; is it not UTF-8?")

data CompileToCircuit
  = CompileToCircuit
  | DONTCompileToCircuit

toCircuit ::
  FileName ->
  TargetName ->
  Source ->
  BitsPerByte ->
  RowCount ->
  Either ErrorMessage ArithmeticCircuit
toCircuit fileName targetName source bitsPerByte rowCount = do
  stages <- runCompilerStages fileName targetName source bitsPerByte
              rowCount CompileToCircuit
  case stages ^. #mungedCircuit of
    Just c -> pure c
    Nothing -> Left (ErrorMessage "no circuit produced")

runCompilerStages ::
  FileName ->
  TargetName ->
  Source ->
  BitsPerByte ->
  RowCount ->
  CompileToCircuit ->
  Either ErrorMessage Stages
runCompilerStages (FileName fileName) (TargetName targetName) (Source source) bitsPerByte rowCount compileToCircuit = do
  toks <-
    mapLeft (ErrorMessage . ("Tokenizing error: " <>) . show) $
      tokenize fileName source
  rawCtx <-
    mapLeft (ErrorMessage . ("Parse error: " <>) . show) $
      parseContext fileName toks
  validCtx <-
    mapLeft (ErrorMessage . ("Type checking error: " <>) . show) $
      validateContext rawCtx
  gc <-
    mapLeft (ErrorMessage . ("Error building context: " <>) . show) $
      buildTranslationContext validCtx
  let lc = toLocalTranslationContext gc
  case getDeclaration validCtx (Sym (pack targetName)) of
    Just (Defined _ targetTerm) -> do
      (translated, aux) <-
        mapLeft (ErrorMessage . ("Error translating: " <>) . show) $
          runStateT (translateToFormula gc lc targetTerm) mempty
      case compileToCircuit of
        DONTCompileToCircuit ->
          pure $ Stages translated aux Nothing Nothing Nothing
                   Nothing Nothing Nothing Nothing Nothing Nothing
                   Nothing Nothing Nothing Nothing
        CompileToCircuit -> do
          pnf <-
            mapLeft (ErrorMessage . ("Error converting to prenex normal form: " <>) . show) $
              toPrenexNormalForm () (fst (deBruijnToGensyms translated))
          spnf <-
            mapLeft (ErrorMessage . ("Error converting to strong prenex normal form: " <>) . show) $
              uncurry (toStrongPrenexNormalForm ()) pnf
          -- let sspnf = uncurry toSuperStrongPrenexNormalForm spnf
          pnff <-
            mapLeft (ErrorMessage . ("Error converting to PNF formula: " <>) . show) $
              toPNFFormula () (uncurry prependQuantifiers spnf)
          let semi = toSemicircuit pnff
              (logic, layout) = semicircuitToLogicCircuit rowCount semi
              traceLayout = getMapping 8 logic
              traceType = logicCircuitToTraceType bitsPerByte logic
              circuitLayout = mappings traceType traceLayout
              circuit = traceTypeToArithmeticCircuit traceType traceLayout
              mungedCircuit = mungeLookupArguments circuit
              circuitMetrics = getCircuitMetrics mungedCircuit
              traceTypeMetrics = getTraceTypeMetrics traceType
          pure $ Stages translated aux (Just pnf) (Just spnf) (Just pnff) (Just semi) (Just logic) (Just layout) (Just traceLayout) (Just traceType) (Just circuitLayout) (Just circuit) (Just mungedCircuit) (Just circuitMetrics) (Just traceTypeMetrics)
    _ -> Left . ErrorMessage $ "please provide the name of a defined term"

calcMain ::
  FileName ->
  TargetName ->
  Source ->
  BitsPerByte ->
  RowCount ->
  CompileToCircuit ->
  Either ErrorMessage SuccessfulOutput
calcMain fileName targetName source bitsPerByte rowCount compileToCircuit = do
  stages <- runCompilerStages fileName targetName source bitsPerByte rowCount compileToCircuit
  let translated = stages ^. #translation
      aux = stages ^. #auxTables
  let translatedOutput =
        "Translated OSL:\n"
          <> show translated
          <> (if aux == mempty then "" else "\n\nAux Data:\n" <> show aux)
  case compileToCircuit of
    DONTCompileToCircuit ->
      pure (SuccessfulOutput translatedOutput)
    CompileToCircuit ->
      pure . SuccessfulOutput $
        translatedOutput
          <> "\n\nTrace type metrics: "
          <> show (stages ^. #traceTypeMetrics)
          <> "\n\nCircuit metrics: "
          <> show (stages ^. #circuitMetrics)
          <> "\n\nPrenex normal form: "
          <> show (stages ^. #pnf)
          <> "\n\nStrong prenex normal form: "
          <> show (stages ^. #spnf)
          -- <> "\n\nSuper strong prenex normal form: "
          -- <> show sspnf
          <> "\n\nPNF formula: "
          <> show (stages ^. #pnff)
          <> "\n\nSemicircuit: "
          <> show (stages ^. #semi)
          <> "\n\nLayout: "
          <> show (stages ^. #layout)
          <> "\n\nLogic circuit: "
          <> show (stages ^. #logic)
          <> "\n\nTrace type layout: "
          <> show (stages ^. #traceLayout)
          <> "\n\nTrace type: "
          <> show (stages ^. #traceType)
          <> "\n\nArithmetic circuit layout:\n"
          <> show (stages ^. #layout)
          <> "\n\nArithmetic circuit:\n"
          <> show (stages ^. #circuit)

genActusDictionary :: IO Output
genActusDictionary =
  pure $ Output actusDictionaryFormatted
