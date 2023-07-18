{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module OSL.Spec.MiniSudokuSpec (spec) where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.Trans.Except (runExceptT)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Die (die)
import Halo2.Types.BitsPerByte (BitsPerByte)
import Halo2.Types.RowCount (RowCount)
import OSL.ArgumentForm (getArgumentForm)
import OSL.LoadContext (loadContext)
import OSL.Satisfaction (satisfiesSimple)
import OSL.SimplifyType (complexifyValueUnsafe, simplifyType)
import OSL.Spec.Sudoku.Types (Cell (Cell), Col (Col), Digit (Digit), Problem (Problem, unProblem), Row (Row), Solution (Solution, unSolution), SudokuWitness (SudokuWitness))
import OSL.TranslatedEvaluation (evalTranslatedFormula1, evalTranslatedFormula2, evalTranslatedFormula3, evalTranslatedFormula4, evalTranslatedFormula5, evalTranslatedFormula6, evalTranslatedFormula7, evalTranslatedFormula8, evalTranslatedFormula9, evalTranslatedFormula10, evalTranslatedFormula11)
import OSL.Types.Argument (Argument (Argument), Statement (Statement), Witness (Witness))
import OSL.Types.ArgumentForm (ArgumentForm (ArgumentForm), StatementType (StatementType), WitnessType (WitnessType))
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))
import OSL.Types.FileName (FileName (FileName))
import OSL.Types.OSL (ContextType (Global), Declaration (Defined), Name (Sym), Type (F, Fin, NamedType, Product), ValidContext)
import OSL.Types.Value (Value (Fin', Fun, Maybe'', Pair', To'))
import OSL.ValidContext (getNamedTermUnsafe)
import Stark.Types.Scalar (integerToScalar)
import Test.Syd (Spec, describe, expectationFailure, it, liftIO, shouldBe)
import Text.Parsec (SourcePos)

spec :: Spec
spec =
  describe "mini-sudoku" $ do
    mctx <- loadContext (FileName "examples/mini-sudoku.osl")
    case mctx of
      Left err -> liftIO . expectationFailure $ show err
      Right c -> spec' c

spec' :: ValidContext 'Global SourcePos -> Spec
spec' c = do
  describe "argument form" $ argumentFormSpec c
  describe "example" $ exampleSpec c

argumentFormSpec :: ValidContext 'Global SourcePos -> Spec
argumentFormSpec c = do
  it "has the correct argument form" $
    case Map.lookup
      (Sym "problemIsSolvable")
      (c ^. #unValidContext) of
      Just (Defined t x) -> do
        getArgumentForm c t x
          `shouldBe` Right
            ( ArgumentForm
                (StatementType complexStatementType)
                (WitnessType complexWitnessType)
            )
      _ ->
        liftIO . expectationFailure $
          "problemIsSolvable definition not found"

  it "has the correct simplified statement type" $
    simplifyType complexStatementType `shouldBe` Just simpleStatementType

  it "has the correct simplified witness type" $
    simplifyType complexWitnessType `shouldBe` Just simpleWitnessType

exampleSpec :: ValidContext 'Global SourcePos -> Spec
exampleSpec c = do
  it "example problem matches example solution" $
    forM_ (Cell <$> ((,) <$> [0 .. 2] <*> [0 .. 2])) $
      \cell ->
        case unProblem exampleProblem cell of
          Nothing -> pure ()
          Just v -> (cell, v) `shouldBe` (cell, unSolution exampleSolution cell)

  it "Sudoku spec is satisfied on a true example" $
    satisfiesSimple
      c
      (getNamedTermUnsafe c "problemIsSolvable")
      (exampleArgument c)
      `shouldBe` Right True

  it "Sudoku spec is unsatisfied on a false example" $
    satisfiesSimple
      c
      (getNamedTermUnsafe c "problemIsSolvable")
      (exampleUnsoundArgument c)
      `shouldBe` Right False

  it "Sudoku spec's semantics are preserved in codegen stage 1" $ do
    evalTranslatedFormula1 c "problemIsSolvable" argumentForm (exampleArgument c)
      `shouldBe` Right True

    evalTranslatedFormula1 c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
      `shouldBe` Right False

  it "Sudoku spec's semantics are preserved in codegen stage 2" $ do
    evalTranslatedFormula2 c "problemIsSolvable" argumentForm (exampleArgument c)
      `shouldBe` Right True

    evalTranslatedFormula2 c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
      `shouldBe` Right False

  it "Sudoku spec's semantics are preserved in codegen stage 3" $ do
    evalTranslatedFormula3 c "problemIsSolvable" argumentForm (exampleArgument c)
      `shouldBe` Right True

    evalTranslatedFormula3 c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
      `shouldBe` Right False

  it "Sudoku spec's semantics are preserved in codegen stage 4" $ do
    evalTranslatedFormula4 c "problemIsSolvable" argumentForm (exampleArgument c)
      `shouldBe` Right True

    evalTranslatedFormula4 c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
      `shouldBe` Right False

  it "Sudoku spec's semantics are preserved in codegen stage 5" $ do
    evalTranslatedFormula5 c "problemIsSolvable" argumentForm (exampleArgument c)
      `shouldBe` Right True

    evalTranslatedFormula5 c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
      `shouldBe` Right False

  describe "Sudoku spec's semantics are preserved in codegen stage 6" $ do
    it "a positive case" $
      evalTranslatedFormula6 (9 :: RowCount) c "problemIsSolvable" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula6 (9 :: RowCount) c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evaluate: \"quantifierFreeFormulaIsTrue\": not satisfied on the following rows: [(0,Just False)]")

  describe "Sudoku spec's semantics are preserved in codegen stage 7" $ do
    it "a positive case" $
      evalTranslatedFormula7 (9 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula7 (9 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evalTrace: polynomial constraint not satisfied: (\"assert\",1 + 28948022309329048855892746252171976963363056481941560715954676764349967630336x77,0^1,0,SubexpressionTrace {value = 0, stepType = 55, adviceValues = fromList [(81,0),(82,0),(83,0)]},134,fromList [(19,0),(20,0),(21,0),(22,0),(23,0),(24,0),(25,0),(26,0),(27,0),(28,0),(29,0),(30,0),(31,0),(32,0),(33,0),(34,0),(35,0),(36,0),(37,0),(38,0),(39,0),(40,0),(41,0),(42,0),(43,0),(44,0),(45,0),(46,0),(47,0),(48,0),(49,0),(50,0),(51,0),(52,0),(53,0),(54,0),(55,0),(56,0),(57,0),(58,0),(59,0),(60,0),(61,0),(62,0),(63,0),(64,0),(65,0),(66,0),(67,0),(68,0),(69,0),(70,0),(71,0),(72,0),(73,0),(74,0),(75,1),(76,0),(77,0),(78,0),(79,0),(80,0),(81,0),(82,0),(83,0)])")

  describe "Sudoku spec's semantics are preserved in codegen stage 8" $ do
    it "a positive case" $
      evalTranslatedFormula8 (9 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula8 (9 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evaluate: \"assert\": not satisfied on the following rows: [(104,Just 1)] out of 1530")

  describe "Sudoku spec's semantics are preserved in codegen stage 9" $ do
    it "a positive case" $
      evalTranslatedFormula9 (9 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula9 (9 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evaluate: \"assert\": not satisfied on the following rows: [(104,Just 1)] out of 1531")

  describe "Sudoku spec's semantics are preserved in codegen stage 10" $ do
    it "a positive case" $
      evalTranslatedFormula10 (9 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula10 (9 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evaluate: \"assert\": not satisfied on the following rows: [(104,Just 1)] out of 1531")

  describe "Sudoku spec's semantics are preserved in codegen stage 11" $ do
    it "a positive case" $ do
      result <- runExceptT $ evalTranslatedFormula11 (9 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleArgument c)
      result `shouldBe` Right ()

--     TODO: enable the negative case
--     it "a negative case" $ do
--       result <- liftIO . runExceptT $ evalTranslatedFormula11 (9 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
--       result `shouldBe` Left (ErrorMessage Nothing "mockProve: mock prover returned error: ConnectionError (HttpExceptionRequest Request {\n  host                 = \"127.0.0.1\"\n  port                 = 1727\n  secure               = False\n  requestHeaders       = [(\"Accept\",\"text/plain;charset=utf-8\"),(\"Content-Type\",\"application/json;charset=utf-8\")]\n  path                 = \"/mock_prove\"\n  queryString          = \"\"\n  method               = \"POST\"\n  proxy                = Nothing\n  rawBody              = False\n  redirectCount        = 10\n  responseTimeout      = ResponseTimeoutDefault\n  requestVersion       = HTTP/1.1\n  proxySecureMode      = ProxySecureWithConnect\n}\n NoResponseDataReceived)")



exampleArgument :: ValidContext 'Global ann -> Argument
exampleArgument c =
  Argument
    ( Statement
        ( complexifyValueUnsafe
            c
            complexStatementType
            (problemToValue exampleProblem)
        )
    )
    ( Witness
        ( complexifyValueUnsafe
            c
            complexWitnessType
            (sudokuWitnessToValue exampleWitness)
        )
    )

exampleUnsoundArgument :: ValidContext 'Global ann -> Argument
exampleUnsoundArgument c =
  Argument
    ( Statement
        ( complexifyValueUnsafe
            c
            complexStatementType
            (problemToValue unsoundExampleProblem)
        )
    )
    ( Witness
        ( complexifyValueUnsafe
            c
            complexWitnessType
            (sudokuWitnessToValue exampleWitness)
        )
    )

exampleProblem :: Problem
exampleProblem =
  Problem $ \cell ->
    Map.lookup cell exampleProblemMap

exampleProblemMap :: Map Cell Digit
exampleProblemMap =
  Map.fromList
    [ (Cell (0, 0), 0),
      (Cell (0, 2), 1),
      (Cell (1, 1), 0),
      (Cell (1, 2), 2),
      (Cell (2, 1), 1),
      (Cell (2, 2), 0)
    ]

unsoundExampleProblem :: Problem
unsoundExampleProblem =
  Problem $ \cell ->
    Map.lookup cell unsoundExampleProblemMap

unsoundExampleProblemMap :: Map Cell Digit
unsoundExampleProblemMap =
  Map.singleton (Cell (0, 0)) 1

exampleSolution :: Solution
exampleSolution =
  Solution $ \cell ->
    case Map.lookup cell exampleSolutionMap of
      Just d -> d
      Nothing -> die "exampleSolution: partiality"

exampleSolutionMap :: Map Cell Digit
exampleSolutionMap =
  Map.fromList
    [ (Cell (Row r, Col c), d)
      | (row, r) <- zip exampleSolutionMatrix [0 .. 2],
        (c, d) <- zip [0 .. 2] row
    ]

exampleSolutionMatrix :: [[Digit]]
exampleSolutionMatrix =
  [ [0, 2, 1],
    [1, 0, 2],
    [2, 1, 0]
  ]

exampleWitness :: SudokuWitness
exampleWitness =
  case createWitness exampleSolution of
    Just w -> w
    Nothing -> die "exampleWitness: failed to create a witness"

createWitness :: Solution -> Maybe SudokuWitness
createWitness s =
  SudokuWitness s
    <$> getRowPermutations s
    <*> getColPermutations s
    <*> pure mempty

getRowPermutations :: Solution -> Maybe (Map Row (Map Digit Col))
getRowPermutations s =
  mconcat
    <$> sequence
      [Map.singleton r <$> getRowPermutation s r | r <- [0 .. 2]]

getRowPermutation :: Solution -> Row -> Maybe (Map Digit Col)
getRowPermutation (Solution s) r =
  Map.fromList
    <$> sequence
      [ (v,) <$> find ((== v) . (\c -> s (Cell (r, c)))) [0 .. 2]
        | v <- [0 .. 2]
      ]

getColPermutations :: Solution -> Maybe (Map Col (Map Digit Row))
getColPermutations s =
  mconcat
    <$> sequence
      [Map.singleton c <$> getColPermutation s c | c <- [0 .. 2]]

getColPermutation :: Solution -> Col -> Maybe (Map Digit Row)
getColPermutation (Solution s) c =
  Map.fromList
    <$> sequence
      [ (v,) <$> find ((== v) . (\r -> s (Cell (r, c)))) [0 .. 2]
        | v <- [0 .. 2]
      ]

complexStatementType :: Type ()
complexStatementType =
  Product () (NamedType () (Sym "Problem")) (Fin () 1)

simpleStatementType :: Type ()
simpleStatementType = NamedType () (Sym "Problem")

problemToValue :: Problem -> Value
problemToValue (Problem p) =
  To' "Problem" . Fun . Map.fromList $
    [ (cellToValue c, Maybe'' (digitToValue <$> p c))
      | c <- Cell <$> ((,) <$> [0 .. 2] <*> [0 .. 2])
    ]

digitToValue :: Digit -> Value
digitToValue (Digit d) =
  maybe
    (die "digitToValue: out of range")
    (To' "Digit" . Fin')
    (integerToScalar d)

cellToValue :: Cell -> Value
cellToValue (Cell (r, c)) =
  To' "Cell" (Pair' (rowToValue r) (colToValue c))

rowToValue :: Row -> Value
rowToValue (Row r) =
  maybe
    (die "rowToValue: out of range")
    (To' "Row" . Fin')
    (integerToScalar r)

colToValue :: Col -> Value
colToValue (Col c) =
  maybe
    (die "colToValue: out of range")
    (To' "Col" . Fin')
    (integerToScalar c)

complexWitnessType :: Type ()
complexWitnessType =
  Product
    ()
    (NamedType () "Solution")
    ( Product
        ()
        ( F
            ()
            Nothing
            (NamedType () "Cell")
            (Product () (Fin () 1) (Fin () 1))
        )
        ( Product
            ()
            ( F
                ()
                Nothing
                (NamedType () "Row")
                ( F
                    ()
                    Nothing
                    (NamedType () "Digit")
                    ( Product
                        ()
                        (NamedType () "Col")
                        (Fin () 1)
                    )
                )
            )
            ( F
                ()
                Nothing
                (NamedType () "Col")
                ( F
                    ()
                    Nothing
                    (NamedType () "Digit")
                    ( Product
                        ()
                        (NamedType () "Row")
                        (Fin () 1)
                    )
                )
            )
        )
    )

simpleWitnessType :: Type ()
simpleWitnessType =
  Product
    ()
    (NamedType () "Solution")
    ( Product
        ()
        ( F
            ()
            Nothing
            (NamedType () "Row")
            ( F
                ()
                Nothing
                (NamedType () "Digit")
                (NamedType () "Col")
            )
        )
        ( F
            ()
            Nothing
            (NamedType () "Col")
            ( F
                ()
                Nothing
                (NamedType () "Digit")
                (NamedType () "Row")
            )
        )
    )

argumentForm :: ArgumentForm
argumentForm =
  ArgumentForm
    (StatementType complexStatementType)
    (WitnessType complexWitnessType)

solutionToValue :: Solution -> Value
solutionToValue (Solution s) =
  To' "Solution" . Fun . Map.fromList $
    [ (cellToValue c, digitToValue (s c))
      | c <- Cell <$> ((,) <$> [0 .. 2] <*> [0 .. 2])
    ]

rowPermutationsToValue :: Map Row (Map Digit Col) -> Value
rowPermutationsToValue =
  Fun . Map.mapKeys rowToValue . fmap rowPermutationToValue

rowPermutationToValue :: Map Digit Col -> Value
rowPermutationToValue =
  Fun . Map.mapKeys digitToValue . fmap colToValue

colPermutationsToValue :: Map Col (Map Digit Row) -> Value
colPermutationsToValue =
  Fun . Map.mapKeys colToValue . fmap colPermutationToValue

colPermutationToValue :: Map Digit Row -> Value
colPermutationToValue =
  Fun . Map.mapKeys digitToValue . fmap rowToValue

sudokuWitnessToValue :: SudokuWitness -> Value
sudokuWitnessToValue w =
  Pair'
    (solutionToValue (w ^. #solution))
    ( Pair'
        (rowPermutationsToValue (w ^. #rowPermutations))
        (colPermutationsToValue (w ^. #colPermutations))
    )
