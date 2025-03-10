{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module OSL.Spec.SudokuSpec (spec) where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Die (die)
import Halo2.Types.BitsPerByte (BitsPerByte)
import Halo2.Types.RowCount (RowCount)
import OSL.ArgumentForm (getArgumentForm)
import OSL.LoadContext (loadContext)
import OSL.Satisfaction (satisfiesSimple)
import OSL.SimplifyType (complexifyValueUnsafe, simplifyType)
import OSL.Spec.Sudoku.Types (Cell (Cell), Col (Col), Digit (Digit), Problem (Problem, unProblem), Row (Row), Solution (Solution, unSolution), Square (Square), SquareCell (SquareCell), SudokuWitness (SudokuWitness), X (X), Y (Y))
import OSL.TranslatedEvaluation (evalTranslatedFormula1, evalTranslatedFormula10, evalTranslatedFormula2, evalTranslatedFormula3, evalTranslatedFormula4, evalTranslatedFormula5, evalTranslatedFormula6, evalTranslatedFormula7, evalTranslatedFormula8, evalTranslatedFormula9)
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
  describe "sudoku" $ do
    mctx <- loadContext (FileName "examples/sudoku.osl")
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
    forM_ (Cell <$> ((,) <$> [0 .. 8] <*> [0 .. 8])) $
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
      evalTranslatedFormula6 (81 :: RowCount) c "problemIsSolvable" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula6 (81 :: RowCount) c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evaluate: \"quantifierFreeFormulaIsTrue\": not satisfied on the following rows: [(77,Just False)]")

  describe "Sudoku spec's semantics are preserved in codegen stage 7" $ do
    it "a positive case" $
      evalTranslatedFormula7 (81 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula7 (81 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evalTrace: polynomial constraint not satisfied: (\"assert\",1 + 28948022309329048855892746252171976963363056481941560715954676764349967630336x118,0^1,21912,SubexpressionTrace {value = 0, stepType = 84, adviceValues = fromList [(122,0),(123,0),(124,0)]},234,fromList [(31,77),(32,0),(33,0),(34,0),(35,0),(36,0),(37,0),(38,0),(39,0),(40,0),(41,0),(42,0),(43,0),(44,0),(45,0),(46,0),(47,0),(48,0),(49,0),(50,0),(51,0),(52,0),(53,0),(54,0),(55,0),(56,0),(57,0),(58,0),(59,0),(60,0),(61,0),(62,0),(63,0),(64,0),(65,0),(66,0),(67,0),(68,0),(69,0),(70,0),(71,0),(72,0),(73,0),(74,0),(75,0),(76,0),(77,0),(78,0),(79,0),(80,0),(81,0),(82,0),(83,0),(84,0),(85,0),(86,0),(87,0),(88,0),(89,0),(90,0),(91,0),(92,0),(93,0),(94,0),(95,0),(96,0),(97,0),(98,0),(99,0),(100,0),(101,0),(102,0),(103,0),(104,0),(105,0),(106,0),(107,0),(108,0),(109,0),(110,0),(111,0),(112,0),(113,0),(114,0),(115,0),(116,1),(117,0),(118,0),(119,0),(120,0),(121,0),(122,0),(123,0),(124,0)])")

  describe "Sudoku spec's semantics are preserved in codegen stage 8" $ do
    it "a positive case" $
      evalTranslatedFormula8 (81 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula8 (81 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evaluate: \"assert\": not satisfied on the following rows: [(21810,Just 1)] out of 22761")

  describe "Sudoku spec's semantics are preserved in codegen stage 9" $ do
    it "a positive case" $
      evalTranslatedFormula9 (81 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula9 (81 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evaluate: \"assert\": not satisfied on the following rows: [(21810,Just 1)] out of 22762")

  describe "Sudoku spec's semantics are preserved in codegen stage 10" $ do
    it "a positive case" $
      evalTranslatedFormula10 (81 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula10 (81 :: RowCount) (8 :: BitsPerByte) c "problemIsSolvable" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evaluate: \"assert\": not satisfied on the following rows: [(21810,Just 1)] out of 22762")

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

-- Arizona Daily Wildcat: Tuesday, Jan 17th, 2006
-- Source: https://sandiway.arizona.edu/sudoku/examples.html
-- Accessed 2022-12-13.
exampleProblemMap :: Map Cell Digit
exampleProblemMap =
  Map.fromList
    [ (Cell (0, 3), 1),
      (Cell (0, 4), 5),
      (Cell (0, 6), 6),
      (Cell (0, 8), 0),
      (Cell (1, 0), 5),
      (Cell (1, 1), 7),
      (Cell (1, 4), 6),
      (Cell (1, 7), 8),
      (Cell (2, 0), 0),
      (Cell (2, 1), 8),
      (Cell (2, 5), 3),
      (Cell (2, 6), 4),
      (Cell (3, 0), 7),
      (Cell (3, 1), 1),
      (Cell (3, 3), 0),
      (Cell (3, 7), 3),
      (Cell (4, 2), 3),
      (Cell (4, 3), 5),
      (Cell (4, 5), 1),
      (Cell (4, 6), 8),
      (Cell (5, 1), 4),
      (Cell (5, 5), 2),
      (Cell (5, 7), 1),
      (Cell (5, 8), 7),
      (Cell (6, 2), 8),
      (Cell (6, 3), 2),
      (Cell (6, 7), 6),
      (Cell (6, 8), 3),
      (Cell (7, 2), 7),
      (Cell (7, 4), 4),
      (Cell (7, 7), 2),
      (Cell (7, 8), 5),
      (Cell (8, 0), 6),
      (Cell (8, 2), 2),
      (Cell (8, 4), 0),
      (Cell (8, 5), 7)
    ]

unsoundExampleProblem :: Problem
unsoundExampleProblem =
  Problem $ \cell ->
    Map.lookup cell unsoundExampleProblemMap

unsoundExampleProblemMap :: Map Cell Digit
unsoundExampleProblemMap =
  Map.singleton (Cell (8, 5)) 6

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
      | (row, r) <- zip exampleSolutionMatrix [0 .. 8],
        (c, d) <- zip [0 .. 8] row
    ]

exampleSolutionMatrix :: [[Digit]]
exampleSolutionMatrix =
  [ [3, 2, 4, 1, 5, 8, 6, 7, 0],
    [5, 7, 1, 4, 6, 0, 3, 8, 2],
    [0, 8, 6, 7, 2, 3, 4, 5, 1],
    [7, 1, 5, 0, 8, 4, 2, 3, 6],
    [2, 6, 3, 5, 7, 1, 8, 0, 4],
    [8, 4, 0, 6, 3, 2, 5, 1, 7],
    [4, 0, 8, 2, 1, 5, 7, 6, 3],
    [1, 3, 7, 8, 4, 6, 0, 2, 5],
    [6, 5, 2, 3, 0, 7, 1, 4, 8]
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
    <*> getSquarePermutations s

getRowPermutations :: Solution -> Maybe (Map Row (Map Digit Col))
getRowPermutations s =
  mconcat
    <$> sequence
      [Map.singleton r <$> getRowPermutation s r | r <- [0 .. 8]]

getRowPermutation :: Solution -> Row -> Maybe (Map Digit Col)
getRowPermutation (Solution s) r =
  Map.fromList
    <$> sequence
      [ (v,) <$> find ((== v) . (\c -> s (Cell (r, c)))) [0 .. 8]
        | v <- [0 .. 8]
      ]

getColPermutations :: Solution -> Maybe (Map Col (Map Digit Row))
getColPermutations s =
  mconcat
    <$> sequence
      [Map.singleton c <$> getColPermutation s c | c <- [0 .. 8]]

getColPermutation :: Solution -> Col -> Maybe (Map Digit Row)
getColPermutation (Solution s) c =
  Map.fromList
    <$> sequence
      [ (v,) <$> find ((== v) . (\r -> s (Cell (r, c)))) [0 .. 8]
        | v <- [0 .. 8]
      ]

getSquarePermutations :: Solution -> Maybe (Map Square (Map Digit SquareCell))
getSquarePermutations s =
  mconcat
    <$> sequence
      [ Map.singleton sq <$> getSquarePermutation s sq
        | sq <- Square <$> ((,) <$> [0 .. 2] <*> [0 .. 2])
      ]

getSquarePermutation :: Solution -> Square -> Maybe (Map Digit SquareCell)
getSquarePermutation (Solution s) sq =
  Map.fromList
    <$> sequence
      [ (v,)
          <$> find
            ((== v) . s . getCell sq)
            (SquareCell <$> ((,) <$> [0 .. 2] <*> [0 .. 2]))
        | v <- [0 .. 8]
      ]

getCell :: Square -> SquareCell -> Cell
getCell (Square (X sx, Y sy)) (SquareCell (X x, Y y)) =
  Cell (Row (3 * sy + y), Col (3 * sx + x))

complexStatementType :: Type ()
complexStatementType =
  Product () (NamedType () (Sym "Problem")) (Fin () 1)

simpleStatementType :: Type ()
simpleStatementType = NamedType () (Sym "Problem")

problemToValue :: Problem -> Value
problemToValue (Problem p) =
  To' "Problem" . Fun . Map.fromList $
    [ (cellToValue c, Maybe'' (digitToValue <$> p c))
      | c <- Cell <$> ((,) <$> [0 .. 8] <*> [0 .. 8])
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

xToValue :: X -> Value
xToValue (X x) =
  maybe
    (die "xToValue: out of range")
    Fin'
    (integerToScalar x)

yToValue :: Y -> Value
yToValue (Y y) =
  maybe
    (die "yToValue: out of range")
    Fin'
    (integerToScalar y)

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
            ( F
                ()
                Nothing
                (NamedType () "SquareEncoded")
                ( F
                    ()
                    Nothing
                    (NamedType () "Digit")
                    ( Product
                        ()
                        (NamedType () "Square")
                        ( Product
                            ()
                            (NamedType () "SquareCell")
                            (Product () (Fin () 1) (Fin () 1))
                        )
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
        ( F
            ()
            Nothing
            (NamedType () "SquareEncoded")
            ( F
                ()
                Nothing
                (NamedType () "Digit")
                ( Product
                    ()
                    (NamedType () "Square")
                    (NamedType () "SquareCell")
                )
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
      | c <- Cell <$> ((,) <$> [0 .. 8] <*> [0 .. 8])
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

squarePermutationsToValue :: Map Square (Map Digit SquareCell) -> Value
squarePermutationsToValue =
  Fun . Map.mapKeys squareToEncodedValue . Map.mapWithKey squarePermutationToValue

squarePermutationToValue :: Square -> Map Digit SquareCell -> Value
squarePermutationToValue square =
  Fun . Map.mapKeys digitToValue . fmap (squareCellToValue square)

squareToValue :: Square -> Value
squareToValue (Square (x, y)) =
  To' "Square" (Pair' (yToValue y) (xToValue x))

squareToEncodedValue :: Square -> Value
squareToEncodedValue (Square (X x, Y y)) =
  To' "SquareEncoded" . Fin' $
    fromMaybe
      (die "squareToEncodedValue: out of range of scalar")
      (integerToScalar ((3 * y) + x))

squareCellToValue :: Square -> SquareCell -> Value
squareCellToValue s (SquareCell (x, y)) =
  Pair'
    (squareToValue s)
    (To' "SquareCell" (Pair' (yToValue y) (xToValue x)))

sudokuWitnessToValue :: SudokuWitness -> Value
sudokuWitnessToValue w =
  Pair'
    (solutionToValue (w ^. #solution))
    ( Pair'
        ( Pair'
            (rowPermutationsToValue (w ^. #rowPermutations))
            (colPermutationsToValue (w ^. #colPermutations))
        )
        (squarePermutationsToValue (w ^. #squarePermutations))
    )
