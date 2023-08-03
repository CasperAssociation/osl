{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OSL.Spec.SumSpec (spec) where

import Control.Lens ((^.))
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Map as Map
import Halo2.ProverClient (Port (Port))
import Halo2.Types.BitsPerByte (BitsPerByte)
import Halo2.Types.RowCount (RowCount)
import OSL.ArgumentForm (getArgumentForm)
import OSL.LoadContext (loadContext)
import OSL.Satisfaction (satisfiesSimple)
import OSL.SimplifyType (complexifyValueUnsafe, simplifyType)
import OSL.TranslatedEvaluation (evalTranslatedFormula3, evalTranslatedFormula4, evalTranslatedFormula6, evalTranslatedFormula7, evalTranslatedFormula8, evalTranslatedFormula9, evalTranslatedFormula10, evalTranslatedFormula11)
import OSL.Types.Argument (Argument (Argument), Statement (Statement), Witness (Witness))
import OSL.Types.ArgumentForm (ArgumentForm (ArgumentForm), StatementType (StatementType), WitnessType (WitnessType))
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))
import OSL.Types.FileName (FileName (FileName))
import OSL.Types.OSL (ContextType (Global), Declaration (Defined), Name (Sym), Type (N, Fin, Product), ValidContext)
import OSL.Types.Value (Value (Nat, Pair', Fin'))
import OSL.ValidContext (getNamedTermUnsafe)
import Stark.Types.Scalar (Scalar)
import Test.Syd (Spec, describe, expectationFailure, it, liftIO, shouldBe)
import Text.Parsec (SourcePos)

spec :: Spec
spec =
  describe "sum" $ do
    mctx <- loadContext (FileName "examples/sum.osl")
    case mctx of
      Left err -> liftIO . expectationFailure $ show err
      Right c -> spec' c

spec' :: ValidContext Global SourcePos -> Spec
spec' c = do
  describe "argument form" $ argumentFormSpec c
  describe "example" $ exampleSpec c

argumentFormSpec :: ValidContext Global SourcePos -> Spec
argumentFormSpec c = do
  it "has the correct argument form" $
    case Map.lookup (Sym "sumIs") (c ^. #unValidContext) of
      Just (Defined t x) -> do
        getArgumentForm c t x
          `shouldBe` Right
            ( ArgumentForm
                (StatementType complexStatementType)
                (WitnessType complexWitnessType)
            )
      _ ->
        liftIO . expectationFailure $
          "sumIs definition not found"

  it "has the correct simplified statement type" $
    simplifyType complexStatementType `shouldBe` Just simpleStatementType

  it "has the correct simplified witness type" $
    simplifyType complexWitnessType `shouldBe` Nothing

exampleSpec :: ValidContext Global SourcePos -> Spec
exampleSpec c = do
  it "sum spec is satisfied on a true example" $
    satisfiesSimple
      c
      (getNamedTermUnsafe c "sumIs")
      (exampleArgument c)
      `shouldBe` Right True

  it "sum spec is unsatisfied on a false example" $
    satisfiesSimple
      c
      (getNamedTermUnsafe c "sumIs")
      (exampleUnsoundArgument c)
      `shouldBe` Right False

  describe "sum spec's semantics are preserved in codegen stage 3" $ do
    it "a positive case" $
      evalTranslatedFormula3 c "sumIs" argumentForm (exampleArgument c)
        `shouldBe` Right True

    it "a negative case" $
      evalTranslatedFormula3 c "sumIs" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Right False 

  describe "sum spec's semantics are preserved in codegen stage 4" $ do
    it "a positive case" $
      evalTranslatedFormula4 c "sumIs" argumentForm (exampleArgument c)
        `shouldBe` Right True

    it "a negative case" $
      evalTranslatedFormula4 c "sumIs" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Right False 

  describe "sum spec's semantics are preserved in codegen stage 6" $ do
    it "a positive case" $
      evalTranslatedFormula6 (1 :: RowCount) c "sumIs" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula6 (1 :: RowCount) c "sumIs" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evaluate: \"quantifierFreeFormulaIsTrue\": not satisfied on the following rows: [(0,Just False)]")

  describe "sum spec's semantics are preserved in codegen stage 7" $ do
    it "a positive case" $
      evalTranslatedFormula7 (1 :: RowCount) (8 :: BitsPerByte) c "sumIs" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula7 (1 :: RowCount) (8 :: BitsPerByte) c "sumIs" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evalTrace: polynomial constraint not satisfied: (\"assert\",1 + 28948022309329048855892746252171976963363056481941560715954676764349967630336x29,0^1,0,SubexpressionTrace {value = 0, stepType = 21, adviceValues = fromList [(32,0),(33,0),(34,0)]},27,fromList [(5,0),(6,0),(7,0),(8,0),(9,0),(10,0),(11,0),(12,0),(13,0),(14,0),(15,0),(16,0),(17,0),(18,0),(19,0),(20,0),(21,0),(22,0),(23,0),(24,0),(25,0),(26,0),(27,1),(28,0),(29,0),(30,0),(31,0),(32,0),(33,0),(34,0)])")

  describe "sum spec's semantics are preserved in codegen stage 8" $ do
    it "a positive case" $
      evalTranslatedFormula8 (1 :: RowCount) (8 :: BitsPerByte) c "sumIs" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula8 (1 :: RowCount) (8 :: BitsPerByte) c "sumIs" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evaluate: \"assert\": not satisfied on the following rows: [(19,Just 1)] out of 256")

  describe "sum spec's semantics are preserved in codegen stage 9" $ do
    it "a positive case" $
      evalTranslatedFormula9 (1 :: RowCount) (8 :: BitsPerByte) c "sumIs" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula9 (1 :: RowCount) (8 :: BitsPerByte) c "sumIs" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evaluate: \"assert\": not satisfied on the following rows: [(19,Just 1)] out of 257")

  describe "sum spec's semantics are preserved in codegen stage 10" $ do
    it "a positive case" $
      evalTranslatedFormula10 (1 :: RowCount) (8 :: BitsPerByte) c "sumIs" argumentForm (exampleArgument c)
        `shouldBe` Right ()

    it "a negative case" $
      evalTranslatedFormula10 (1 :: RowCount) (8 :: BitsPerByte) c "sumIs" argumentForm (exampleUnsoundArgument c)
        `shouldBe` Left (ErrorMessage Nothing "evaluate: \"assert\": not satisfied on the following rows: [(19,Just 1)] out of 257")

  describe "sum spec's semantics are preserved in codegen stage 11" $ do
    it "a positive case" $ do
      result <- runExceptT $ evalTranslatedFormula11 "./mock-prover-3" (Port 1789) (1 :: RowCount) (8 :: BitsPerByte) c "sumIs" argumentForm (exampleArgument c)
      result `shouldBe` Right ()

    it "a negative case" $ do
      result <- runExceptT $ evalTranslatedFormula11 "./mock-prover-4" (Port 1790) (1 :: RowCount) (8 :: BitsPerByte) c "sumIs" argumentForm (exampleUnsoundArgument c)
      result `shouldBe` Left (ErrorMessage Nothing "mockProve: mock prover returned error: ConnectionError (HttpExceptionRequest Request {\n  host                 = \"127.0.0.1\"\n  port                 = 1790\n  secure               = False\n  requestHeaders       = [(\"Accept\",\"text/plain;charset=utf-8\"),(\"Content-Type\",\"application/json;charset=utf-8\")]\n  path                 = \"/mock_prove\"\n  queryString          = \"\"\n  method               = \"POST\"\n  proxy                = Nothing\n  rawBody              = False\n  redirectCount        = 10\n  responseTimeout      = ResponseTimeoutDefault\n  requestVersion       = HTTP/1.1\n  proxySecureMode      = ProxySecureWithConnect\n}\n NoResponseDataReceived)")


complexStatementType :: Type ()
complexStatementType =
  Product ()
    (N ())
    (Product ()
      (N ())
      (Product () (N ()) (Fin () 1)))

complexWitnessType :: Type ()
complexWitnessType = Fin () 1

simpleStatementType :: Type ()
simpleStatementType = Product () (N ()) (Product () (N ()) (N ()))

argumentForm :: ArgumentForm
argumentForm =
  ArgumentForm
    (StatementType complexStatementType)
    (WitnessType complexWitnessType)

exampleArgument :: ValidContext Global ann -> Argument
exampleArgument c =
  Argument
    ( Statement
        ( complexifyValueUnsafe
            c
            complexStatementType
            (tripleToValue (1, 1, 2))
        )
    )
    ( Witness (Fin' 0) )

exampleUnsoundArgument :: ValidContext Global ann -> Argument
exampleUnsoundArgument c =
  Argument
    ( Statement
        ( complexifyValueUnsafe
            c
            complexStatementType
            (tripleToValue (1, 1, 3))
        )
    )
    ( Witness (Fin' 0) )

tripleToValue :: (Scalar, Scalar, Scalar) -> Value
tripleToValue (a, b, c) = Pair' (Nat a) (Pair' (Nat b) (Nat c))
