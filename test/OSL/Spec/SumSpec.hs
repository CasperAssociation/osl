{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OSL.Spec.SumSpec (spec) where

import Control.Lens ((^.))
import qualified Data.Map as Map
import Halo2.Types.BitsPerByte (BitsPerByte)
import Halo2.Types.RowCount (RowCount)
import OSL.ArgumentForm (getArgumentForm)
import OSL.LoadContext (loadContext)
import OSL.Satisfaction (satisfiesSimple)
import OSL.SimplifyType (complexifyValueUnsafe, simplifyType)
import OSL.TranslatedEvaluation (evalTranslatedFormula10)
import OSL.Types.Argument (Argument (Argument), Statement (Statement), Witness (Witness))
import OSL.Types.ArgumentForm (ArgumentForm (ArgumentForm), StatementType (StatementType), WitnessType (WitnessType))
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

  describe "sum spec's semantics are preserved in codegen stage 10" $ do
    it "a positive case" $
      evalTranslatedFormula10 (1 :: RowCount) (8 :: BitsPerByte) c "sumIs" argumentForm (exampleArgument c)
        `shouldBe` Right ()

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
