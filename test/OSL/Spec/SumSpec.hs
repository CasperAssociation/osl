{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OSL.Spec.SumSpec (spec) where

import Control.Lens ((^.))
import qualified Data.Map as Map
import OSL.ArgumentForm (getArgumentForm)
import OSL.LoadContext (loadContext)
import OSL.SimplifyType (simplifyType)
-- import OSL.Types.Argument (Argument (Argument), Statement (Statement), Witness (Witness))
import OSL.Types.ArgumentForm (ArgumentForm (ArgumentForm), StatementType (StatementType), WitnessType (WitnessType))
import OSL.Types.FileName (FileName (FileName))
import OSL.Types.OSL (ContextType (Global), Declaration (Defined), Name (Sym), Type (N, Fin, Product), ValidContext)
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

argumentFormSpec :: ValidContext 'Global SourcePos -> Spec
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
