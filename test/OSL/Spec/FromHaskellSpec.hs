{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OSL.Spec.FromHaskellSpec (spec) where

import Control.Monad.State (State, execState)
import Data.Fixed (Fixed, Pico)
import qualified Data.Map as Map
import Data.Proxy (Proxy (Proxy))
import Data.Time (Day, LocalTime, TimeOfDay)
import GHC.Generics (Generic)
import OSL.FromHaskell (AddToOSLContext, Newtype, ToOSLType (toOSLType), addToOSLContextM, mkDataToAddOSL, mkDataToOSL)
import OSL.Spec.Sudoku.Types (Cell, Col, Digit, Problem, Row, Solution, Square, SquareCell, X, Y)
import qualified OSL.Types.OSL as OSL
import Test.Syd (Spec, describe, it, shouldBe)

mkDataToOSL "TimeOfDay"
mkDataToOSL "LocalTime"

-- Deriving Generic is present on these type definitions only to suppress
-- -Wunused-top-binds
data Record2 = Record2 Int Int
  deriving (Generic)

mkDataToOSL "Record2"

data Record3 = Record3 Int Int Int
  deriving (Generic)

mkDataToOSL "Record3"

data Record4 = Record4 Int Int Int Int
  deriving (Generic)

mkDataToOSL "Record4"

data Enum3 = A | B | C
  deriving (Generic)

mkDataToAddOSL "Enum3"

spec :: Spec
spec =
  describe "FromHaskell" $ do
    unitType
    unitProductType
    unitSumType
    integerType
    fixedType
    dayType
    picoType
    record2Type
    record3Type
    record4Type
    timeOfDayType
    localTimeType
    enumType
    sudokuTypes

unitType :: Spec
unitType =
  it "() -> Fin(1)" $
    toOSLType (Proxy @()) mempty
      `shouldBe` OSL.Fin () 1

unitProductType :: Spec
unitProductType =
  it "((), ()) -> Fin(1) * Fin(1)" $
    toOSLType (Proxy @((), ())) mempty
      `shouldBe` OSL.Product () (OSL.Fin () 1) (OSL.Fin () 1)

unitSumType :: Spec
unitSumType =
  it "Either () () -> Fin(1) + Fin(1)" $
    toOSLType (Proxy @(Either () ())) mempty
      `shouldBe` OSL.Coproduct () (OSL.Fin () 1) (OSL.Fin () 1)

integerType :: Spec
integerType =
  it "Integer -> Z" $
    toOSLType (Proxy @Integer) mempty
      `shouldBe` OSL.Z ()

fixedType :: Spec
fixedType =
  it "Fixed 4 -> Fin(4)" $
    toOSLType (Proxy @(Fixed 4)) mempty
      `shouldBe` OSL.Fin () 4

dayType :: Spec
dayType =
  it "Day -> Z" $
    toOSLType (Proxy @Day) mempty
      `shouldBe` OSL.Z ()

picoType :: Spec
picoType =
  it "Pico -> Fin(1)" $
    toOSLType (Proxy @Pico) mempty
      `shouldBe` OSL.Fin () 1000000000000

record2Type :: Spec
record2Type =
  it "Record2 -> Z * Z" $
    toOSLType (Proxy @Record2) mempty
      `shouldBe` OSL.Product () (OSL.Z ()) (OSL.Z ())

record3Type :: Spec
record3Type =
  it "Record3 -> Z * Z * Z" $
    toOSLType (Proxy @Record3) mempty
      `shouldBe` OSL.Product () (OSL.Product () (OSL.Z ()) (OSL.Z ())) (OSL.Z ())

record4Type :: Spec
record4Type =
  it "Record4 -> Z * Z * Z * Z" $
    toOSLType (Proxy @Record4) mempty
      `shouldBe` OSL.Product () (OSL.Product () (OSL.Product () (OSL.Z ()) (OSL.Z ())) (OSL.Z ())) (OSL.Z ())

timeOfDayType :: Spec
timeOfDayType =
  it "TimeOfDay -> _" $
    toOSLType (Proxy @TimeOfDay) mempty
      `shouldBe` OSL.Product
        ()
        ( OSL.Product
            ()
            (OSL.Z ())
            (OSL.Z ())
        )
        (OSL.Fin () 1000000000000)

localTimeType :: Spec
localTimeType =
  it "LocalTime -> _" $
    toOSLType (Proxy @LocalTime) mempty
      `shouldBe` OSL.Product
        ()
        (OSL.Z ())
        ( OSL.Product
            ()
            ( OSL.Product
                ()
                (OSL.Z ())
                (OSL.Z ())
            )
            (OSL.Fin () 1000000000000)
        )

enumType :: Spec
enumType =
  it "correctly assembles an enum type context " $
    enumTypesContext `shouldBe` expectedContext
  where
    enumTypesContext = execState (add (Proxy @Enum3)) mempty

    add = addToOSLContextM

    expectedContext =
      OSL.ValidContext . Map.fromList $
        [ ("A", OSL.Data (OSL.Fin () 1)),
          ("B", OSL.Data (OSL.Fin () 1)),
          ("C", OSL.Data (OSL.Fin () 1)),
          ( "Enum3",
            OSL.Data
              ( OSL.Coproduct
                  ()
                  ( OSL.Coproduct
                      ()
                      (OSL.NamedType () "A")
                      (OSL.NamedType () "B")
                  )
                  (OSL.NamedType () "C")
              )
          ),
          ( "A_inj",
            OSL.Defined
              (OSL.F () Nothing
                (OSL.NamedType () "A")
                (OSL.NamedType () "Enum3"))
              (OSL.Lambda () "x"
                (OSL.NamedType () "A")
                (OSL.Apply ()
                  (OSL.To () "Enum3")
                  (OSL.Apply ()
                    (OSL.Iota1 ())
                    (OSL.Apply ()
                      (OSL.Iota1 ())
                      (OSL.NamedTerm () "x")))))
          ),
          ( "B_inj",
            OSL.Defined
              (OSL.F () Nothing
                (OSL.NamedType () "B")
                (OSL.NamedType () "Enum3"))
              (OSL.Lambda () "x"
                (OSL.NamedType () "B")
                (OSL.Apply ()
                  (OSL.To () "Enum3")
                  (OSL.Apply ()
                    (OSL.Iota1 ())
                    (OSL.Apply ()
                      (OSL.Iota2 ())
                      (OSL.NamedTerm () "x")))))
          ),
          ( "C_inj",
            OSL.Defined
              (OSL.F () Nothing
                (OSL.NamedType () "C")
                (OSL.NamedType () "Enum3"))
              (OSL.Lambda () "x"
                (OSL.NamedType () "C")
                (OSL.Apply ()
                  (OSL.To () "Enum3")
                  (OSL.Apply ()
                    (OSL.Iota2 ())
                    (OSL.NamedTerm () "x"))))
          )
        ]

sudokuTypes :: Spec
sudokuTypes =
  it "correctly assembles a sudoku types context" $
    sudokuTypesContext `shouldBe` expectedContext
  where
    sudokuTypesContext = flip execState mempty $ do
      add (Proxy @(Newtype Digit))
      add (Proxy @(Newtype Row))
      add (Proxy @(Newtype Col))
      add (Proxy @(Newtype Cell))
      add (Proxy @(Newtype Problem))
      add (Proxy @(Newtype Solution))
      add (Proxy @(Newtype X))
      add (Proxy @(Newtype Y))
      add (Proxy @(Newtype Square))
      add (Proxy @(Newtype SquareCell))

    expectedContext =
      OSL.ValidContext . Map.fromList $
        [ (OSL.Sym "Digit", OSL.Data (OSL.Z ())),
          (OSL.Sym "Col", OSL.Data (OSL.Z ())),
          (OSL.Sym "Row", OSL.Data (OSL.Z ())),
          ( OSL.Sym "Cell",
            OSL.Data
              ( OSL.Product
                  ()
                  (OSL.NamedType () (OSL.Sym "Row"))
                  (OSL.NamedType () (OSL.Sym "Col"))
              )
          ),
          ( OSL.Sym "Problem",
            OSL.Data
              ( OSL.F
                  ()
                  Nothing
                  (OSL.NamedType () (OSL.Sym "Cell"))
                  (OSL.Maybe () (OSL.NamedType () (OSL.Sym "Digit")))
              )
          ),
          ( OSL.Sym "Solution",
            OSL.Data
              ( OSL.F
                  ()
                  Nothing
                  (OSL.NamedType () (OSL.Sym "Cell"))
                  (OSL.NamedType () (OSL.Sym "Digit"))
              )
          ),
          (OSL.Sym "X", OSL.Data (OSL.Z ())),
          (OSL.Sym "Y", OSL.Data (OSL.Z ())),
          ( OSL.Sym "Square",
            OSL.Data
              ( OSL.Product
                  ()
                  (OSL.NamedType () "X")
                  (OSL.NamedType () "Y")
              )
          ),
          ( OSL.Sym "SquareCell",
            OSL.Data
              ( OSL.Product
                  ()
                  (OSL.NamedType () "X")
                  (OSL.NamedType () "Y")
              )
          )
        ]

    add :: forall a. AddToOSLContext a => Proxy a -> State (OSL.ValidContext 'OSL.Global ()) ()
    add = addToOSLContextM
