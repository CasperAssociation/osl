{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module OSL.Types.OSL
  ( Name (..),
    Cardinality (..),
    Type (..),
    Term (..),
    Bound (..),
    LeftBound (..),
    RightBound (..),
    DomainBound (..),
    CodomainBound (..),
    ValuesBound (..),
    KeysBound (..),
    Declaration (..),
    Context (..),
    ContextType (..),
    ValidContext (..),
    Quantifier (ForAll', ForSome'),
  )
where

import Data.Generics.Labels ()
import Data.Map (Map)
import Data.String (IsString (fromString))
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import OSL.Types.Cardinality (Cardinality (..))

data Name
  = Sym Text
  | GenSym Int
  deriving (Eq, Generic)

instance Show Name where
  show =
    \case
      Sym x -> unpack x
      GenSym i -> "g%" <> show i

instance IsString Name where
  fromString = Sym . pack

instance Ord Name where
  Sym a <= Sym b = a <= b
  GenSym a <= GenSym b = a <= b
  Sym _ <= GenSym _ = True
  GenSym _ <= Sym _ = False

data Type ann
  = Prop ann
  | F ann (Maybe Cardinality) (Type ann) (Type ann) -- functions
  | P ann (Maybe Cardinality) (Type ann) (Type ann) -- permutations
  | N ann -- natural numbers
  | Z ann -- integers
  | Fp ann -- native field elements
  | Fin ann Integer
  | Product ann (Type ann) (Type ann)
  | Coproduct ann (Type ann) (Type ann)
  | NamedType ann Name
  | Maybe ann (Type ann)
  | List ann Cardinality (Type ann)
  | Map ann Cardinality (Type ann) (Type ann)
  deriving (Eq, Ord)

instance Show (Type ann) where
  show (Prop _) = "Prop"
  show (F _ (Just n) a b) = "(" <> show a <> " ->^" <> show n <> " " <> show b <> ")"
  show (F _ Nothing a b) = "(" <> show a <> " -> " <> show b <> ")"
  show (P _ (Just n) a b) = "(" <> show a <> " <->^" <> show n <> " " <> show b <> ")"
  show (P _ Nothing a b) = "(" <> show a <> " <-> " <> show b <> ")"
  show (N _) = "N"
  show (Z _) = "Z"
  show (Fp _) = "Fp"
  show (Fin _ n) = "Fin(" <> show n <> ")"
  show (Product _ a b) = "(" <> show a <> " * " <> show b <> ")"
  show (Coproduct _ a b) = "(" <> show a <> " + " <> show b <> ")"
  show (NamedType _ (Sym a)) = unpack a
  show (NamedType _ (GenSym a)) = "$gensym" <> show a
  show (Maybe _ a) = "Maybe(" <> show a <> ")"
  show (List _ n a) = "List^ " <> show n <> "(" <> show a <> ")"
  show (Map _ n a b) = "Map^" <> show n <> "(" <> show a <> ", " <> show b <> ")"

data Term ann
  = NamedTerm ann Name
  | AddN ann
  | MulN ann
  | ConstN ann Integer
  | AddZ ann
  | MulZ ann
  | ConstZ ann Integer
  | ConstFp ann Integer
  | AddFp ann
  | MulFp ann
  | Cast ann
  | ConstFin ann Integer
  | ConstF ann [(Term ann, Term ann)]
  | ConstSet ann [Term ann]
  | Inverse ann
  | Pair ann
  | Pi1 ann -- Product projections
  | Pi2 ann
  | Iota1 ann -- Coproduct injections
  | Iota2 ann
  | FunctionProduct ann (Term ann) (Term ann)
  | FunctionCoproduct ann (Term ann) (Term ann)
  | Lambda ann Name (Type ann) (Term ann)
  | Apply ann (Term ann) (Term ann)
  | To ann Name
  | From ann Name
  | Let ann Name (Type ann) (Term ann) (Term ann)
  | IsNothing ann
  | Just' ann
  | Nothing' ann
  | Maybe' ann (Term ann)
  | MaybePi1 ann
  | MaybePi2 ann
  | MaybeTo ann Name
  | MaybeFrom ann Name
  | MaxN ann
  | MaxZ ann
  | MaxFp ann
  | Exists ann
  | Length ann
  | Nth ann
  | ListCast ann
  | ListPi1 ann
  | ListPi2 ann
  | ListTo ann Name
  | ListFrom ann Name
  | ListLength ann
  | ListMaybePi1 ann
  | ListMaybePi2 ann
  | ListMaybeLength ann
  | ListMaybeFrom ann Name
  | ListMaybeTo ann Name
  | Sum ann
  | Lookup ann
  | Keys ann
  | MapPi1 ann
  | MapPi2 ann
  | MapTo ann Name
  | MapFrom ann Name
  | SumMapLength ann
  | SumListLookup ann (Term ann)
  | Equal ann (Term ann) (Term ann)
  | LessOrEqual ann (Term ann) (Term ann)
  | And ann (Term ann) (Term ann)
  | Or ann (Term ann) (Term ann)
  | Not ann (Term ann)
  | Implies ann (Term ann) (Term ann)
  | Iff ann (Term ann) (Term ann)
  | ForAll ann Name (Type ann) (Maybe (Bound ann)) (Term ann)
  | ForSome ann Name (Type ann) (Maybe (Bound ann)) (Term ann)
  | Top ann
  | Bottom ann
  deriving (Eq, Ord)

instance Show (Term ann) where
  show (NamedTerm _ name) = show name
  show (AddN _) = "+N"
  show (MulN _) = "*N"
  show (ConstN _ int) = show int <> "N"
  show (AddZ _) = "+Z"
  show (MulZ _) = "*Z"
  show (ConstZ _ int) = show int <> "Z"
  show (ConstFp _ int) = show int <> "F"
  show (AddFp _) = "+F"
  show (MulFp _) = "*F"
  show (Cast _) = "cast"
  show (ConstFin _ int) = "fin(" <> show int <> ")"
  show (ConstF _ _) = ""
  show (ConstSet _ _) = ""
  show (Inverse _) = "inverse"
  show (Pair _) = ""
  show (Pi1 _) = "pi1"
  show (Pi2 _) = "pi2"
  show (Iota1 _) = "iota1"
  show (Iota2 _) = "iota2"
  show (FunctionProduct _ trm1 trm2) = "(" <> show trm1 <> " × " <> show trm2 <> ")"
  show (FunctionCoproduct _ trm1 trm2) = "(" <> show trm1 <> " ⊕ " <> show trm2 <> ")"
  show (Lambda _ name typ trm) = "\\" <> show name <> " : " <> show typ <> " => " <> show trm
  show (Apply _ trm1 trm2) = show trm1 <> "(" <> show trm2 <> ")"
  show (To _ name) = "to(" <> show name <> ")"
  show (From _ name) = "from(" <> show name <> ")"
  show (Let _ name typ trm1 trm2) = "let " <> show name <> " : " <> show typ <> " := " <> show trm1 <> "; " <> show trm2
  show (IsNothing _) = "isNothing"
  show (Just' _) = "just"
  show (Nothing' _) = "nothing"
  show (Maybe' _ trm) = "maybe(" <> show trm <> ")"
  show (MaybePi1 _) = "pi1"
  show (MaybePi2 _) = "pi2"
  show (MaybeTo _ name) = "to(" <> show name <> ")"
  show (MaybeFrom _ name) = "from(" <> show name <> ")"
  show (MaxN _) = "maxN"
  show (MaxZ _) = "maxZ"
  show (MaxFp _) = "maxF"
  show (Exists _) = "exists"
  show (Length _) = "length"
  show (Nth _) = "nth"
  show (ListCast _) = "cast"
  show (ListPi1 _) = "pi1"
  show (ListPi2 _) = "pi2"
  show (ListTo _ name) = "to(" <> show name <> ")"
  show (ListFrom _ name) = "from(" <> show name <> ")"
  show (ListLength _) = "length"
  show (ListMaybePi1 _) = "pi1"
  show (ListMaybePi2 _) = "pi2"
  show (ListMaybeLength _) = "length"
  show (ListMaybeFrom _ name) = "to(" <> show name <> ")"
  show (ListMaybeTo _ name) = "from(" <> show name <> ")"
  show (Sum _) = "sum"
  show (Lookup _) = "lookup"
  show (Keys _) = "keys"
  show (MapPi1 _) = "pi1"
  show (MapPi2 _) = "pi2"
  show (MapTo _ name) = "to(" <> show name <> ")"
  show (MapFrom _ name) = "from(" <> show name <> ")"
  show (SumMapLength _) = ""
  show (SumListLookup _ _) = ""
  show (Equal _ trm1 trm2) = "(" <> show trm1 <> " = " <> show trm2 <> ")"
  show (LessOrEqual _ trm1 trm2) = "(" <> show trm1 <> " <= " <> show trm2 <> ")"
  show (And _ trm1 trm2) = "(" <> show trm1 <> " & " <> show trm2 <> ")"
  show (Or _ trm1 trm2) = "(" <> show trm1 <> " | " <> show trm2 <> ")"
  show (Not _ trm1) = "!" <> show trm1
  show (Implies _ trm1 trm2) = "(" <> show trm1 <> " -> " <> show trm2 <> ")"
  show (Iff _ trm1 trm2) = "(" <> show trm1 <> " <-> " <> show trm2 <> ")"
  show (ForAll _ name typ Nothing trm) = "all " <> show name <> " : " <> show typ <> ", " <> show trm
  show (ForAll _ name typ (Just bnd) trm) = "all " <> show name <> " : " <> show typ <> " < " <> show bnd <> ", " <> show trm
  show (ForSome _ name typ Nothing trm) = "some " <> show name <> " : " <> show typ <> ", " <> show trm
  show (ForSome _ name typ (Just bnd) trm) = "some " <> show name <> " : " <> show typ <> " < " <> show bnd <> ", " <> show trm
  show (Top _) = "⊤" -- Is there a non-unicode alternative?
  show (Bottom _) = "⊥" -- Is there a non-unicode alternative?

data Quantifier ann
  = ForAll' ann Name (Type ann) (Maybe (Bound ann))
  | ForSome' ann Name (Type ann) (Maybe (Bound ann))
  deriving (Eq, Ord, Show)

data Bound ann
  = ScalarBound ann (Term ann)
  | FieldMaxBound ann
  | ProductBound ann (LeftBound ann) (RightBound ann)
  | CoproductBound ann (LeftBound ann) (RightBound ann)
  | FunctionBound ann (DomainBound ann) (CodomainBound ann)
  | ListBound ann (ValuesBound ann)
  | MaybeBound ann (ValuesBound ann)
  | MapBound ann (KeysBound ann) (ValuesBound ann)
  | ToBound ann Name (Bound ann)
  deriving (Eq, Ord)

instance Show (Bound ann) where
  show (ScalarBound _ trm) = show trm
  show (FieldMaxBound _) = "MAX"
  show (ProductBound _ lb rb) = "(" <> show lb <> " × " <> show rb <> ")"
  show (CoproductBound _ lb rb) = "(" <> show lb <> " ⊕ " <> show rb <> ")"
  show (FunctionBound _ _ _) = ""
  show (ListBound _ _) = ""
  show (MaybeBound _ _) = ""
  show (MapBound _ _ _) = ""
  show (ToBound _ name bnd) = "to(" <> show name <> ")(" <> show bnd <> ")"

newtype LeftBound ann = LeftBound {unLeftBound :: Bound ann}
  deriving (Eq, Ord, Show)

newtype RightBound ann = RightBound {unRightBound :: Bound ann}
  deriving (Eq, Ord, Show)

newtype DomainBound ann = DomainBound {unDomainBound :: Bound ann}
  deriving (Eq, Ord, Show)

newtype CodomainBound ann = CodomainBound {unCodomainBound :: Bound ann}
  deriving (Eq, Ord, Show)

newtype ValuesBound ann = ValuesBound {unValuesBound :: Bound ann}
  deriving (Eq, Ord, Show)

newtype KeysBound ann = KeysBound {unKeysBound :: Bound ann}
  deriving (Eq, Ord, Show)

data Declaration ann
  = FreeVariable (Type ann)
  | Data (Type ann)
  | Defined (Type ann) (Term ann)
  deriving (Eq, Show)

newtype Context ann = Context {unContext :: [(Name, Declaration ann)]}

data ContextType = Global | Local
  deriving (Show)

newtype ValidContext (t :: ContextType) ann = ValidContext {unValidContext :: Map Name (Declaration ann)}
  deriving (Eq, Generic, Show, Semigroup, Monoid)
