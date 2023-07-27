{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Stark.Types.Scalar
  ( Scalar,
    order,
    inverseScalar,
    generator,
    primitiveNthRoot,
    sample,
    scalarToRational,
    scalarToInt,
    scalarToInteger,
    integerToScalar,
    zero,
    one,
    minusOne,
    two,
    half,
  )
where

import qualified Algebra.Additive as Group
import qualified Algebra.Ring as Ring
import Cast (word8ToInteger)
import Control.Monad (guard)
import Data.Bits (toIntegralSized, (.&.))
import qualified Data.ByteString as BS
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Text (pack)
import Data.Word (Word64)
import Die (die)
import GHC.Generics (Generic)
import Prelude hiding (fromInteger, toInteger)
import qualified Prelude

-- The scalar field of the Vesta curve, which is the base
-- field of the Pallas curve.
type Scalar :: Type
newtype Scalar = Scalar {unScalar :: Integer}
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show)

instance Group.C Scalar where
  zero = zero
  (+) = addScalar
  negate = negateScalar

instance Ring.C Scalar where
  one = one
  (*) = mulScalar

order :: Integer
order = 0x40000000000000000000000000000000224698fc094cf91b992d30ed00000001

fromInteger :: Integer -> Maybe Scalar
fromInteger x | x < order = Just $ Scalar x
fromInteger _ = Nothing

toInteger :: Scalar -> Integer
toInteger (Scalar x) = x

addScalar :: Scalar -> Scalar -> Scalar
addScalar (Scalar x) (Scalar y) = Scalar ((x + y) `mod` order)

negateScalar :: Scalar -> Scalar
negateScalar (Scalar x) = Scalar $ order - x

mulScalar :: Scalar -> Scalar -> Scalar
mulScalar (Scalar x) (Scalar y) =
  Scalar ((x * y) `mod` order)

inverseScalar :: Scalar -> Maybe Scalar
inverseScalar (Scalar 0) = Nothing
inverseScalar x = Just $ x ^ (order - 2)

generator :: Scalar
generator = Scalar 5

primitiveNthRoot :: Word64 -> Maybe Scalar
primitiveNthRoot n = do
  guard (n <= initOrder)
  guard (n .&. (n - 1) == 0)
  pure $ f initRoot initOrder
  where
    initRoot = generator ^ (4294967295 :: Int)

    initOrder :: Word64
    initOrder = 2 ^ (32 :: Integer)

    f :: Num p => p -> Word64 -> p
    f root order' =
      if order' /= n
        then f (root * root) (order' `quot` 2)
        else root

instance Bounded Scalar where
  minBound = Scalar 0
  maxBound = Scalar (order - 1)

instance Num Scalar where
  fromInteger n =
    case fromInteger n of
      Just n' -> n'
      Nothing -> die (pack (show n) <> " is not less than " <> pack (show order))
  (+) = addScalar
  (*) = mulScalar
  negate = negateScalar
  abs = die "abs Scalar unimplemented"
  signum = die "signum Scalar unimplemented"

instance Fractional Scalar where
  recip x = case inverseScalar x of
    Just y -> y
    Nothing -> die "0 has no reciprocal"
  fromRational x =
    (if x < 0 then negate else id) $
      Prelude.fromInteger (numerator (abs x))
        / Prelude.fromInteger (denominator (abs x))

instance Real Scalar where
  toRational = toRational . toInteger

sample :: BS.ByteString -> Scalar
sample = Prelude.fromInteger . sampleInteger

sampleInteger :: BS.ByteString -> Integer
sampleInteger = BS.foldl (\x b -> x + word8ToInteger b) 0

scalarToInteger :: Scalar -> Integer
scalarToInteger = unScalar

scalarToRational :: Scalar -> Rational
scalarToRational = toRational . unScalar

integerToScalar :: Integer -> Maybe Scalar
integerToScalar n =
  if n >= 0
    then fromInteger n
    else negateScalar <$> fromInteger (negate n)

scalarToInt :: Scalar -> Int
scalarToInt = fromMaybe (die "scalarToInt partiality") . toIntegralSized . toInteger

zero, one, minusOne, two, half :: Scalar
zero = 0
one = 1
minusOne = negateScalar one
two = 2
half = fromMaybe (die "half: partiality") (inverseScalar two)
