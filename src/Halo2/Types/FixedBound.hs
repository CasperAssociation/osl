{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Halo2.Types.FixedBound
  ( FixedBound (FixedBound),
    boolBound,
    integerToFixedBound,
    fixedBoundToInteger,
  )
where

import Die (die)
import Halo2.Prelude
import Stark.Types.Scalar (order)

newtype FixedBound = FixedBound
  {unFixedBound :: Integer}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord)

instance Num FixedBound where
  a + b = integerToFixedBound (fixedBoundToInteger a + fixedBoundToInteger b)
  a * b = integerToFixedBound (fixedBoundToInteger a * fixedBoundToInteger b)
  abs = id
  signum = const 1
  fromInteger = integerToFixedBound
  negate = die "FixedBound.negate: not implemented"

fixedBoundToInteger :: FixedBound -> Integer
fixedBoundToInteger = unFixedBound

integerToFixedBound :: Integer -> FixedBound
integerToFixedBound k =
  let w = abs k
   in if w < order then FixedBound w else FixedBound order

boolBound :: FixedBound
boolBound = FixedBound 2
