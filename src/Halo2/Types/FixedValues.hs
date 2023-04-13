{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Halo2.Types.FixedValues (FixedValues (FixedValues, getFixedValues)) where

import qualified Data.Map as Map
import Halo2.Prelude
import Halo2.Types.ColumnIndex (ColumnIndex)
import Halo2.Types.FixedColumn (FixedColumn)

newtype FixedValues a = FixedValues
  {getFixedValues :: Map ColumnIndex (FixedColumn a)}
  deriving (Eq, Ord, Generic, Show)

instance Ord a => Semigroup (FixedValues a) where
  FixedValues a <> FixedValues b = FixedValues (Map.unionWith (<>) a b)

instance Ord a => Monoid (FixedValues a) where
  mempty = FixedValues mempty
