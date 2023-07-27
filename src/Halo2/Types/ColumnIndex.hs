{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Halo2.Types.ColumnIndex
  ( ColumnIndex (ColumnIndex, getColumnIndex),
  )
where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Halo2.Prelude

newtype ColumnIndex = ColumnIndex {getColumnIndex :: Int}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Num, Enum, Real, Integral, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
