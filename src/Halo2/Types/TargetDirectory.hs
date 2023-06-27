{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Halo2.Types.TargetDirectory (TargetDirectory (TargetDirectory)) where

import Data.String (IsString)
import GHC.Generics (Generic)

newtype TargetDirectory =
  TargetDirectory
    { unTargetDirectory :: String }
  deriving (Generic, IsString)
