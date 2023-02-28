{-# LANGUAGE DeriveGeneric #-}

module Halo2.Types.TargetDirectory (TargetDirectory (TargetDirectory)) where

import GHC.Generics (Generic)

newtype TargetDirectory =
  TargetDirectory
    { unTargetDirectory :: String }
  deriving Generic
