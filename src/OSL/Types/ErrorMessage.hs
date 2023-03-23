{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage)) where

import Data.Text (Text)
import GHC.Generics (Generic)

data ErrorMessage ann = ErrorMessage
  { annotation :: ann,
    message :: Text
  }
  deriving (Eq, Ord, Generic, Show)

instance Semigroup a => Semigroup (ErrorMessage a) where
  (ErrorMessage a1 m1) <> (ErrorMessage a2 m2) = ErrorMessage (a1 <> a2) (m1 <> " | " <> m2)

instance Monoid a => Monoid (ErrorMessage a) where
  mempty = mempty
