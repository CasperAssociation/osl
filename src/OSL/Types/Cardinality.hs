{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OSL.Types.Cardinality (Cardinality (..), cardinalityToInteger) where

-- The maximum number of elements of a collection type.
newtype Cardinality = Cardinality Integer
  deriving newtype (Eq, Ord, Show, Num)

cardinalityToInteger :: Cardinality -> Integer
cardinalityToInteger (Cardinality c) = c
