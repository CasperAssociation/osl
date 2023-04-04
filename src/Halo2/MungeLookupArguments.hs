-- This module exists to deal with putting lookup arguments into a form that
-- satisfies the constraints of Halo 2 dynamic lookups: all fixed columns come
-- before all advice columns in lookup tables, and there are no instance columns
-- in lookup tables. To deal with instance columns in lookup tables, we add
-- advice columns with the same data (constrained using equality constraints).

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Halo2.MungeLookupArguments
  ( mungeLookupArguments,
    mungeArgument
  ) where

import Control.Arrow (second)
import Control.Lens ((.~), (^.))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Die (die)
import Halo2.Types.Argument (Argument)
import Halo2.Types.Circuit (ArithmeticCircuit)
import Halo2.Types.ColumnIndex (ColumnIndex)
import Halo2.Types.ColumnType (ColumnType (Advice, Instance))
import Halo2.Types.LookupArgument (LookupArgument (LookupArgument))
import Halo2.Types.LookupArguments (LookupArguments (LookupArguments))
import Halo2.Types.LookupTableColumn (LookupTableColumn (LookupTableColumn))
import Halo2.Types.Polynomial (Polynomial)
import OSL.Types.ErrorMessage (ErrorMessage)

todo :: a
todo = die "todo"

newtype InstanceToAdviceMapping =
  InstanceToAdviceMapping
    (Map ColumnIndex ColumnIndex)

mungeLookupArguments ::
  ArithmeticCircuit ->
  Either (ErrorMessage ()) ArithmeticCircuit
mungeLookupArguments =
  reorderLookupTableColumns . replaceInstanceWithAdvice

replaceInstanceWithAdvice :: ArithmeticCircuit -> ArithmeticCircuit
replaceInstanceWithAdvice c =
  let m = getInstanceToAdviceMapping c
  in doReplacementOnCircuit m (insertNewAdviceInColumnTypes m c)

getInstanceToAdviceMapping ::
  ArithmeticCircuit ->
  InstanceToAdviceMapping
getInstanceToAdviceMapping c =
  let icols = getLookupArgumentInstanceColumns c
      acol0 = getFirstUnusedColumnIndex c
  in InstanceToAdviceMapping . Map.fromList
       $ zip (Set.toList icols) [acol0..]

doReplacementOnCircuit ::
  InstanceToAdviceMapping ->
  ArithmeticCircuit ->
  ArithmeticCircuit
doReplacementOnCircuit m c =
  (#lookupArguments) .~
    LookupArguments
      (Set.map
        (doReplacementOnLookupArgument m)
        (c ^. #lookupArguments . #getLookupArguments))
    $ c

insertNewAdviceInColumnTypes ::
  InstanceToAdviceMapping ->
  ArithmeticCircuit ->
  ArithmeticCircuit
insertNewAdviceInColumnTypes (InstanceToAdviceMapping m) c =
  (#columnTypes . #getColumnTypes) .~
    foldl (.) id
      [ Map.insert k Advice
        | k <- Map.elems m
      ]
      (c ^. #columnTypes . #getColumnTypes)
    $ c

doReplacementOnLookupArgument ::
  InstanceToAdviceMapping ->
  LookupArgument Polynomial ->
  LookupArgument Polynomial
doReplacementOnLookupArgument m (LookupArgument lbl g t) =
  LookupArgument lbl g (second (doReplacementOnLookupTableColumn m) <$> t)

doReplacementOnLookupTableColumn ::
  InstanceToAdviceMapping ->
  LookupTableColumn ->
  LookupTableColumn
doReplacementOnLookupTableColumn
    (InstanceToAdviceMapping m)
    (LookupTableColumn c) =
  LookupTableColumn
    . fromMaybe c
      $ Map.lookup c m

getLookupArgumentInstanceColumns ::
  ArithmeticCircuit ->
  Set ColumnIndex
getLookupArgumentInstanceColumns c =
  getLookupArgumentsColumns (c ^. #lookupArguments)
    `Set.intersection` getInstanceColumns c

getLookupArgumentsColumns ::
  LookupArguments Polynomial ->
  Set ColumnIndex
getLookupArgumentsColumns =
  mconcat . fmap getLookupArgumentColumns
    . Set.toList . (^. #getLookupArguments)

getLookupArgumentColumns ::
  LookupArgument Polynomial ->
  Set ColumnIndex
getLookupArgumentColumns =
  Set.fromList
    . fmap ((^. #unLookupTableColumn) . snd)
    . (^. #tableMap)

getInstanceColumns ::
  ArithmeticCircuit ->
  Set ColumnIndex
getInstanceColumns =
  Map.keysSet
    . Map.filter (== Instance)
    . (^. #columnTypes . #getColumnTypes)

getFirstUnusedColumnIndex ::
  ArithmeticCircuit ->
  ColumnIndex
getFirstUnusedColumnIndex =
  fromMaybe 0
    . fmap fst
    . Map.lookupMax
    . (^. #columnTypes . #getColumnTypes)

reorderLookupTableColumns ::
  ArithmeticCircuit ->
  Either (ErrorMessage ()) ArithmeticCircuit
reorderLookupTableColumns = todo

mungeArgument ::
  ArithmeticCircuit ->
  Argument ->
  Either (ErrorMessage ()) Argument
mungeArgument = replicateInstanceToAdviceInArgument

replicateInstanceToAdviceInArgument ::
  ArithmeticCircuit ->
  Argument ->
  Either (ErrorMessage ()) Argument
replicateInstanceToAdviceInArgument = todo
