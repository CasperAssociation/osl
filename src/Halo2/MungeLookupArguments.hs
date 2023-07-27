-- This module exists to deal with putting lookup arguments into a form that
-- satisfies the constraints of Halo 2 dynamic lookups: all fixed columns come
-- before all advice columns in lookup tables, and there are no instance columns
-- in lookup tables. To deal with instance columns in lookup tables, we add
-- advice columns with the same data (constrained using equality constraints).
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Halo2.MungeLookupArguments
  ( mungeLookupArguments,
    mungeArgument,
    getColumnsOfType,
  )
where

import Control.Arrow (second)
import Control.Lens ((%~), (.~), (^.))
import Data.List (foldl', sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (pack)
import Halo2.Circuit (getColumnType, getRowSet)
import Halo2.Types.Argument (Argument, Witness (Witness))
import Halo2.Types.CellReference (CellReference (CellReference))
import Halo2.Types.Circuit (ArithmeticCircuit)
import Halo2.Types.ColumnIndex (ColumnIndex)
import Halo2.Types.ColumnType (ColumnType (Advice, Fixed, Instance))
import Halo2.Types.EqualityConstraint (EqualityConstraint (EqualityConstraint))
import Halo2.Types.LookupArgument (LookupArgument (LookupArgument))
import Halo2.Types.LookupArguments (LookupArguments (LookupArguments))
import Halo2.Types.LookupTableColumn (LookupTableColumn (LookupTableColumn))
import Halo2.Types.Polynomial (Polynomial)
import Halo2.Types.RowCount (RowCount)
import OSL.Types.ErrorMessage (ErrorMessage (ErrorMessage))

newtype InstanceToAdviceMapping
  = InstanceToAdviceMapping
      (Map ColumnIndex ColumnIndex)

mungeLookupArguments ::
  ArithmeticCircuit ->
  ArithmeticCircuit
mungeLookupArguments =
  reorderLookupTableColumnsInCircuit . replaceInstanceWithAdvice

replaceInstanceWithAdvice :: ArithmeticCircuit -> ArithmeticCircuit
replaceInstanceWithAdvice c =
  let m = getInstanceToAdviceMapping c
   in doReplacementOnCircuit
        m
        ( addEqualityConstraints
            m
            (insertNewAdviceInColumnTypes m c)
        )

getInstanceToAdviceMapping ::
  ArithmeticCircuit ->
  InstanceToAdviceMapping
getInstanceToAdviceMapping c =
  let icols = getLookupArgumentInstanceColumns c
      acol0 = getFirstUnusedColumnIndex c
   in InstanceToAdviceMapping . Map.fromList $
        zip (Set.toList icols) [acol0 ..]

doReplacementOnCircuit ::
  InstanceToAdviceMapping ->
  ArithmeticCircuit ->
  ArithmeticCircuit
doReplacementOnCircuit m c =
  #lookupArguments
    .~ LookupArguments
      ( Set.map
          (doReplacementOnLookupArgument m)
          (c ^. #lookupArguments . #getLookupArguments)
      )
    $ c

addEqualityConstraints ::
  InstanceToAdviceMapping ->
  ArithmeticCircuit ->
  ArithmeticCircuit
addEqualityConstraints (InstanceToAdviceMapping m) c =
  ( (#equalityConstrainableColumns . #getEqualityConstrainableColumns)
      %~ Set.union (Map.keysSet m `Set.union` Set.fromList (Map.elems m))
  )
    . ( (#equalityConstraints . #getEqualityConstraints)
          %~ (<> (uncurry (getEqualityConstraint (c ^. #rowCount)) `concatMap` Map.toList m))
      )
    $ c

getEqualityConstraint ::
  RowCount ->
  ColumnIndex ->
  ColumnIndex ->
  [EqualityConstraint]
getEqualityConstraint rc ci cj =
  [ EqualityConstraint $
      Set.fromList
        [ CellReference ci ri,
          CellReference cj ri
        ]
    | ri <- Set.toList $ getRowSet rc Nothing
  ]

insertNewAdviceInColumnTypes ::
  InstanceToAdviceMapping ->
  ArithmeticCircuit ->
  ArithmeticCircuit
insertNewAdviceInColumnTypes (InstanceToAdviceMapping m) c =
  (#columnTypes . #getColumnTypes)
    .~ foldl'
      (.)
      id
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
    `Set.intersection` getColumnsOfType Instance c

getLookupArgumentsColumns ::
  LookupArguments Polynomial ->
  Set ColumnIndex
getLookupArgumentsColumns =
  mconcat . fmap getLookupArgumentColumns
    . Set.toList
    . (^. #getLookupArguments)

getLookupArgumentColumns ::
  LookupArgument Polynomial ->
  Set ColumnIndex
getLookupArgumentColumns =
  Set.fromList
    . fmap ((^. #unLookupTableColumn) . snd)
    . (^. #tableMap)

getColumnsOfType ::
  ColumnType ->
  ArithmeticCircuit ->
  Set ColumnIndex
getColumnsOfType t =
  Map.keysSet
    . Map.filter (== t)
    . (^. #columnTypes . #getColumnTypes)

getFirstUnusedColumnIndex ::
  ArithmeticCircuit ->
  ColumnIndex
getFirstUnusedColumnIndex =
  maybe 0 ((+ 1) . fst)
    . Map.lookupMax
    . (^. #columnTypes . #getColumnTypes)

reorderLookupTableColumnsInCircuit ::
  ArithmeticCircuit ->
  ArithmeticCircuit
reorderLookupTableColumnsInCircuit c =
  (#lookupArguments . #getLookupArguments)
    %~ Set.map (reorderLookupTableColumnsInLookupArgument c)
    $ c

reorderLookupTableColumnsInLookupArgument ::
  ArithmeticCircuit ->
  LookupArgument Polynomial ->
  LookupArgument Polynomial
reorderLookupTableColumnsInLookupArgument c =
  #tableMap %~ sortBy cmp
  where
    cmp (_, LookupTableColumn ci) (_, LookupTableColumn cj) =
      case (getColumnType c ci, getColumnType c cj) of
        (Just Advice, Just Fixed) -> GT
        _ -> LT

mungeArgument ::
  ArithmeticCircuit ->
  Argument ->
  Either (ErrorMessage ()) Argument
mungeArgument = replicateInstanceToAdviceInArgument

replicateInstanceToAdviceInArgument ::
  ArithmeticCircuit ->
  Argument ->
  Either (ErrorMessage ()) Argument
replicateInstanceToAdviceInArgument c arg = do
  fs <- getFs
  pure (#witness %~ foldl' (.) id fs $ arg)
  where
    getFs :: Either (ErrorMessage ()) [Witness -> Witness]
    getFs = do
      let InstanceToAdviceMapping m = getInstanceToAdviceMapping c
      sequence
        [ do
            v <-
              maybe
                ( Left
                    ( ErrorMessage
                        ()
                        ( "replicateInstanceToAdviceInArgument: instance value lookup failed"
                            <> pack (show (ci, ri))
                        )
                    )
                )
                pure
                (Map.lookup (CellReference ci ri) (arg ^. #statement . #unStatement))
            pure $
              Witness
                . Map.insert (CellReference cj ri) v
                . (^. #unWitness)
          | (ci, cj) <- Map.toList m,
            ri <- Set.toList $ getRowSet (c ^. #rowCount) Nothing
        ]
