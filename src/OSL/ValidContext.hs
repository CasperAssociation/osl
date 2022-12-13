{-# LANGUAGE OverloadedStrings #-}

module OSL.ValidContext
  ( getDeclaration,
    getExistingDeclaration,
    getNamedTermUnsafe,
    addDeclaration,
    getFreeOSLName,
  )
where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Die (die)
import OSL.Term (termAnnotation)
import OSL.Types.OSL (Declaration (Defined), Name (GenSym, Sym), ValidContext (..), Term (NamedTerm))

getNamedTermUnsafe :: ValidContext t ann -> Name -> Term ann
getNamedTermUnsafe c name =
  case getExistingDeclaration c name of
    Defined _ x ->
      NamedTerm (termAnnotation x) name
    _ -> die "getNamedTermUnsafe: expected the name of a defined term"

getDeclaration :: ValidContext t ann -> Name -> Maybe (Declaration ann)
getDeclaration (ValidContext decls) name = Map.lookup name decls

getExistingDeclaration ::
  ValidContext t ann ->
  Name ->
  Declaration ann
getExistingDeclaration c name =
  fromMaybe
    (die $ "logically impossible: could not find a declaration known to exist: " <> pack (show name))
    (getDeclaration c name)

addDeclaration ::
  Name ->
  Declaration ann ->
  ValidContext t ann ->
  ValidContext t ann
addDeclaration name decl (ValidContext c) =
  ValidContext (Map.insert name decl c)

getFreeOSLName ::
  ValidContext t ann ->
  Name
getFreeOSLName (ValidContext c) =
  case fst <$> Map.lookupMax c of
    Nothing -> GenSym 0
    Just (Sym _) -> GenSym 0
    Just (GenSym i) -> GenSym (i + 1)
