{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Halo2.Codegen
  ( generateProject,
    TargetDirectory (TargetDirectory)
  ) where

import Control.Lens ((^.))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Die (die)
import Halo2.CircuitEdits (getCircuitEdits)
import Halo2.Types.Circuit (ArithmeticCircuit)
import Halo2.Types.CircuitEdit (CircuitEdit (AddColumn, EnableEquality))
import Halo2.Types.ColumnType (ColumnType (Advice, Instance, Fixed))
import Halo2.Types.TargetDirectory (TargetDirectory (TargetDirectory))
import Lib.Git (initDB, add)
import Lib.Git.Type (makeConfig, runGit)
import OSL.Types.ErrorMessage (ErrorMessage)
import System.Directory (createDirectoryIfMissing)
import Text.RawString.QQ (r)

generateProject :: TargetDirectory -> ArithmeticCircuit -> IO ()
generateProject td@(TargetDirectory targetDirectory) c = do
  createDirectoryIfMissing True (targetDirectory <> "/src")
  createLibFile td c
  createLicenseFile td
  createNoticeFile td
  createGitignoreFile td
  createReadmeFile td
  createRustToolchainTomlFile td
  createCargoTomlFile td
  createCargoNixFile td
  createCargoLockFile td
  createFlakeNixFile td
  createFlakeLockFile td
  runGit (makeConfig targetDirectory Nothing) $ do
    initDB False
    add $
      (("./") <>) <$>
        [ "LICENSE", "NOTICE", ".gitignore", "README.md",
          "rust-toolchain.toml", "Cargo.toml", "Cargo.nix",
          "Cargo.lock", "flake.nix", "flake.lock",
          "src/lib.rs"
        ]

createLicenseFile :: TargetDirectory -> IO ()
createLicenseFile =
  writeStaticFile "LICENSE"
    $(embedFile "./halo2-template/LICENSE")

createNoticeFile :: TargetDirectory -> IO ()
createNoticeFile =
  writeStaticFile "NOTICE"
    $(embedFile "./halo2-template/NOTICE")

createGitignoreFile :: TargetDirectory -> IO ()
createGitignoreFile =
  writeStaticFile ".gitignore"
    $(embedFile "./halo2-template/.gitignore")

createReadmeFile :: TargetDirectory -> IO ()
createReadmeFile =
  writeStaticFile "README.md"
    $(embedFile "./halo2-template/README.md")

createRustToolchainTomlFile :: TargetDirectory -> IO ()
createRustToolchainTomlFile =
  writeStaticFile "rust-toolchain.toml"
    $(embedFile "./halo2-template/rust-toolchain.toml")

createCargoTomlFile :: TargetDirectory -> IO ()
createCargoTomlFile =
  writeStaticFile "Cargo.toml"
    $(embedFile "./halo2-template/Cargo.toml")

createCargoNixFile :: TargetDirectory -> IO ()
createCargoNixFile =
  writeStaticFile "Cargo.nix"
    $(embedFile "./halo2-template/Cargo.nix")

createCargoLockFile :: TargetDirectory -> IO ()
createCargoLockFile =
  writeStaticFile "Cargo.lock"
    $(embedFile "./halo2-template/Cargo.lock")

createFlakeNixFile :: TargetDirectory -> IO ()
createFlakeNixFile =
  writeStaticFile "flake.nix"
    $(embedFile "./halo2-template/flake.nix")

createFlakeLockFile :: TargetDirectory -> IO ()
createFlakeLockFile =
  writeStaticFile "flake.lock"
    $(embedFile "./halo2-template/flake.lock")

createLibFile :: TargetDirectory -> ArithmeticCircuit -> IO ()
createLibFile targetDir c =
  case getLibSource c of
    Right src ->
      writeStaticFile "src/lib.rs" src targetDir
    Left err ->
      die (err ^. #message)

getLibSource :: ArithmeticCircuit -> Either (ErrorMessage ()) ByteString
getLibSource c = do
  edits <- getCircuitEdits c
  pure $
    mconcat
      [ prelude,
        BS.intercalate "\n"
          (("    " <>) . getEditSource <$> edits),
        postlude
      ]
  where
    prelude = $(embedFile "./halo2-template/src/prelude.rs")

    postlude = [r|
    MyConfig {}
  }
}
|]

getEditSource :: CircuitEdit -> ByteString
getEditSource =
  \case
    AddColumn ci Advice ->
      "let c" <> f ci <> " = meta.advice_column();"
    AddColumn ci Instance ->
      "let c" <> f ci <> " = meta.instance_column();"
    AddColumn ci Fixed ->
      "let c" <> f ci <> " = meta.fixed_column();"
    EnableEquality ci ->
      "meta.enable_equality(c" <> f ci <> ");"
    _ -> todo
  where
    f = encodeUtf8 . pack . show

todo :: a
todo = die "todo"

writeStaticFile :: FilePath -> ByteString -> TargetDirectory -> IO ()
writeStaticFile filename contents (TargetDirectory targetDir) =
  BS.writeFile (targetDir <> "/" <> filename) contents
