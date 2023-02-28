{-# LANGUAGE TemplateHaskell #-}

module Halo2.Codegen
  ( generateProject,
    TargetDirectory (TargetDirectory)
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Halo2.Types.Circuit (ArithmeticCircuit)
import Halo2.Types.TargetDirectory (TargetDirectory (TargetDirectory))
import Lib.Git (initDB, add)
import Lib.Git.Type (makeConfig, runGit)
import System.Directory (createDirectoryIfMissing)

generateProject :: TargetDirectory -> ArithmeticCircuit -> IO ()
generateProject td@(TargetDirectory targetDirectory) _c = do
  createDirectoryIfMissing True (targetDirectory <> "/src/bin")
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
  createLibFile td
  createGenProofFile td
  createVerifyFile td
  runGit (makeConfig targetDirectory Nothing) $ do
    initDB False
    add $
      (("./") <>) <$>
        [ "LICENSE", "NOTICE", ".gitignore", "README.md",
          "rust-toolchain.toml", "Cargo.toml", "Cargo.nix",
          "Cargo.lock", "flake.nix", "flake.lock",
          "src/lib.rs", "src/bin/gen_proof.rs",
          "src/bin/verify.rs"
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

createLibFile :: TargetDirectory -> IO ()
createLibFile =
  writeStaticFile "src/lib.rs"
    $(embedFile "./halo2-template/src/lib.rs")

createGenProofFile :: TargetDirectory -> IO ()
createGenProofFile =
  writeStaticFile "src/bin/gen_proof.rs"
    $(embedFile "./halo2-template/src/bin/gen_proof.rs")

createVerifyFile :: TargetDirectory -> IO ()
createVerifyFile =
  writeStaticFile "src/bin/verify.rs"
    $(embedFile "./halo2-template/src/bin/verify.rs")

writeStaticFile :: FilePath -> ByteString -> TargetDirectory -> IO ()
writeStaticFile filename contents (TargetDirectory targetDir) =
  BS.writeFile (targetDir <> "/" <> filename) contents
