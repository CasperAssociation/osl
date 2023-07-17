{-# LANGUAGE DataKinds #-}
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
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Die (die)
import Halo2.Circuit (getPolynomialVariables)
import Halo2.CircuitEdits (getCircuitEdits)
import Halo2.MungeLookupArguments (getColumnsOfType)
import Halo2.Types.CellReference (CellReference (CellReference))
import Halo2.Types.Circuit (ArithmeticCircuit)
import Halo2.Types.CircuitEdit (CircuitEdit (AddColumn, AddEqualityConstraint, AddFixedColumn, AddLookupTable, AddLookupArgument, EnableEquality, AddGate))
import Halo2.Types.Coefficient (Coefficient)
import Halo2.Types.ColumnIndex (ColumnIndex (ColumnIndex))
import Halo2.Types.ColumnType (ColumnType (Advice, Instance, Fixed))
import Halo2.Types.Exponent (Exponent)
import Halo2.Types.InputExpression (InputExpression (InputExpression))
import Halo2.Types.Label (Label)
import Halo2.Types.LookupArgument (LookupArgument)
import Halo2.Types.LookupTableColumn (LookupTableColumn (LookupTableColumn))
import Halo2.Types.Polynomial (Polynomial)
import Halo2.Types.PolynomialVariable (PolynomialVariable (PolynomialVariable))
import Halo2.Types.PowerProduct (PowerProduct)
import Halo2.Types.RowIndex (RowIndex (RowIndex), RowIndexType (Absolute))
import Halo2.Types.TargetDirectory (TargetDirectory (TargetDirectory))
import Lib.Git (initDB, add)
import Lib.Git.Type (makeConfig, runGit)
import OSL.Types.ErrorMessage (ErrorMessage)
import Stark.Types.Scalar (Scalar, scalarToInt)
import System.Directory (createDirectoryIfMissing)
import Text.RawString.QQ (r)

generateProject :: TargetDirectory -> ArithmeticCircuit -> IO ()
generateProject td@(TargetDirectory targetDirectory) c = do
  createDirectoryIfMissing True (targetDirectory <> "/src")
  createLibFile td c
  createMainFile td
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
          "src/lib.rs", "src/main.rs"
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

createMainFile :: TargetDirectory -> IO ()
createMainFile =
  writeStaticFile "src/main.rs"
    $(embedFile "./halo2-template/src/main.rs")

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
        "\npub const ROW_COUNT: usize = "
          <> encodeUtf8 (pack (show (c ^. #rowCount . #getRowCount)))
          <> ";\n",
        interludeA,
        BS.intercalate "\n"
          (("    " <>) . getEditConfigureSource c <$> edits),
        interludeB,
        BS.intercalate "\n"
          (("      " <>) . getEditSynthesizeSource c <$> edits),
        postlude,
        entryPoint
      ]
  where
    prelude = $(embedFile "./halo2-template/src/prelude.rs")


    interludeA = [r|
#[derive(Clone)]
pub struct MyCircuit<F> {
  advice_data: Option<HashMap<ColumnIndex, Vec<F>>>
}

impl<F: PrimeField> Circuit<F> for MyCircuit<F> {
  type Config = MyConfig;
  type FloorPlanner = SimpleFloorPlanner;

  fn without_witnesses(&self) -> Self {
    MyCircuit {advice_data: None}
  }

  fn configure(meta: &mut ConstraintSystem<F>) -> Self::Config {
    let selector_all = meta.complex_selector();
    let mut instance_cols = HashMap::new();
    let mut advice_cols = HashMap::new();
    let mut fixed_cols = HashMap::new();
    let mut lookup_tables = Vec::new();

|]


    interludeB = [r|
    MyConfig {
      instance_columns: instance_cols,
      advice_columns: advice_cols,
      fixed_columns:  fixed_cols,
      selector_all: selector_all,
      lookup_tables: lookup_tables
    }
  }

  fn synthesize(&self, config: Self::Config, mut layouter: impl Layouter<F>) -> Result<(), Error> {
    let advice_data = &(self.advice_data);
    layouter.assign_region(|| "region", |mut region| {
      let ri = RegionIndex::from(0);
      let mut equality_constraints: Vec<Vec<Cell>> = Vec::new();
      let mut fixed_values: HashMap<ColumnIndex, Vec<F>> = HashMap::new();
      for hm in advice_data.into_iter() {
        for (ci, xs) in hm.iter() {
          let col = config.advice_columns.get(ci);
          match col {
            Some(col) => {
              for (ri, x) in xs.iter().enumerate() {
                region
                  .assign_advice(|| "", *col, ri, || Value::known(Assigned::from(x)))
                  .unwrap();
              }
            },
            None => {
              let col = config.fixed_columns.get(ci);
              match col {
                Some(col) => {
                  for (ri, x) in xs.iter().enumerate() {
                    region
                      .assign_fixed(|| "", *col, ri, || Value::known(Assigned::from(x)))
                      .unwrap();
                  }
                },
                None => {
                  die!("found witness data which does not correlate to a fixed or advice column");
                }
              }
            }
          };
        }
      }
|]


    postlude = [r|
      for (ci, xs) in &fixed_values {
        let col = config.fixed_columns.get(ci).expect(format!("Could not find column: {}", ci).as_str());
        for (ri, x) in xs.iter().enumerate() {
          region
            .assign_fixed(|| "", *col, ri, || Value::known(Assigned::from(x)))
            .unwrap();
        }
      }

      for i in 0 .. ROW_COUNT {
        config.selector_all
          .enable(&mut region, i)
          .unwrap();
        for tab in &config.lookup_tables {
          tab.add_row(&mut region, i).unwrap();
        }
      }

      for cells in &equality_constraints {
        if cells.len() > 0 {
          let cell0 = cells[0];
          for cell in &cells[1..] {
            region.constrain_equal(cell0, *cell).unwrap();
          }
        }
      }
      Ok(())
    })
  }
}
|]

    entryPoint = $(embedFile "./halo2-template/src/entry-point.rs")


getEditConfigureSource :: ArithmeticCircuit -> CircuitEdit -> ByteString
getEditConfigureSource c =
  \case
    AddColumn ci Advice ->
      "let c" <> f ci <> " = meta.advice_column();\n"
        <> "advice_cols.insert(ColumnIndex {index:" <> f ci <> "}, c" <> f ci <> ");"
    AddColumn ci Instance ->
      "let c" <> f ci <> " = meta.instance_column();\n"
        <> "instance_cols.insert(ColumnIndex {index:" <> f ci <> "}, c" <> f ci <> ");"
    AddColumn ci Fixed ->
      "let c" <> f ci <> " = meta.fixed_column();\n"
        <> "fixed_cols.insert(ColumnIndex {index:" <> f ci <> "}, c" <> f ci <> ");"
    EnableEquality ci ->
      "meta.enable_equality(c" <> f ci <> ");"
    AddGate l p ->
      getAddGateSource l p
    AddLookupTable l tab ->
      let fixedCols = Set.map LookupTableColumn
                      $ getColumnsOfType Fixed c
          adviceCols = Set.map LookupTableColumn
                       $ getColumnsOfType Advice c
          tabFixedCols = (`Set.member` fixedCols) `filter` tab
          tabAdviceCols = (`Set.member` adviceCols) `filter` tab
      in "let " <> getTableName tab <> " = meta.create_dynamic_table(" <> f l <> ", "
          <> getLookupTableColumnsSource tabFixedCols
          <> ", " <> getLookupTableColumnsSource tabAdviceCols <> ");"
          <> "\n        lookup_tables.push("
          <> getTableName tab <> ".clone());"
    AddLookupArgument arg ->
      getAddLookupArgumentSource arg
    AddEqualityConstraint {} -> mempty
    AddFixedColumn {} -> mempty


getEditSynthesizeSource :: ArithmeticCircuit -> CircuitEdit -> ByteString
getEditSynthesizeSource c =
  \case
    AddColumn ci Fixed ->
      "let c" <> f ci <> ": Column<Any> = (*(config.fixed_columns.get(&ColumnIndex { index: "
        <> f ci <> " }).unwrap())).into();"
    AddColumn ci Advice ->
      "let c" <> f ci <> ": Column<Any> = (*(config.advice_columns.get(&ColumnIndex { index: "
        <> f ci <> " }).unwrap())).into();"
    AddColumn ci Instance ->
      "let c" <> f ci <> ": Column<Any> = (*(config.instance_columns.get(&ColumnIndex { index: "
        <> f ci <> " }).unwrap())).into();"
    AddEqualityConstraint eq ->
      getAddEqualityConstraintSource eq
    AddFixedColumn ci fvs ->
      getAddFixedColumnSource c ci fvs
    EnableEquality {} -> mempty
    AddGate {} -> mempty
    AddLookupTable {} -> mempty
    AddLookupArgument {} -> mempty


f :: Show a => a -> ByteString
f = encodeUtf8 . pack . show

getTableName :: [LookupTableColumn] -> ByteString
getTableName =
  ("t_" <>) . BS.intercalate "_" . fmap f

getLookupTable :: LookupArgument Polynomial -> [LookupTableColumn]
getLookupTable = fmap snd . (^. #tableMap)

getAddLookupArgumentSource :: LookupArgument Polynomial -> ByteString
getAddLookupArgumentSource arg =
  "meta.lookup_dynamic(&"
    <> getTableName (getLookupTable arg)
    <> ", |meta| {\n"
    <> BS.intercalate "\n"
         (("       " <>) . getColumnRotationSource
           <$> Set.toList (getPolynomialVariables arg)) <> "\n"
    <> "        (selector_all, vec!["
    <> mconcat
         [ "            (" <> getPolySource p
                     <> ", c" <> f c <> ".into()),"
           | (InputExpression p, LookupTableColumn c)
               <- arg ^. #tableMap
         ]
    <> "        ])"
    <> "    });"


getAddGateSource :: Label -> Polynomial -> ByteString
getAddGateSource l p =
  "meta.create_gate(" <>  f l <> ", |meta| {\n"
    <> BS.intercalate "\n"
         (("       " <>) . getColumnRotationSource
           <$> Set.toList (getPolynomialVariables p)) <> "\n"
    <> "        Constraints::with_selector(Expression::Constant(Field::ZERO), [\n"
    <> "            Constraint::from(" <> getPolySource p <> ")\n"
    <> "        ])\n"
    <> "    });"

getPolySource :: Polynomial -> ByteString
getPolySource p =
  BS.intercalate " + " (uncurry getMonoSource <$> Map.toList (p ^. #monos))

getMonoSource :: PowerProduct -> Coefficient -> ByteString
getMonoSource pp c
  | null (pp ^. #getPowerProduct) = getCoefficientSource c
  | otherwise = getCoefficientSource c <> " * " <> getPowerProductSource pp

getCoefficientSource :: Coefficient -> ByteString
getCoefficientSource c =
  "Expression::Constant(PrimeField::from_str_vartime(\"" <> f c <> "\").unwrap())"

getPowerProductSource :: PowerProduct -> ByteString
getPowerProductSource pp =
  BS.intercalate " * " $ uncurry getPowerSource <$> Map.toList (pp ^. #getPowerProduct)

getPowerSource :: PolynomialVariable -> Exponent -> ByteString
getPowerSource v e =
  BS.intercalate "*" $ replicate (e ^. #getExponent) (getPolyVarSource v)

getPolyVarSource :: PolynomialVariable -> ByteString
getPolyVarSource v =
  "r" <> f (v ^. #colIndex) <> "_" <> f (v ^. #rowIndex) <> ".clone()"

getColumnRotationSource :: PolynomialVariable -> ByteString
getColumnRotationSource (PolynomialVariable ci 0) =
  "let r" <> f ci <> "_0" <>
    " = meta.query_any(c" <> f ci <>
    ", Rotation(0));"
getColumnRotationSource _ = die "Halo2 codegen: cannot handle rotations (this is a compiler bug)"

getLookupTableColumnsSource :: [LookupTableColumn] -> ByteString
getLookupTableColumnsSource cs =
  ("&[" <>) . (<> "]") . BS.intercalate ", " $
    ("c" <>) . f . (^. #unLookupTableColumn)
      <$> cs

getAddEqualityConstraintSource :: Set.Set CellReference -> ByteString
getAddEqualityConstraintSource cs =
  "equality_constraints.push(vec!["
    <> BS.intercalate ", "
         ((\(CellReference (ColumnIndex ci) (RowIndex ri)) ->
           "Cell { region_index: ri, row_offset: " <> encodeUtf8 (pack (show ri))
             <> ", column: c" <> f ci <> " }")
           <$> Set.toList cs)
    <> "]);"


getAddFixedColumnSource :: ArithmeticCircuit -> ColumnIndex -> Map.Map (RowIndex Absolute) Scalar -> ByteString
getAddFixedColumnSource c ci xs
  | Map.keys xs == [0 .. RowIndex (nRows - 1)] =
    "fixed_values.insert(ColumnIndex {index:"
      <> encodeUtf8 (pack (show ci))
      <> "}, vec!["
      <> BS.intercalate ","
           ((<> ")") . ("F::from(" <>) . encodeUtf8 . pack . show
             <$> Map.elems xs)
      <> "]);"
  | otherwise =
     die "getAddFixedColumnSource: not enough rows in fixed column (this is a compiler bug)"    
  where
    nRows = scalarToInt $ c ^. #rowCount . #getRowCount

writeStaticFile :: FilePath -> ByteString -> TargetDirectory -> IO ()
writeStaticFile filename contents (TargetDirectory targetDir) =
  BS.writeFile (targetDir <> "/" <> filename) contents
