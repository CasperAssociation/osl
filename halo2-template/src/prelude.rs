use ff::PrimeField;
use halo2_proofs::{
  arithmetic::Field,
  circuit::{SimpleFloorPlanner, Layouter, Cell, RegionIndex},
  poly::Rotation,
  plonk::{Advice, Fixed, Instance, Circuit, Column, Constraint, Constraints, ConstraintSystem, Error, Expression},
};
use pasta_curves::Fp;
use std::collections::HashMap;
use std::cmp::{PartialEq, Eq};
use std::hash::Hash;
use std::option::{Option, Option::None};

#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Clone)]
#[derive(Hash)]
pub struct ColumnIndex {
  index: u64
}

pub struct RowIndex {
  index: u64
}

#[derive(Clone)]
pub struct MyConfig {
  instance_columns: HashMap<ColumnIndex, Column<Instance>>,
  advice_columns: HashMap<ColumnIndex, Column<Advice>>,
  fixed_columns: HashMap<ColumnIndex, Column<Fixed>>
}

pub const ROW_COUNT: u64 = 1; // TODO: set correct row count

#[derive(Clone)]
pub struct MyCircuit {
  instance_data: Option<HashMap<ColumnIndex, Vec<Fp>>>,
  advice_data: Option<HashMap<ColumnIndex, Vec<Fp>>>
}

impl<F: PrimeField> Circuit<F> for MyCircuit {
  type Config = MyConfig;
  type FloorPlanner = SimpleFloorPlanner;

  fn without_witnesses(&self) -> Self {
    MyCircuit {instance_data: None, advice_data: None}
  }

  fn configure(meta: &mut ConstraintSystem<F>) -> Self::Config {
    let selector_all = meta.selector();
    let mut instance_cols = HashMap::new();
    let mut advice_cols = HashMap::new();
    let mut fixed_cols = HashMap::new();
    // TODO: enable selector_all on all rows

