use ff::PrimeField;
use halo2_proofs::{
  arithmetic::Field,
  circuit::{SimpleFloorPlanner, Layouter, Cell, RegionIndex, Value},
  poly::Rotation,
  plonk::{Advice, Assigned, Fixed, Instance, Circuit, Column, Constraint, Constraints, ConstraintSystem, Error, Expression, Selector},
};
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
  fixed_columns: HashMap<ColumnIndex, Column<Fixed>>,
  selector_all: Selector
}
