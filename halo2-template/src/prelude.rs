use die::die;
use ff::{FromUniformBytes, PrimeField};
use halo2_proofs::{
  arithmetic::Field,
  circuit::{SimpleFloorPlanner, Layouter, Cell, RegionIndex, Value},
  dev::MockProver,
  poly::{ commitment::Params, Rotation },
  plonk::{Advice, Any, Assigned, Fixed, Instance, Circuit, Column, Constraint, Constraints, ConstraintSystem, DynamicTable, Error, Expression, Selector, create_proof, keygen_pk, keygen_vk},
  transcript::Blake2bWrite,
};
use pasta_curves::{vesta, EqAffine};
use rand_core::OsRng;
use std::collections::HashMap;
use std::cmp::{PartialEq, Eq};
use std::env;
use std::hash::Hash;
use std::option::{Option, Option::None};
use std::str::FromStr;
use str;
use serde::{Serialize, Deserialize};
use warp::Filter;
use pasta_curves::Fp;
use derive_more::Display;

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash, Display)]
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
  selector_all: Selector,
  lookup_tables: Vec<DynamicTable>
}
