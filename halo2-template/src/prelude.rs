use ff::PrimeField;
use halo2_proofs::{
  arithmetic::Field,
  circuit::{SimpleFloorPlanner, Layouter},
  poly::Rotation,
  plonk::{Circuit, Constraint, Constraints, ConstraintSystem, Error, Expression},
};

pub struct MyCircuit {}

#[derive(Clone)]
pub struct MyConfig {}

impl<F: PrimeField> Circuit<F> for MyCircuit {
  type Config = MyConfig;
  type FloorPlanner = SimpleFloorPlanner;

  fn without_witnesses(&self) -> Self {
    MyCircuit {}
  }

  fn synthesize(&self, _config: Self::Config, _layouter: impl Layouter<F>) -> Result<(), Error> {
    Ok(())
  }

  fn configure(meta: &mut ConstraintSystem<F>) -> Self::Config {
    let selector_all = meta.selector();
    // TODO: enable selector_all on all rows

