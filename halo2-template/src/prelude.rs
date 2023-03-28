use ff::PrimeField;
use halo2_proofs::{
  arithmetic::Field,
  circuit::{SimpleFloorPlanner, Layouter},
  poly::Rotation,
  plonk::{Circuit, Constraints, Constraint, ConstraintSystem, Error, Expression, VirtualCells},
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
    let cs = VirtualCells::new(meta);

