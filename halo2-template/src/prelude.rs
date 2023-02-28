use halo2_proofs::{
  arithmetic::Field,
  circuit::{SimpleFloorPlanner},
  plonk::{Circuit, ConstraintSystem},
};

pub struct MyCircuit<F: Field> {}
pub struct MyConfig {}

impl<F: Field> Circuit<F> for MyCircuit<F> {
  type Config = MyConfig;
  type FloorPlanner = SimpleFloorPlanner;

  fn without_witnesses(&self) -> Self {
    Self::default()
  }

  fn configure(meta: &mut ConstraintSystem<F>) -> Self::Config {
