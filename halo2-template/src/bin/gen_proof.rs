use halo2_proofs::{
    plonk::{create_proof, keygen_pk, keygen_vk},
    poly::commitment::Params,
    transcript::Blake2bWrite,
};
use pasta_curves::{vesta, EqAffine};
use rand_core::OsRng;
use std::{env, fs::File, io::prelude::*, path::Path};

use halo2_example::*;

fn main() {
    use halo2_proofs::pasta::Fp;
    let args: Vec<String> = env::args().collect();

    // The number of rows in our circuit cannot exceed 2^k. Since our example
    // circuit is very small, we can pick a very small value here.
    let k = 4;

    // Prepare the private and public inputs to the circuit!
    let a = Fp::from(args[1].parse::<u64>().unwrap());
    let b = Fp::from(args[2].parse::<u64>().unwrap());
    let c = a * b;

    // Instantiate the circuit with the private inputs.
    let circuit = MyCircuit {
        a: Some(a),
        b: Some(b),
    };

    let params: Params<EqAffine> = halo2_proofs::poly::commitment::Params::new(k);
    let vk = keygen_vk(&params, &circuit).unwrap();
    let proof_path = "./proof";

    let pk = keygen_pk(&params, vk, &circuit).unwrap();
    let mut transcript = Blake2bWrite::<_, vesta::Affine, _>::init(vec![]);

    create_proof(
        &params,
        &pk,
        &[circuit],
        &[&[&[c]]],
        &mut OsRng,
        &mut transcript,
    )
    .expect("Failed to create proof");

    let proof = transcript.finalize();
    File::create(Path::new(proof_path))
        .expect("Failed to create proof file")
        .write_all(&proof[..])
        .expect("Failed to write proof");
    println!("proof written to {}", proof_path);
}
