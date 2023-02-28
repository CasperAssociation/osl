use halo2_proofs::{
    plonk::{keygen_pk, keygen_vk, verify_proof, SingleVerifier},
    poly::commitment::Params,
    transcript::Blake2bRead,
};
use pasta_curves::EqAffine;
use std::{env, fs::File, io::prelude::*, path::Path};

use halo2_example::*;

fn main() {
    use halo2_proofs::pasta::Fp;
    let args: Vec<String> = env::args().collect();
    let k = 4;
    let c = Fp::from(args[1].parse::<u64>().unwrap());
    let circuit = MyCircuit::default();
    let params: Params<EqAffine> = halo2_proofs::poly::commitment::Params::new(k);
    // we recompute it since serialization is currently broken
    let vk = keygen_vk(&params, &circuit).unwrap();
    let pk = keygen_pk(&params, vk, &circuit).unwrap();
    let proof_path = "./proof";
    let mut proof_file = File::open(Path::new(proof_path)).expect("couldn't read proof from file");

    let mut proof = Vec::<u8>::new();
    proof_file
        .read_to_end(&mut proof)
        .expect("Couldn't read proof");

    let mut transcript = Blake2bRead::init(&proof[..]);

    println!(
        "{:?}",
        verify_proof(
            &params,
            pk.get_vk(),
            SingleVerifier::new(&params),
            &[&[&[c]]],
            &mut transcript
        )
    );
}
