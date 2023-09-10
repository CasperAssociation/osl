#[derive(Serialize, Deserialize)]
pub struct ProverInputs<K, F> where K: Hash + Eq {
    pub instance_data: HashMap<K, Vec<F>>,
    pub advice_data: HashMap<K, Vec<F>>
}

#[derive(Serialize, Deserialize)]
pub struct VerifierInputs<K, F> where K: Hash + Eq {
    pub instance_data: HashMap<K, Vec<F>>,
    pub proof: Vec<u8>
}

pub async fn run_server() {
  let args: Vec<String> = env::args().collect();
  let port = u16::from_str(&args[1]).unwrap();

  let verifier_server = warp::path!("verify")
      .and(warp::post())
      .and(warp::body::json())
      .map(|req: VerifierInputs<String, [[u8; 8]; 8]>| {
          let mut instance_data: Vec<Vec<Fp>> = Vec::new();
          let mut instance_cols: Vec<&String> = req.instance_data.keys().collect();
          // TODO: optimize this sorting by pre-parsing the strings
          instance_cols.sort_by(|a, b| str::parse::<u64>(a).unwrap().cmp(&str::parse::<u64>(b).unwrap()));
          for i in instance_cols.iter() {
              let xs: &Vec<[[u8; 8]; 8]> = req.instance_data.get(*i).unwrap();
              instance_data.push(
                  xs.iter()
                    .map(|x: &[[u8; 8]; 8]| {
                           let mut x_flat: [u8; 64] = [0; 64];
                           for i in 0..8 {
                               for j in 0..8 {
                                   x_flat[i*8 + j] = (*x)[i][j];
                               }
                           }
                           FromUniformBytes::from_uniform_bytes(&x_flat)
                        }
                    ).collect()
              );
          };
          let circuit = MyCircuit { advice_data: None };
          let params: Params<EqAffine> =
              halo2_proofs::poly::commitment::Params::new(
                  // TODO: more precise row count
                  TryInto::<u32>::try_into(ROW_COUNT).unwrap().ilog2()+1 // TODO: no .unwrap()
              );
          let vk = keygen_vk(&params, &circuit).unwrap(); // TODO: no .unwrap()
          let pk = keygen_pk(&params, vk, &circuit).unwrap(); // TODO: no .unwrap()
          let mut transcript = Blake2bRead::init(&req.proof[..]);
          let res = verify_proof(&params,
                                 pk.get_vk(),
                                 SingleVerifier::new(&params),
                                 &[instance_data
                                    .iter()
                                    .map(|v| v.as_slice())
                                    .collect::<Vec<&[Fp]>>()
                                    .as_slice()
                                 ],
                                 &mut transcript);
          match res {
              Ok(_) => StatusCode::OK,
              Err(e) => StatusCode::NOT_ACCEPTABLE
          }
      });

  let mock_prover_server = warp::path!("mock_prove")
      .and(warp::post())
      .and(warp::body::json())
      // The reason for letting F = [[u8; 8]; 8] instead of F = [u8; 64] is so that
      // serde traits can be derived.
      .map(|req: ProverInputs<String, [[u8; 8]; 8]>| {
          let mut instance_data: Vec<Vec<Fp>> = Vec::new();
          let mut advice_data: HashMap<ColumnIndex, Vec<Fp>> = HashMap::new();
          let mut instance_cols: Vec<&String> = req.instance_data.keys().collect();
          // TODO: optimize this sorting by pre-parsing the strings
          instance_cols.sort_by(|a, b| str::parse::<u64>(a).unwrap().cmp(&str::parse::<u64>(b).unwrap()));
          for i in instance_cols.iter() {
              let xs: &Vec<[[u8; 8]; 8]> = req.instance_data.get(*i).unwrap();
              instance_data.push(
                  xs.iter()
                    .map(|x: &[[u8; 8]; 8]| {
                           let mut x_flat: [u8; 64] = [0; 64];
                           for i in 0..8 {
                               for j in 0..8 {
                                   x_flat[i*8 + j] = (*x)[i][j];
                               }
                           }
                           FromUniformBytes::from_uniform_bytes(&x_flat)
                        }
                    ).collect()
              );
          };
          for (i, xs) in req.advice_data.iter() {
              advice_data.insert
                  (ColumnIndex { index: str::parse::<u64>(i).unwrap() },
                   xs.iter()
                       .map(|x: &[[u8; 8]; 8]| {
                           let mut x_flat: [u8; 64] = [0; 64];
                           for i in 0..8 {
                               for j in 0..8 {
                                   x_flat[i*8 + j] = (*x)[i][j];
                               }
                           }
                           let x = FromUniformBytes::from_uniform_bytes(&x_flat);
                           x
                       }).collect());
          };
          let circuit = MyCircuit { advice_data: Some(advice_data) };
          let prover =
              MockProver::run(
                  // TODO: more precise row count
                  TryInto::<u64>::try_into(ROW_COUNT).unwrap().ilog2()+1, // TODO: no .unwrap()
                  &circuit,
                  instance_data
              ).unwrap();
          assert_eq!(prover.verify(), Ok(()));
          format!("Ran mock prover; system is satisfied!")
      });

  let real_prover_server = warp::path!("prove")
      .and(warp::post())
      .and(warp::body::json())
      .map(|req: ProverInputs<String, [[u8; 8]; 8]>| {
          // TODO: factor out common code in mock_prover_server and real_prover_server
          let mut instance_data: Vec<Vec<Fp>> = Vec::new();
          let mut advice_data: HashMap<ColumnIndex, Vec<Fp>> = HashMap::new();
          let mut instance_cols: Vec<&String> = req.instance_data.keys().collect();
          // TODO: optimize this sorting by pre-parsing the strings
          instance_cols.sort_by(|a, b| str::parse::<u64>(a).unwrap().cmp(&str::parse::<u64>(b).unwrap()));
          for i in instance_cols.iter() {
              let xs: &Vec<[[u8; 8]; 8]> = req.instance_data.get(*i).unwrap();
              instance_data.push(
                  xs.iter()
                    .map(|x: &[[u8; 8]; 8]| {
                           let mut x_flat: [u8; 64] = [0; 64];
                           for i in 0..8 {
                               for j in 0..8 {
                                   x_flat[i*8 + j] = (*x)[i][j];
                               }
                           }
                           FromUniformBytes::from_uniform_bytes(&x_flat)
                        }
                    ).collect()
              );
          };
          for (i, xs) in req.advice_data.iter() {
              advice_data.insert
                  (ColumnIndex { index: str::parse::<u64>(i).unwrap() },
                   xs.iter()
                       .map(|x: &[[u8; 8]; 8]| {
                           let mut x_flat: [u8; 64] = [0; 64];
                           for i in 0..8 {
                               for j in 0..8 {
                                   x_flat[i*8 + j] = (*x)[i][j];
                               }
                           }
                           let x = FromUniformBytes::from_uniform_bytes(&x_flat);
                           x
                       }).collect());
          };
          let circuit = MyCircuit { advice_data: Some(advice_data) };
          let params: Params<EqAffine> =
              halo2_proofs::poly::commitment::Params::new(
                  // TODO: more precise row count
                  TryInto::<u32>::try_into(ROW_COUNT).unwrap().ilog2()+1 // TODO: no .unwrap()
              );
          let vk = keygen_vk(&params, &circuit).unwrap(); // TODO: no .unwrap()
          let pk = keygen_pk(&params, vk, &circuit).unwrap(); // TODO: no .unwrap()
          let mut transcript = Blake2bWrite::<_, vesta::Affine, _>::init(vec![]);
          create_proof(
              &params,
              &pk,
              &[circuit],
              &[instance_data
                    .iter()
                    .map(|v| v.as_slice())
                    .collect::<Vec<&[Fp]>>()
                    .as_slice()
              ],
              &mut OsRng,
              &mut transcript,
            )
            .expect("Failed to create proof");

          transcript.finalize()
      });

  let server = verifier_server
      .or(mock_prover_server)
      .or(real_prover_server);

  println!("starting OSL prover server on port {}", port);
  warp::serve(server)
    .run(([127, 0, 0, 1], port))
    .await;
}
