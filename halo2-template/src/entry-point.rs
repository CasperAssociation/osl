#[derive(Serialize, Deserialize)]
pub struct ProverInputs<K, F> where K: Hash + Eq {
    pub instance_data: HashMap<K, Vec<F>>,
    pub advice_data: HashMap<K, Vec<F>>
}

pub async fn run_server() {
  let args: Vec<String> = env::args().collect();
  let port = u16::from_str(&args[1]).unwrap();
  let server = warp::path!("mock_prove")
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
                  TryInto::<u64>::try_into(ROW_COUNT).unwrap().ilog2()+1,
                  &circuit,
                  instance_data
              ).unwrap();
          assert_eq!(prover.verify(), Ok(()));
          format!("Ran mock prover; system is satisfied!")
      });

  println!("starting OSL prover server on port {}", port);
  warp::serve(server)
    .run(([127, 0, 0, 1], port))
    .await;
}
