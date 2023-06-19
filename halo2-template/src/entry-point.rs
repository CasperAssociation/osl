#[derive(Serialize, Deserialize)]
pub struct ProverInputs<F> {
    pub instance_data: HashMap<ColumnIndex, Vec<F>>,
    pub advice_data: HashMap<ColumnIndex, Vec<F>>
}

pub async fn run_server() {
  let server = warp::path!("mock_prove")
      .and(warp::post())
      .and(warp::body::json())
      .map(|req: ProverInputs<[[u8; 8]; 8]>| {
          let mut instance_data: Vec<Vec<Fp>> = Vec::new();
          let mut advice_data: HashMap<ColumnIndex, Vec<Fp>> = HashMap::new();
          let mut instance_cols: Vec<ColumnIndex> =
              req.instance_data.keys().map(|x| *x).collect();
          instance_cols.sort();
          for i in instance_cols.iter() {
              let xs: &Vec<[[u8; 8]; 8]> = req.instance_data.get(i).unwrap();
              instance_data.push(
                  xs.iter()
                    .map(|x: &[[u8; 8]; 8]| {
                           let mut x_flat: [u8; 64] = [0; 64];
                           for i in 0..7 {
                               for j in 0..7 {
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
                  (*i,
                   xs.iter()
                   .map(|x: &[[u8; 8]; 8]| {
                       let mut x_flat: [u8; 64] = [0; 64];
                       for i in 0..7 {
                           for j in 0..7 {
                               x_flat[i*8 + j] = (*x)[i][j];
                           }
                       }
                       FromUniformBytes::from_uniform_bytes(&x_flat)
                   }).collect());
          };
          let circuit = MyCircuit { advice_data: Some(advice_data) };
          let prover =
              MockProver::run(
                  ROW_COUNT.try_into().unwrap(),
                  &circuit,
                  instance_data
              ).unwrap();
          assert_eq!(prover.verify(), Ok(()));
          format!("Ran mock prover; system is satisfied!")
      });

  println!("starting OSL prover server on port 1727");
  warp::serve(server)
    .run(([127, 0, 0, 1], 1727))
    .await;
}
