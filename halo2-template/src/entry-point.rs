use serde::{Serialize, Deserialize};
use warp::Filter;
use pasta_curves::Fp;

#[derive(Serialize, Deserialize)]
pub struct ProverInputs<F> {
    pub instance_data: HashMap<ColumnIndex, Vec<F>>,
    pub advice_data: HashMap<ColumnIndex, Vec<F>>
}

pub async fn run_server() {
  let server = warp::path!("mock_prove")
      .and(warp::post())
      .and(warp::body::json())
      .map(|req: ProverInputs<Fp>| format!("Hello world!"));

  println!("starting OSL prover server on port 1727");
  warp::serve(server)
    .run(([127, 0, 0, 1], 1727))
    .await;
}
