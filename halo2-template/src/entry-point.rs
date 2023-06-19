use warp::Filter;

pub async fn run_server() {
  let server = warp::path!("mock_prove")
      .and(warp::post())
      .and(warp::body::json())
      .map(|req: HashMap<String, String>| format!("Hello world!"));

  println!("starting OSL prover server on port 1727");
  warp::serve(server)
    .run(([127, 0, 0, 1], 1727))
    .await;
}
