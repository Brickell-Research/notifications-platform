import gleam/erlang/process
import gleam/int
import gleam/io
import mist
import wisp
import wisp/wisp_mist

const port = 8088

pub fn main() {
  wisp.configure_logger()
  let secret_key_base = wisp.random_string(64)

  let assert Ok(_) =
    wisp_mist.handler(handle_request, secret_key_base)
    |> mist.new
    |> mist.port(port)
    |> mist.bind("0.0.0.0")
    |> mist.start

  io.println("Server running on http://localhost:" <> int.to_string(port))
  process.sleep_forever()
}

fn handle_request(req: wisp.Request) -> wisp.Response {
  case wisp.path_segments(req) {
    [] -> wisp.ok() |> wisp.string_body("Hello from notifications_platform!")
    ["health"] -> wisp.ok() |> wisp.string_body("OK")
    _ -> wisp.not_found()
  }
}
