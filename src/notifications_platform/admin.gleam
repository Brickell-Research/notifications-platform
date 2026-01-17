import envoy
import gleam/bit_array
import gleam/crypto
import gleam/result
import wisp

pub type AdminConfig {
  AdminConfig(email: String, password: String)
}

pub fn load_config() -> Result(AdminConfig, String) {
  use email <- result.try(
    envoy.get("ADMIN_EMAIL")
    |> result.replace_error("ADMIN_EMAIL not set"),
  )
  use password <- result.try(
    envoy.get("ADMIN_PASSWORD")
    |> result.replace_error("ADMIN_PASSWORD not set"),
  )
  Ok(AdminConfig(email:, password:))
}

pub fn verify_credentials(
  config: AdminConfig,
  email: String,
  password: String,
) -> Bool {
  let email_match =
    crypto.secure_compare(
      bit_array.from_string(config.email),
      bit_array.from_string(email),
    )
  let password_match =
    crypto.secure_compare(
      bit_array.from_string(config.password),
      bit_array.from_string(password),
    )
  email_match && password_match
}

const session_cookie_name = "admin_session"

const session_max_age = 86_400

pub fn create_session(
  req: wisp.Request,
  response: wisp.Response,
) -> wisp.Response {
  let session_token = wisp.random_string(32)
  wisp.set_cookie(
    response,
    req,
    session_cookie_name,
    session_token,
    wisp.Signed,
    session_max_age,
  )
}

pub fn is_authenticated(req: wisp.Request) -> Bool {
  case wisp.get_cookie(req, session_cookie_name, wisp.Signed) {
    Ok(_) -> True
    Error(_) -> False
  }
}

pub fn clear_session(
  req: wisp.Request,
  response: wisp.Response,
) -> wisp.Response {
  wisp.set_cookie(response, req, session_cookie_name, "", wisp.Signed, 0)
}
