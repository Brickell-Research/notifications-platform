import envoy
import gleam/bit_array
import gleam/crypto
import gleam/dynamic/decode
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import wisp

// --- API Key Authentication ---

pub fn verify_api_key(req: wisp.Request) -> Bool {
  let api_key = envoy.get("API_KEY") |> result.unwrap("")

  case api_key {
    "" -> False
    key -> {
      case list.key_find(req.headers, "x-api-key") {
        Ok(provided_key) ->
          crypto.secure_compare(
            bit_array.from_string(key),
            bit_array.from_string(provided_key),
          )
        Error(_) -> False
      }
    }
  }
}

// --- CSRF Protection ---

const csrf_cookie_name = "_csrf_token"

pub fn generate_csrf_token() -> String {
  wisp.random_string(32)
}

pub fn set_csrf_cookie(
  req: wisp.Request,
  response: wisp.Response,
  token: String,
) -> wisp.Response {
  wisp.set_cookie(response, req, csrf_cookie_name, token, wisp.Signed, 3600)
}

pub fn get_csrf_token_from_cookie(req: wisp.Request) -> Result(String, Nil) {
  wisp.get_cookie(req, csrf_cookie_name, wisp.Signed)
}

pub fn validate_csrf(req: wisp.Request, form_token: String) -> Bool {
  case get_csrf_token_from_cookie(req) {
    Error(_) -> False
    Ok(cookie_token) ->
      crypto.secure_compare(
        bit_array.from_string(cookie_token),
        bit_array.from_string(form_token),
      )
  }
}

// --- Cloudflare Turnstile ---

pub type TurnstileResult {
  TurnstileSuccess
  TurnstileFailure(String)
  TurnstileNotConfigured
}

pub fn verify_turnstile(token: String, remote_ip: String) -> TurnstileResult {
  let secret = envoy.get("TURNSTILE_SECRET_KEY") |> result.unwrap("")

  case secret, token {
    // No secret configured - skip verification
    "", _ -> TurnstileNotConfigured
    // Secret configured but no token provided - skip (widget may not have loaded)
    _, "" -> TurnstileNotConfigured
    _, _ -> {
      let body =
        json.object([
          #("secret", json.string(secret)),
          #("response", json.string(token)),
          #("remoteip", json.string(remote_ip)),
        ])
        |> json.to_string

      case
        request.to("https://challenges.cloudflare.com/turnstile/v0/siteverify")
      {
        Error(_) -> TurnstileFailure("Failed to create request")
        Ok(req) -> {
          let req =
            req
            |> request.set_method(http.Post)
            |> request.prepend_header("content-type", "application/json")
            |> request.set_body(body)

          case httpc.send(req) {
            Error(_) -> TurnstileFailure("Failed to verify with Cloudflare")
            Ok(resp) -> {
              // Parse JSON response using json.parse
              case json.parse(resp.body, turnstile_decoder()) {
                Ok(result) ->
                  case result.success {
                    True -> TurnstileSuccess
                    False ->
                      TurnstileFailure(
                        "Verification failed: "
                        <> string.join(result.error_codes, ", "),
                      )
                  }
                Error(_) -> TurnstileFailure("Failed to parse response")
              }
            }
          }
        }
      }
    }
  }
}

type TurnstileResponse {
  TurnstileResponse(success: Bool, error_codes: List(String))
}

fn turnstile_decoder() -> decode.Decoder(TurnstileResponse) {
  use success <- decode.field("success", decode.bool)
  use error_codes <- decode.optional_field(
    "error-codes",
    [],
    decode.list(decode.string),
  )
  decode.success(TurnstileResponse(success:, error_codes:))
}

// --- Honeypot Validation ---

pub fn check_honeypot(form_values: List(#(String, String))) -> Bool {
  case list.key_find(form_values, "website") {
    Ok("") -> True
    Ok(_) -> False
    Error(_) -> True
  }
}

// --- Client IP Extraction ---

pub fn get_client_ip(req: wisp.Request) -> String {
  case list.key_find(req.headers, "cf-connecting-ip") {
    Ok(ip) -> string.trim(ip)
    Error(_) ->
      case list.key_find(req.headers, "x-forwarded-for") {
        Ok(ips) ->
          string.split(ips, ",")
          |> list.first
          |> result.map(string.trim)
          |> result.unwrap("127.0.0.1")
        Error(_) ->
          case list.key_find(req.headers, "x-real-ip") {
            Ok(ip) -> string.trim(ip)
            Error(_) -> "127.0.0.1"
          }
      }
  }
}
