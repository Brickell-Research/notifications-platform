import envoy
import gleam/bit_array
import gleam/result
import gleam/string

pub type TokenAction {
  Confirm
  Unsubscribe
}

pub type TokenError {
  InvalidToken
  InvalidSignature
  MissingSecret
}

/// Generate a signed token for the given email and action
pub fn generate(
  email: String,
  action: TokenAction,
) -> Result(String, TokenError) {
  use secret <- result.try(get_secret())

  let action_str = action_to_string(action)
  let payload = email <> ":" <> action_str
  let signature = hmac_sha256(secret, payload)
  let token = payload <> ":" <> signature

  Ok(base64_encode(token))
}

/// Verify and decode a token, returning the email if valid
pub fn verify(
  token: String,
  expected_action: TokenAction,
) -> Result(String, TokenError) {
  use secret <- result.try(get_secret())
  use decoded <- result.try(
    base64_decode(token)
    |> result.replace_error(InvalidToken),
  )

  // Parse: email:action:signature
  case string.split(decoded, ":") {
    [email, action_str, signature] -> {
      let expected_action_str = action_to_string(expected_action)
      case action_str == expected_action_str {
        False -> Error(InvalidToken)
        True -> {
          let payload = email <> ":" <> action_str
          let expected_signature = hmac_sha256(secret, payload)
          case signature == expected_signature {
            True -> Ok(email)
            False -> Error(InvalidSignature)
          }
        }
      }
    }
    _ -> Error(InvalidToken)
  }
}

fn get_secret() -> Result(String, TokenError) {
  envoy.get("TOKEN_SECRET")
  |> result.replace_error(MissingSecret)
}

fn action_to_string(action: TokenAction) -> String {
  case action {
    Confirm -> "confirm"
    Unsubscribe -> "unsubscribe"
  }
}

fn hmac_sha256(secret: String, data: String) -> String {
  let secret_bytes = bit_array.from_string(secret)
  let data_bytes = bit_array.from_string(data)
  let hash = do_hmac_sha256(secret_bytes, data_bytes)
  hex_encode(hash)
}

@external(erlang, "token_ffi", "hmac_sha256")
fn do_hmac_sha256(secret: BitArray, data: BitArray) -> BitArray

@external(erlang, "token_ffi", "hex_encode")
fn hex_encode(data: BitArray) -> String

@external(erlang, "token_ffi", "base64_encode")
fn base64_encode(data: String) -> String

@external(erlang, "token_ffi", "base64_decode")
fn base64_decode(data: String) -> Result(String, Nil)
