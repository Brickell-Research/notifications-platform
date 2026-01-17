import envoy
import gleam/result

pub type SmtpConfig {
  SmtpConfig(
    host: String,
    port: Int,
    username: String,
    password: String,
    from_email: String,
  )
}

pub type SmtpError {
  ConfigError(String)
  SendError(String)
}

/// Load SMTP configuration from environment variables
pub fn load_config() -> Result(SmtpConfig, SmtpError) {
  use host <- result.try(
    envoy.get("SMTP_HOST")
    |> result.replace_error(ConfigError("SMTP_HOST not set")),
  )
  use port_str <- result.try(
    envoy.get("SMTP_PORT")
    |> result.replace_error(ConfigError("SMTP_PORT not set")),
  )
  use username <- result.try(
    envoy.get("SMTP_USERNAME")
    |> result.replace_error(ConfigError("SMTP_USERNAME not set")),
  )
  use password <- result.try(
    envoy.get("SMTP_PASSWORD")
    |> result.replace_error(ConfigError("SMTP_PASSWORD not set")),
  )
  use from_email <- result.try(
    envoy.get("SMTP_FROM_EMAIL")
    |> result.replace_error(ConfigError("SMTP_FROM_EMAIL not set")),
  )

  case int_parse(port_str) {
    Ok(port) -> Ok(SmtpConfig(host:, port:, username:, password:, from_email:))
    Error(_) -> Error(ConfigError("SMTP_PORT must be a number"))
  }
}

/// Send a plain text email via SMTP
pub fn send_email(
  config: SmtpConfig,
  to: String,
  subject: String,
  body: String,
) -> Result(Nil, SmtpError) {
  // Build the email message in RFC 2822 format
  let message =
    "From: "
    <> config.from_email
    <> "\r\n"
    <> "To: "
    <> to
    <> "\r\n"
    <> "Subject: "
    <> subject
    <> "\r\n"
    <> "Content-Type: text/plain; charset=utf-8\r\n"
    <> "\r\n"
    <> body

  do_send_email(
    config.host,
    config.port,
    config.username,
    config.password,
    config.from_email,
    to,
    message,
  )
}

/// Send a multipart email with both plain text and HTML versions
pub fn send_email_multipart(
  config: SmtpConfig,
  to: String,
  subject: String,
  text_body: String,
  html_body: String,
) -> Result(Nil, SmtpError) {
  // Use a fixed boundary - unique enough for our purposes
  let boundary = "----=_NextPart_Brickell_001"

  // Build multipart/alternative MIME message
  let message =
    "From: "
    <> config.from_email
    <> "\r\n"
    <> "To: "
    <> to
    <> "\r\n"
    <> "Subject: "
    <> subject
    <> "\r\n"
    <> "MIME-Version: 1.0\r\n"
    <> "Content-Type: multipart/alternative; boundary=\""
    <> boundary
    <> "\"\r\n"
    <> "\r\n"
    <> "--"
    <> boundary
    <> "\r\n"
    <> "Content-Type: text/plain; charset=utf-8\r\n"
    <> "Content-Transfer-Encoding: 8bit\r\n"
    <> "\r\n"
    <> text_body
    <> "\r\n"
    <> "\r\n"
    <> "--"
    <> boundary
    <> "\r\n"
    <> "Content-Type: text/html; charset=utf-8\r\n"
    <> "Content-Transfer-Encoding: 8bit\r\n"
    <> "\r\n"
    <> html_body
    <> "\r\n"
    <> "\r\n"
    <> "--"
    <> boundary
    <> "--\r\n"

  do_send_email(
    config.host,
    config.port,
    config.username,
    config.password,
    config.from_email,
    to,
    message,
  )
}

@external(erlang, "smtp_ffi", "send_email")
fn do_send_email(
  host: String,
  port: Int,
  username: String,
  password: String,
  from: String,
  to: String,
  message: String,
) -> Result(Nil, SmtpError)

@external(erlang, "gleam_stdlib", "parse_int")
fn int_parse(string: String) -> Result(Int, Nil)
