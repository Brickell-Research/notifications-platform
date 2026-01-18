import envoy
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/http
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mist
import notifications_platform/admin
import notifications_platform/email
import notifications_platform/rate_limit.{type RateLimiter}
import notifications_platform/security
import notifications_platform/smtp
import notifications_platform/sql
import notifications_platform/token
import notifications_platform/views
import pog
import wisp
import wisp/wisp_mist
import youid/uuid

const port = 8088

pub type Context {
  Context(
    db: pog.Connection,
    admin_config: admin.AdminConfig,
    rate_limiter: Option(RateLimiter),
  )
}

pub fn main() {
  wisp.configure_logger()

  // Load secret_key_base from env (persistent across restarts)
  let secret_key_base = case envoy.get("SECRET_KEY_BASE") {
    Ok(key) if key != "" -> key
    _ -> {
      io.println(
        "WARNING: SECRET_KEY_BASE not set, using random key (sessions will not persist across restarts)",
      )
      wisp.random_string(64)
    }
  }

  // Connect to the database
  let db = connect_to_database()

  // Load admin config
  let assert Ok(admin_config) = admin.load_config()

  // Start rate limiter
  let rate_limiter = case rate_limit.start() {
    Ok(limiter) -> Some(limiter)
    Error(_) -> {
      io.println("WARNING: Failed to start rate limiter")
      None
    }
  }

  let ctx = Context(db:, admin_config:, rate_limiter:)

  let handler = fn(req) { handle_request(req, ctx) }

  let assert Ok(_) =
    wisp_mist.handler(handler, secret_key_base)
    |> mist.new
    |> mist.port(port)
    |> mist.bind("0.0.0.0")
    |> mist.start

  io.println("Server running on http://localhost:" <> int.to_string(port))
  process.sleep_forever()
}

fn connect_to_database() -> pog.Connection {
  let database_url =
    envoy.get("DATABASE_URL")
    |> result.unwrap(
      "postgres://postgres:postgres@localhost:5432/notifications_platform",
    )

  let pool_name = process.new_name("notifications_db_pool")
  let assert Ok(config) = pog.url_config(pool_name, database_url)
  // Limit pool size to avoid exhausting DO managed database connections
  let config = pog.pool_size(config, 5)
  let assert Ok(started) = pog.start(config)
  started.data
}

// CORS allowed origins
const allowed_origins = [
  "https://caffeine-lang.run",
  "http://localhost:8080",
  "https://brickellresearch.org",
]

fn add_cors_headers(req: wisp.Request, response: wisp.Response) -> wisp.Response {
  let origin = list.key_find(req.headers, "origin") |> result.unwrap("")
  let allow_origin = case list.contains(allowed_origins, origin) {
    True -> origin
    False -> ""
  }
  response
  |> wisp.set_header("access-control-allow-origin", allow_origin)
  |> wisp.set_header("access-control-allow-methods", "POST, OPTIONS")
  |> wisp.set_header("access-control-allow-headers", "content-type")
}

fn add_security_headers(response: wisp.Response) -> wisp.Response {
  response
  |> wisp.set_header("x-content-type-options", "nosniff")
  |> wisp.set_header("x-frame-options", "DENY")
  |> wisp.set_header("x-xss-protection", "1; mode=block")
  |> wisp.set_header("referrer-policy", "strict-origin-when-cross-origin")
  |> wisp.set_header(
    "content-security-policy",
    "default-src 'self'; script-src 'self' 'unsafe-inline'; style-src 'self' 'unsafe-inline'; img-src 'self' data: https://brickellresearch.org; frame-ancestors 'none';",
  )
  |> wisp.set_header(
    "strict-transport-security",
    "max-age=31536000; includeSubDomains",
  )
}

fn handle_request(req: wisp.Request, ctx: Context) -> wisp.Response {
  let response = route_request(req, ctx)
  log_request(req, response)
  add_security_headers(response)
}

fn log_request(req: wisp.Request, response: wisp.Response) -> Nil {
  let method = http.method_to_string(req.method)
  let path = "/" <> string.join(wisp.path_segments(req), "/")
  let status = int.to_string(response.status)
  let client_ip = security.get_client_ip(req)

  io.println("[" <> method <> "] " <> path <> " " <> status <> " - " <> client_ip)
}

fn audit_log(action: String, detail: String, client_ip: String) -> Nil {
  io.println("[AUDIT] " <> action <> " | " <> detail <> " | IP: " <> client_ip)
}

fn route_request(req: wisp.Request, ctx: Context) -> wisp.Response {
  case wisp.path_segments(req), req.method {
    [], http.Get ->
      wisp.ok() |> wisp.string_body("Hello from notifications_platform!")
    ["health"], http.Get -> wisp.ok() |> wisp.string_body("OK")

    // CORS preflight
    ["subscribe"], http.Options -> wisp.ok() |> add_cors_headers(req, _)
    ["unsubscribe"], http.Options -> wisp.ok() |> add_cors_headers(req, _)

    ["subscribe"], http.Post ->
      handle_subscribe(req, ctx) |> add_cors_headers(req, _)
    ["unsubscribe"], http.Post ->
      handle_unsubscribe(req, ctx) |> add_cors_headers(req, _)

    // Protected API endpoint - requires API key
    ["send"], http.Post -> {
      case security.verify_api_key(req) {
        True -> handle_send(req)
        False ->
          json.object([
            #("error", json.string("Unauthorized - invalid or missing API key")),
          ])
          |> json.to_string
          |> wisp.json_response(401)
      }
    }

    // Email confirmation and unsubscribe (token-based)
    ["confirm"], http.Get -> handle_confirm(req, ctx)
    ["unsubscribe"], http.Get -> handle_unsubscribe_link(req, ctx)

    // Static files
    ["static", ..rest], http.Get -> serve_static(rest)

    // Admin routes - login (unprotected)
    ["admin", "login"], http.Get -> show_login_page_with_csrf(req, None)
    ["admin", "login"], http.Post -> handle_login(req, ctx)
    ["admin", "logout"], http.Post -> handle_logout(req)

    // Admin routes - protected
    ["admin"], http.Get -> require_auth(req, fn() { show_dashboard() })
    ["admin", "subscribers"], http.Get ->
      require_auth(req, fn() { show_subscribers(ctx, None) })
    ["admin", "subscribers", "add"], http.Post ->
      require_auth(req, fn() { handle_admin_add_subscriber(req, ctx) })
    ["admin", "subscribers", "delete"], http.Post ->
      require_auth(req, fn() { handle_admin_delete_subscriber(req, ctx) })

    // Templates routes
    ["admin", "templates"], http.Get ->
      require_auth(req, fn() { show_templates(ctx, None) })
    ["admin", "templates", "new"], http.Get ->
      require_auth(req, fn() { show_template_new(None) })
    ["admin", "templates", "new"], http.Post ->
      require_auth(req, fn() { handle_template_create(req, ctx) })
    ["admin", "templates", "delete"], http.Post ->
      require_auth(req, fn() { handle_template_delete(req, ctx) })
    ["admin", "templates", id, "preview"], http.Get ->
      require_auth(req, fn() { handle_template_preview(ctx, id) })
    ["admin", "templates", id], http.Get ->
      require_auth(req, fn() { show_template_edit(ctx, id, None) })
    ["admin", "templates", id], http.Post ->
      require_auth(req, fn() { handle_template_update(req, ctx, id) })

    // Send routes
    ["admin", "send"], http.Get ->
      require_auth(req, fn() { show_send_page(ctx, None) })
    ["admin", "send"], http.Post ->
      require_auth(req, fn() { handle_admin_send(req, ctx) })

    // History route
    ["admin", "history"], http.Get ->
      require_auth(req, fn() { show_history(ctx) })

    // Analytics route
    ["admin", "analytics"], http.Get ->
      require_auth(req, fn() { show_analytics(req, ctx) })

    _, _ -> wisp.not_found()
  }
}

fn serve_static(_path: List(String)) -> wisp.Response {
  wisp.not_found()
}

fn require_auth(
  req: wisp.Request,
  handler: fn() -> wisp.Response,
) -> wisp.Response {
  case admin.is_authenticated(req) {
    True -> handler()
    False -> wisp.redirect("/admin/login")
  }
}

fn show_login_page_with_csrf(
  req: wisp.Request,
  error: Option(String),
) -> wisp.Response {
  let csrf_token = security.generate_csrf_token()
  views.login_page(error, csrf_token)
  |> wisp.html_response(200)
  |> security.set_csrf_cookie(req, _, csrf_token)
}

fn handle_login(req: wisp.Request, ctx: Context) -> wisp.Response {
  let client_ip = security.get_client_ip(req)

  // Rate limit: 5 login attempts per 5 minutes per IP
  case rate_limit.is_allowed(ctx.rate_limiter, "login:" <> client_ip, 5, 300) {
    False ->
      show_login_page_with_csrf(
        req,
        Some("Too many login attempts. Please try again later."),
      )
    True -> {
      use form <- wisp.require_form(req)

      let email = list.key_find(form.values, "email") |> result.unwrap("")
      let password = list.key_find(form.values, "password") |> result.unwrap("")

      // Validate CSRF token
      let csrf_token =
        list.key_find(form.values, "csrf_token") |> result.unwrap("")
      case security.validate_csrf(req, csrf_token) {
        False ->
          show_login_page_with_csrf(
            req,
            Some("Invalid request. Please try again."),
          )
        True -> {
          case admin.verify_credentials(ctx.admin_config, email, password) {
            True -> {
              audit_log("LOGIN_SUCCESS", email, client_ip)
              wisp.redirect("/admin")
              |> admin.create_session(req, _)
            }
            False -> {
              audit_log("LOGIN_FAILED", email, client_ip)
              show_login_page_with_csrf(req, Some("Invalid email or password"))
            }
          }
        }
      }
    }
  }
}

fn handle_logout(req: wisp.Request) -> wisp.Response {
  wisp.redirect("/admin/login")
  |> admin.clear_session(req, _)
}

fn show_dashboard() -> wisp.Response {
  views.admin_dashboard(views.Home)
  |> wisp.html_response(200)
}

fn show_subscribers(
  ctx: Context,
  message: option.Option(views.Message),
) -> wisp.Response {
  case sql.get_all_subscribers(ctx.db) {
    Ok(returned) ->
      views.subscribers_page(returned.rows, message)
      |> wisp.html_response(200)
    Error(_) -> wisp.internal_server_error()
  }
}

fn handle_admin_add_subscriber(req: wisp.Request, ctx: Context) -> wisp.Response {
  use form <- wisp.require_form(req)

  let email = list.key_find(form.values, "email") |> result.unwrap("")

  case email {
    "" -> show_subscribers(ctx, None)
    _ -> {
      // Admin-added subscribers are auto-confirmed
      case sql.admin_add_subscriber(ctx.db, email) {
        Ok(_) -> show_subscribers(ctx, Some(views.Success("Added " <> email)))
        Error(_) ->
          show_subscribers(ctx, Some(views.Error("Failed to add subscriber")))
      }
    }
  }
}

fn handle_admin_delete_subscriber(
  req: wisp.Request,
  ctx: Context,
) -> wisp.Response {
  use form <- wisp.require_form(req)
  let client_ip = security.get_client_ip(req)

  let email = list.key_find(form.values, "email") |> result.unwrap("")

  case email {
    "" -> show_subscribers(ctx, None)
    _ -> {
      case sql.unsubscribe(ctx.db, email) {
        Ok(returned) -> {
          case list.first(returned.rows) {
            Ok(_) -> {
              audit_log("DELETE_SUBSCRIBER", email, client_ip)
              show_subscribers(ctx, Some(views.Success("Removed " <> email)))
            }
            Error(_) ->
              show_subscribers(ctx, Some(views.Error("Subscriber not found")))
          }
        }
        Error(_) ->
          show_subscribers(
            ctx,
            Some(views.Error("Failed to remove subscriber")),
          )
      }
    }
  }
}

// --- Template handlers -------------------------------------------------------

fn show_templates(
  ctx: Context,
  message: option.Option(views.Message),
) -> wisp.Response {
  case sql.list_templates(ctx.db) {
    Ok(returned) ->
      views.templates_page(returned.rows, message)
      |> wisp.html_response(200)
    Error(_) -> wisp.internal_server_error()
  }
}

fn show_template_new(message: option.Option(views.Message)) -> wisp.Response {
  views.template_edit_page(None, message)
  |> wisp.html_response(200)
}

fn show_template_edit(
  ctx: Context,
  id: String,
  message: option.Option(views.Message),
) -> wisp.Response {
  case uuid.from_string(id) {
    Error(_) -> wisp.not_found()
    Ok(template_id) -> {
      case sql.get_template_by_id(ctx.db, template_id) {
        Ok(returned) -> {
          case list.first(returned.rows) {
            Ok(template) ->
              views.template_edit_page(Some(template), message)
              |> wisp.html_response(200)
            Error(_) -> wisp.not_found()
          }
        }
        Error(_) -> wisp.internal_server_error()
      }
    }
  }
}

fn handle_template_create(req: wisp.Request, ctx: Context) -> wisp.Response {
  use form <- wisp.require_form(req)

  let name = list.key_find(form.values, "name") |> result.unwrap("")
  let subject = list.key_find(form.values, "subject") |> result.unwrap("")
  let body = list.key_find(form.values, "body") |> result.unwrap("")

  case name, subject {
    "", _ -> show_template_new(Some(views.Error("Name is required")))
    _, "" -> show_template_new(Some(views.Error("Subject is required")))
    _, _ -> {
      case sql.create_template(ctx.db, name, subject, body) {
        Ok(_) ->
          show_templates(
            ctx,
            Some(views.Success("Template \"" <> name <> "\" created")),
          )
        Error(_) ->
          show_template_new(
            Some(views.Error(
              "Failed to create template (name may already exist)",
            )),
          )
      }
    }
  }
}

fn handle_template_update(
  req: wisp.Request,
  ctx: Context,
  id: String,
) -> wisp.Response {
  case uuid.from_string(id) {
    Error(_) -> wisp.not_found()
    Ok(template_id) -> {
      use form <- wisp.require_form(req)

      let name = list.key_find(form.values, "name") |> result.unwrap("")
      let subject = list.key_find(form.values, "subject") |> result.unwrap("")
      let body = list.key_find(form.values, "body") |> result.unwrap("")

      case name, subject {
        "", _ ->
          show_template_edit(ctx, id, Some(views.Error("Name is required")))
        _, "" ->
          show_template_edit(ctx, id, Some(views.Error("Subject is required")))
        _, _ -> {
          case sql.update_template(ctx.db, template_id, name, subject, body) {
            Ok(returned) -> {
              case list.first(returned.rows) {
                Ok(_) ->
                  show_template_edit(
                    ctx,
                    id,
                    Some(views.Success("Template saved")),
                  )
                Error(_) -> wisp.not_found()
              }
            }
            Error(_) ->
              show_template_edit(
                ctx,
                id,
                Some(views.Error("Failed to save template")),
              )
          }
        }
      }
    }
  }
}

fn handle_template_delete(req: wisp.Request, ctx: Context) -> wisp.Response {
  use form <- wisp.require_form(req)
  let client_ip = security.get_client_ip(req)

  let id = list.key_find(form.values, "id") |> result.unwrap("")

  case uuid.from_string(id) {
    Error(_) -> show_templates(ctx, Some(views.Error("Invalid template ID")))
    Ok(template_id) -> {
      case sql.delete_template(ctx.db, template_id) {
        Ok(returned) -> {
          case list.first(returned.rows) {
            Ok(_) -> {
              audit_log("DELETE_TEMPLATE", id, client_ip)
              show_templates(ctx, Some(views.Success("Template deleted")))
            }
            Error(_) ->
              show_templates(ctx, Some(views.Error("Template not found")))
          }
        }
        Error(_) ->
          show_templates(ctx, Some(views.Error("Failed to delete template")))
      }
    }
  }
}

fn handle_template_preview(ctx: Context, id: String) -> wisp.Response {
  case uuid.from_string(id) {
    Error(_) -> wisp.not_found()
    Ok(template_id) -> {
      case sql.get_template_by_id(ctx.db, template_id) {
        Ok(returned) -> {
          case list.first(returned.rows) {
            Ok(template) -> {
              // Render the template body to HTML for preview
              let preview_html = email.render_preview(template.body)
              wisp.html_response(preview_html, 200)
            }
            Error(_) -> wisp.not_found()
          }
        }
        Error(_) -> wisp.internal_server_error()
      }
    }
  }
}

type EmailRequest {
  EmailRequest(email: String, website: String, turnstile_token: String)
}

fn email_request_decoder() -> decode.Decoder(EmailRequest) {
  use email <- decode.field("email", decode.string)
  use website <- decode.optional_field("website", "", decode.string)
  use turnstile_token <- decode.optional_field(
    "cf-turnstile-response",
    "",
    decode.string,
  )
  decode.success(EmailRequest(email:, website:, turnstile_token:))
}

fn handle_subscribe(req: wisp.Request, ctx: Context) -> wisp.Response {
  let client_ip = security.get_client_ip(req)

  // Rate limit: 10 subscribe attempts per 5 minutes per IP
  case rate_limit.is_allowed(ctx.rate_limiter, "subscribe:" <> client_ip, 10, 300) {
    False ->
      json.object([#("error", json.string("Too many requests. Please try again later."))])
      |> json.to_string
      |> wisp.json_response(429)
    True -> {
      use json_body <- wisp.require_json(req)

      case decode.run(json_body, email_request_decoder()) {
    Ok(body) -> {
      // Honeypot validation: if website field is not empty, it's a bot
      case body.website {
        "" -> {
          // Turnstile validation (if configured)
          let client_ip = security.get_client_ip(req)
          case security.verify_turnstile(body.turnstile_token, client_ip) {
            security.TurnstileFailure(msg) ->
              json.object([
                #("error", json.string("Verification failed: " <> msg)),
              ])
              |> json.to_string
              |> wisp.json_response(403)
            // TurnstileSuccess or TurnstileNotConfigured - proceed with subscription
            _ -> {
              case sql.subscribe(ctx.db, body.email) {
                Ok(returned) ->
                  case list.first(returned.rows) {
                    Ok(subscriber) -> {
                      // Check if already confirmed (once migration runs)
                      let is_confirmed = subscriber.confirmed

                      case is_confirmed {
                        True ->
                          json.object([
                            #("id", json.string(uuid.to_string(subscriber.id))),
                            #("email", json.string(subscriber.email)),
                            #("confirmed", json.bool(True)),
                            #(
                              "message",
                              json.string("Already subscribed and confirmed"),
                            ),
                          ])
                          |> json.to_string
                          |> wisp.json_response(200)
                        False -> {
                          // Send confirmation email
                          let _ = send_confirmation_email(ctx.db, body.email)
                          json.object([
                            #("id", json.string(uuid.to_string(subscriber.id))),
                            #("email", json.string(subscriber.email)),
                            #("confirmed", json.bool(False)),
                            #(
                              "message",
                              json.string(
                                "Please check your email to confirm subscription",
                              ),
                            ),
                          ])
                          |> json.to_string
                          |> wisp.json_response(201)
                        }
                      }
                    }

                    Error(_) -> wisp.internal_server_error()
                  }

                Error(_) -> wisp.internal_server_error()
              }
            }
          }
        }
        // Honeypot triggered - bot detected
        _ ->
          json.object([#("error", json.string("Bot detected"))])
          |> json.to_string
          |> wisp.json_response(403)
      }
    }
    Error(_) ->
      json.object([#("error", json.string("Invalid request body"))])
      |> json.to_string
      |> wisp.json_response(400)
      }
    }
  }
}

fn send_confirmation_email(db: pog.Connection, email: String) -> Result(Nil, Nil) {
  let subject = "Confirm your subscription"

  case token.generate(email, token.Confirm) {
    Error(_) -> {
      io.println("[ERROR] Failed to generate confirmation token for " <> email)
      // Record failed send in history
      let _ = sql.create_send_history(db, "Confirmation", subject, "confirmation", 1, 0, 1)
      Error(Nil)
    }
    Ok(confirm_token) -> {
      let base_url =
        envoy.get("BASE_URL") |> result.unwrap("http://localhost:8088")
      let confirm_link = base_url <> "/confirm?token=" <> confirm_token

      case smtp.load_config() {
        Error(err) -> {
          let msg = case err {
            smtp.ConfigError(m) -> m
            smtp.SendError(m) -> m
          }
          io.println("[ERROR] SMTP config error: " <> msg)
          // Record failed send in history
          let _ = sql.create_send_history(db, "Confirmation", subject, "confirmation", 1, 0, 1)
          Error(Nil)
        }
        Ok(config) -> {
          let body =
            "Thank you for subscribing to Brickell Research notifications!\n\n"
            <> "Please click the link below to confirm your subscription:\n\n"
            <> confirm_link
            <> "\n\n"
            <> "If you did not request this subscription, you can ignore this email."

          case smtp.send_email(config, email, subject, body) {
            Ok(_) -> {
              io.println("[INFO] Confirmation email sent to " <> email)
              // Record successful send in history
              let _ = sql.create_send_history(db, "Confirmation", subject, "confirmation", 1, 1, 0)
              Ok(Nil)
            }
            Error(err) -> {
              let msg = case err {
                smtp.ConfigError(m) -> m
                smtp.SendError(m) -> m
              }
              io.println("[ERROR] Failed to send confirmation email: " <> msg)
              // Record failed send in history
              let _ = sql.create_send_history(db, "Confirmation", subject, "confirmation", 1, 0, 1)
              Error(Nil)
            }
          }
        }
      }
    }
  }
}

fn handle_confirm(req: wisp.Request, ctx: Context) -> wisp.Response {
  // Get token from query string
  let token_value = get_query_param(req, "token")

  case token_value {
    "" -> confirmation_page("Invalid confirmation link", False)
    t -> {
      case token.verify(t, token.Confirm) {
        Error(_) ->
          confirmation_page("Invalid or expired confirmation link", False)
        Ok(email) -> {
          // Confirm the subscriber
          case sql.confirm_subscriber(ctx.db, email) {
            Ok(returned) -> {
              case list.first(returned.rows) {
                Ok(_) ->
                  confirmation_page(
                    "Your subscription has been confirmed!",
                    True,
                  )
                Error(_) -> confirmation_page("Subscriber not found", False)
              }
            }
            Error(_) ->
              confirmation_page("Failed to confirm subscription", False)
          }
        }
      }
    }
  }
}

fn handle_unsubscribe_link(req: wisp.Request, ctx: Context) -> wisp.Response {
  // Get token from query string
  let token_value = get_query_param(req, "token")

  case token_value {
    "" -> unsubscribe_page("Invalid unsubscribe link", False)
    t -> {
      case token.verify(t, token.Unsubscribe) {
        Error(_) ->
          unsubscribe_page("Invalid or expired unsubscribe link", False)
        Ok(email) -> {
          // Delete the subscriber
          case sql.unsubscribe(ctx.db, email) {
            Ok(returned) -> {
              case list.first(returned.rows) {
                Ok(_) ->
                  unsubscribe_page(
                    "You have been unsubscribed from our mailing list.",
                    True,
                  )
                Error(_) ->
                  unsubscribe_page(
                    "You are not subscribed to our mailing list.",
                    False,
                  )
              }
            }
            Error(_) -> unsubscribe_page("Failed to unsubscribe", False)
          }
        }
      }
    }
  }
}

fn get_query_param(req: wisp.Request, key: String) -> String {
  wisp.get_query(req)
  |> list.key_find(key)
  |> result.unwrap("")
}

fn confirmation_page(message: String, success: Bool) -> wisp.Response {
  let status_class = case success {
    True -> "success"
    False -> "error"
  }
  let html =
    "<!DOCTYPE html><html><head><title>Subscription Confirmation</title>"
    <> "<style>body{font-family:Monaco,monospace;background:#000;color:#E0FFFF;display:flex;justify-content:center;align-items:center;min-height:100vh;margin:0;}"
    <> ".container{max-width:500px;padding:2rem;border:2px solid #00CED1;text-align:center;}"
    <> ".success{color:#00CED1;}.error{color:#ff6b6b;}"
    <> "h1{color:#FF1493;font-size:1.4rem;}</style></head>"
    <> "<body><div class=\"container\"><h1>Brickell Research</h1>"
    <> "<p class=\""
    <> status_class
    <> "\">"
    <> message
    <> "</p></div></body></html>"

  wisp.html_response(html, case success {
    True -> 200
    False -> 400
  })
}

fn unsubscribe_page(message: String, success: Bool) -> wisp.Response {
  let status_class = case success {
    True -> "success"
    False -> "error"
  }
  let html =
    "<!DOCTYPE html><html><head><title>Unsubscribe</title>"
    <> "<style>body{font-family:Monaco,monospace;background:#000;color:#E0FFFF;display:flex;justify-content:center;align-items:center;min-height:100vh;margin:0;}"
    <> ".container{max-width:500px;padding:2rem;border:2px solid #00CED1;text-align:center;}"
    <> ".success{color:#00CED1;}.error{color:#ff6b6b;}"
    <> "h1{color:#FF1493;font-size:1.4rem;}</style></head>"
    <> "<body><div class=\"container\"><h1>Brickell Research</h1>"
    <> "<p class=\""
    <> status_class
    <> "\">"
    <> message
    <> "</p></div></body></html>"

  wisp.html_response(html, case success {
    True -> 200
    False -> 400
  })
}

fn handle_unsubscribe(req: wisp.Request, ctx: Context) -> wisp.Response {
  let client_ip = security.get_client_ip(req)

  // Rate limit: 10 unsubscribe attempts per 5 minutes per IP
  case rate_limit.is_allowed(ctx.rate_limiter, "unsubscribe:" <> client_ip, 10, 300) {
    False ->
      json.object([#("error", json.string("Too many requests. Please try again later."))])
      |> json.to_string
      |> wisp.json_response(429)
    True -> {
      use json_body <- wisp.require_json(req)

      case decode.run(json_body, email_request_decoder()) {
        Ok(body) -> {
          case sql.unsubscribe(ctx.db, body.email) {
            Ok(returned) ->
              case list.first(returned.rows) {
                Ok(subscriber) ->
                  json.object([
                    #("id", json.string(uuid.to_string(subscriber.id))),
                    #("email", json.string(subscriber.email)),
                    #("message", json.string("Successfully unsubscribed")),
                  ])
                  |> json.to_string
                  |> wisp.json_response(200)

                Error(_) ->
                  json.object([#("error", json.string("Subscriber not found"))])
                  |> json.to_string
                  |> wisp.json_response(404)
              }

            Error(_) -> wisp.internal_server_error()
          }
        }
        Error(_) ->
          json.object([#("error", json.string("Invalid request body"))])
          |> json.to_string
          |> wisp.json_response(400)
      }
    }
  }
}

// --- Send email endpoint -----------------------------------------------------

type SendEmailRequest {
  SendEmailRequest(to: String, subject: String, body: String)
}

fn send_email_request_decoder() -> decode.Decoder(SendEmailRequest) {
  use to <- decode.field("to", decode.string)
  use subject <- decode.field("subject", decode.string)
  use body <- decode.field("body", decode.string)
  decode.success(SendEmailRequest(to:, subject:, body:))
}

fn handle_send(req: wisp.Request) -> wisp.Response {
  use json_body <- wisp.require_json(req)

  case decode.run(json_body, send_email_request_decoder()) {
    Ok(body) -> {
      case smtp.load_config() {
        Error(err) -> {
          let msg = case err {
            smtp.ConfigError(m) -> m
            smtp.SendError(m) -> m
          }
          json.object([#("error", json.string("SMTP config error: " <> msg))])
          |> json.to_string
          |> wisp.json_response(500)
        }
        Ok(config) -> {
          case smtp.send_email(config, body.to, body.subject, body.body) {
            Ok(_) ->
              json.object([#("message", json.string("Email sent"))])
              |> json.to_string
              |> wisp.json_response(200)
            Error(err) -> {
              let msg = case err {
                smtp.ConfigError(m) -> m
                smtp.SendError(m) -> m
              }
              json.object([#("error", json.string(msg))])
              |> json.to_string
              |> wisp.json_response(500)
            }
          }
        }
      }
    }
    Error(_) ->
      json.object([
        #(
          "error",
          json.string("Invalid request body. Required: to, subject, body"),
        ),
      ])
      |> json.to_string
      |> wisp.json_response(400)
  }
}

// --- Admin send page ---------------------------------------------------------

fn show_send_page(
  ctx: Context,
  result: Option(views.SendResult),
) -> wisp.Response {
  let templates = case sql.list_templates(ctx.db) {
    Ok(r) -> r.rows
    Error(_) -> []
  }
  let subscriber_count = case sql.get_all_subscribers(ctx.db) {
    Ok(r) -> list.length(r.rows)
    Error(_) -> 0
  }
  views.send_page(templates, subscriber_count, None, result)
  |> wisp.html_response(200)
}

fn handle_admin_send(req: wisp.Request, ctx: Context) -> wisp.Response {
  use form <- wisp.require_form(req)
  let client_ip = security.get_client_ip(req)

  let template_id =
    list.key_find(form.values, "template_id") |> result.unwrap("")
  let audience = list.key_find(form.values, "audience") |> result.unwrap("all")
  let single_email =
    list.key_find(form.values, "single_email") |> result.unwrap("")

  // Validate template_id
  case template_id {
    "" -> show_send_page_with_error(ctx, "Please select a template")
    _ -> {
      case uuid.from_string(template_id) {
        Error(_) -> show_send_page_with_error(ctx, "Invalid template ID")
        Ok(tid) -> {
          // Get the template
          case sql.get_template_by_id(ctx.db, tid) {
            Error(_) ->
              show_send_page_with_error(ctx, "Failed to load template")
            Ok(template_result) -> {
              case list.first(template_result.rows) {
                Error(_) -> show_send_page_with_error(ctx, "Template not found")
                Ok(template) -> {
                  // Get recipients with subscriber IDs for tracking
                  let recipients = case audience {
                    "single" -> [
                      Recipient(email: single_email, subscriber_id: None),
                    ]
                    _ -> {
                      // Only send to confirmed subscribers
                      case sql.get_confirmed_subscribers(ctx.db) {
                        Ok(r) ->
                          list.map(r.rows, fn(s) {
                            Recipient(email: s.email, subscriber_id: Some(s.id))
                          })
                        Error(_) -> []
                      }
                    }
                  }
                  // Audit log the send operation
                  let recipient_count = int.to_string(list.length(recipients))
                  audit_log("SEND_EMAIL", template.name <> " to " <> recipient_count <> " recipients", client_ip)
                  // Send emails
                  do_send_emails(ctx, template, recipients)
                }
              }
            }
          }
        }
      }
    }
  }
}

fn show_send_page_with_error(ctx: Context, msg: String) -> wisp.Response {
  let templates = case sql.list_templates(ctx.db) {
    Ok(r) -> r.rows
    Error(_) -> []
  }
  let subscriber_count = case sql.get_all_subscribers(ctx.db) {
    Ok(r) -> list.length(r.rows)
    Error(_) -> 0
  }
  views.send_page(templates, subscriber_count, Some(views.Error(msg)), None)
  |> wisp.html_response(400)
}

// Recipient with optional subscriber ID for tracking
type Recipient {
  Recipient(email: String, subscriber_id: Option(uuid.Uuid))
}

fn do_send_emails(
  ctx: Context,
  template: sql.GetTemplateByIdRow,
  recipients: List(Recipient),
) -> wisp.Response {
  case smtp.load_config() {
    Error(err) -> {
      let msg = case err {
        smtp.ConfigError(m) -> m
        smtp.SendError(m) -> m
      }
      show_send_page_with_error(ctx, "SMTP error: " <> msg)
    }
    Ok(config) -> {
      let base_url =
        envoy.get("BASE_URL") |> result.unwrap("http://localhost:8088")

      // Create notification record for history
      let notification_id = case
        sql.create_notification(ctx.db, template.id, "email")
      {
        Ok(r) ->
          list.first(r.rows) |> result.map(fn(n) { n.id }) |> option.from_result
        Error(_) -> None
      }

      // Send to each recipient
      let results =
        list.map(recipients, fn(recipient) {
          // Generate unsubscribe link for this recipient
          let unsubscribe_link = case
            token.generate(recipient.email, token.Unsubscribe)
          {
            Ok(unsub_token) -> base_url <> "/unsubscribe?token=" <> unsub_token
            Error(err) -> {
              let msg = case err {
                token.MissingSecret -> "TOKEN_SECRET not set"
                token.InvalidToken -> "Invalid token"
                token.InvalidSignature -> "Invalid signature"
              }
              io.println("[ERROR] Failed to generate unsubscribe token: " <> msg)
              "#"
            }
          }

          // Render template body (markdown) to HTML and plain text
          let email_content =
            email.render_template(template.body, unsubscribe_link)

          // Send multipart email with both HTML and plain text versions
          let send_result =
            smtp.send_email_multipart(
              config,
              recipient.email,
              template.subject,
              email_content.plain_text,
              email_content.html,
            )
          let success = result.is_ok(send_result)
          let status = case success {
            True -> "sent"
            False -> "failed"
          }

          // Record delivery in database if we have IDs
          case notification_id, recipient.subscriber_id {
            Some(nid), Some(sid) -> {
              let _ = sql.create_delivery(ctx.db, nid, sid, status)
              Nil
            }
            _, _ -> Nil
          }

          case send_result {
            Ok(_) -> #(recipient.email, True, "")
            Error(err) -> {
              let msg = case err {
                smtp.ConfigError(m) -> m
                smtp.SendError(m) -> m
              }
              #(recipient.email, False, msg)
            }
          }
        })

      let sent = list.filter(results, fn(r) { r.1 }) |> list.length
      let failed = list.filter(results, fn(r) { !r.1 }) |> list.length
      let errors =
        list.filter_map(results, fn(r) {
          case r.1 {
            True -> Error(Nil)
            False -> Ok(r.0 <> ": " <> r.2)
          }
        })

      // Record in denormalized send_history for persistent stats
      let _ =
        sql.create_send_history(
          ctx.db,
          template.name,
          template.subject,
          "email",
          list.length(recipients),
          sent,
          failed,
        )

      let send_result = views.SendResult(sent:, failed:, errors:)
      show_send_page(ctx, Some(send_result))
    }
  }
}

// --- History page ------------------------------------------------------------

fn show_history(ctx: Context) -> wisp.Response {
  let history = case sql.get_send_history(ctx.db) {
    Ok(r) -> r.rows
    Error(_) -> []
  }
  views.history_page(history)
  |> wisp.html_response(200)
}

// --- Analytics page ----------------------------------------------------------

fn show_analytics(req: wisp.Request, ctx: Context) -> wisp.Response {
  // Parse window parameter from query string
  let window_param =
    wisp.get_query(req)
    |> list.key_find("window")
    |> result.unwrap("7")

  let #(window, days) = case window_param {
    "30" -> #(views.Days30, 30)
    "90" -> #(views.Days90, 90)
    _ -> #(views.Days7, 7)
  }

  let data = case sql.get_emails_by_day(ctx.db, days) {
    Ok(r) -> r.rows
    Error(_) -> []
  }

  views.analytics_page(data, window)
  |> wisp.html_response(200)
}
