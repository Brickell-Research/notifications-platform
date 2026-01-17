import gleam/option.{type Option, None, Some}

// Simple rate limiter using ETS (Erlang Term Storage)
// This provides in-memory rate limiting that works for single-server deployments

pub opaque type RateLimiter {
  RateLimiter(table: EtsTable)
}

type EtsTable

pub fn start() -> Result(RateLimiter, Nil) {
  case create_ets_table() {
    Ok(table) -> Ok(RateLimiter(table))
    Error(_) -> Error(Nil)
  }
}

pub fn check(
  limiter: RateLimiter,
  key: String,
  max_requests: Int,
  window_seconds: Int,
) -> Bool {
  let now = current_time_seconds()
  let cutoff = now - window_seconds

  // Get current count for this key within the window
  let count = get_request_count(limiter.table, key, cutoff)

  case count < max_requests {
    True -> {
      // Record this request
      record_request(limiter.table, key, now)
      True
    }
    False -> False
  }
}

pub fn is_allowed(
  limiter: Option(RateLimiter),
  key: String,
  max_requests: Int,
  window_seconds: Int,
) -> Bool {
  case limiter {
    None -> True
    Some(l) -> check(l, key, max_requests, window_seconds)
  }
}

// Erlang FFI functions
@external(erlang, "rate_limit_ffi", "create_ets_table")
fn create_ets_table() -> Result(EtsTable, Nil)

@external(erlang, "rate_limit_ffi", "current_time_seconds")
fn current_time_seconds() -> Int

@external(erlang, "rate_limit_ffi", "get_request_count")
fn get_request_count(table: EtsTable, key: String, cutoff: Int) -> Int

@external(erlang, "rate_limit_ffi", "record_request")
fn record_request(table: EtsTable, key: String, timestamp: Int) -> Nil
