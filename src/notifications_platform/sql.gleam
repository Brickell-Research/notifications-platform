//// This module contains the code to run the sql queries defined in
//// `./src/notifications_platform/sql`.
//// > 🐿️ This module was generated automatically using v4.6.0 of
//// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
////

import gleam/dynamic/decode
import pog
import youid/uuid.{type Uuid}

/// A row you get from running the `admin_add_subscriber` query
/// defined in `./src/notifications_platform/sql/admin_add_subscriber.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type AdminAddSubscriberRow {
  AdminAddSubscriberRow(id: Uuid, email: String)
}

/// Admin add subscriber (already confirmed)
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn admin_add_subscriber(
  db: pog.Connection,
  arg_1: String,
) -> Result(pog.Returned(AdminAddSubscriberRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    use email <- decode.field(1, decode.string)
    decode.success(AdminAddSubscriberRow(id:, email:))
  }

  "-- Admin add subscriber (already confirmed)
INSERT INTO subscribers (email, confirmed)
VALUES ($1, true)
ON CONFLICT (email) DO UPDATE SET confirmed = true
RETURNING id, email
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `confirm_subscriber` query
/// defined in `./src/notifications_platform/sql/confirm_subscriber.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type ConfirmSubscriberRow {
  ConfirmSubscriberRow(id: Uuid, email: String)
}

/// Confirm a subscriber by email
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn confirm_subscriber(
  db: pog.Connection,
  arg_1: String,
) -> Result(pog.Returned(ConfirmSubscriberRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    use email <- decode.field(1, decode.string)
    decode.success(ConfirmSubscriberRow(id:, email:))
  }

  "-- Confirm a subscriber by email
UPDATE subscribers
SET confirmed = true, updated_at = now()
WHERE email = $1
RETURNING id, email
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `create_delivery` query
/// defined in `./src/notifications_platform/sql/create_delivery.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type CreateDeliveryRow {
  CreateDeliveryRow(id: Uuid)
}

/// Create a delivery record for tracking
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn create_delivery(
  db: pog.Connection,
  arg_1: Uuid,
  arg_2: Uuid,
  arg_3: String,
) -> Result(pog.Returned(CreateDeliveryRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    decode.success(CreateDeliveryRow(id:))
  }

  "-- Create a delivery record for tracking
INSERT INTO notification_deliveries (notification_id, subscriber_id, status)
VALUES ($1, $2, $3)
RETURNING id
"
  |> pog.query
  |> pog.parameter(pog.text(uuid.to_string(arg_1)))
  |> pog.parameter(pog.text(uuid.to_string(arg_2)))
  |> pog.parameter(pog.text(arg_3))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `create_notification` query
/// defined in `./src/notifications_platform/sql/create_notification.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type CreateNotificationRow {
  CreateNotificationRow(id: Uuid)
}

/// Create a notification record
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn create_notification(
  db: pog.Connection,
  arg_1: Uuid,
  arg_2: String,
) -> Result(pog.Returned(CreateNotificationRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    decode.success(CreateNotificationRow(id:))
  }

  "-- Create a notification record
INSERT INTO notifications (template_id, type)
VALUES ($1, $2)
RETURNING id
"
  |> pog.query
  |> pog.parameter(pog.text(uuid.to_string(arg_1)))
  |> pog.parameter(pog.text(arg_2))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `create_send_history` query
/// defined in `./src/notifications_platform/sql/create_send_history.sql`.
///
pub type CreateSendHistoryRow {
  CreateSendHistoryRow(id: Uuid)
}

/// Create a send history record
///
pub fn create_send_history(
  db: pog.Connection,
  arg_1: String,
  arg_2: String,
  arg_3: String,
  arg_4: Int,
  arg_5: Int,
  arg_6: Int,
) -> Result(pog.Returned(CreateSendHistoryRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    decode.success(CreateSendHistoryRow(id:))
  }

  "-- Create a send history record
INSERT INTO send_history (template_name, template_subject, notification_type, total_recipients, sent_count, failed_count)
VALUES ($1, $2, $3, $4, $5, $6)
RETURNING id
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.int(arg_4))
  |> pog.parameter(pog.int(arg_5))
  |> pog.parameter(pog.int(arg_6))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `create_template` query
/// defined in `./src/notifications_platform/sql/create_template.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type CreateTemplateRow {
  CreateTemplateRow(id: Uuid, name: String, subject: String, body: String)
}

/// Create a new template
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn create_template(
  db: pog.Connection,
  arg_1: String,
  arg_2: String,
  arg_3: String,
) -> Result(pog.Returned(CreateTemplateRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    use name <- decode.field(1, decode.string)
    use subject <- decode.field(2, decode.string)
    use body <- decode.field(3, decode.string)
    decode.success(CreateTemplateRow(id:, name:, subject:, body:))
  }

  "-- Create a new template
INSERT INTO templates (name, subject, body)
VALUES ($1, $2, $3)
RETURNING id, name, subject, body
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.parameter(pog.text(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `delete_template` query
/// defined in `./src/notifications_platform/sql/delete_template.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type DeleteTemplateRow {
  DeleteTemplateRow(id: Uuid)
}

/// Delete a template by ID
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn delete_template(
  db: pog.Connection,
  arg_1: Uuid,
) -> Result(pog.Returned(DeleteTemplateRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    decode.success(DeleteTemplateRow(id:))
  }

  "-- Delete a template by ID
DELETE FROM templates WHERE id = $1 RETURNING id
"
  |> pog.query
  |> pog.parameter(pog.text(uuid.to_string(arg_1)))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `get_all_subscribers` query
/// defined in `./src/notifications_platform/sql/get_all_subscribers.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type GetAllSubscribersRow {
  GetAllSubscribersRow(id: Uuid, email: String)
}

/// Get all confirmed subscribers for sending notifications
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn get_all_subscribers(
  db: pog.Connection,
) -> Result(pog.Returned(GetAllSubscribersRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    use email <- decode.field(1, decode.string)
    decode.success(GetAllSubscribersRow(id:, email:))
  }

  "-- Get all confirmed subscribers for sending notifications
SELECT id, email
FROM subscribers
WHERE confirmed = true
ORDER BY created_at ASC
"
  |> pog.query
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `get_send_history` query
/// defined in `./src/notifications_platform/sql/get_send_history.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type GetSendHistoryRow {
  GetSendHistoryRow(
    id: Uuid,
    template_name: String,
    template_subject: String,
    notification_type: String,
    created_at: String,
    total_recipients: Int,
    sent_count: Int,
    failed_count: Int,
  )
}

/// Get send history with aggregated stats
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn get_send_history(
  db: pog.Connection,
) -> Result(pog.Returned(GetSendHistoryRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    use template_name <- decode.field(1, decode.string)
    use template_subject <- decode.field(2, decode.string)
    use notification_type <- decode.field(3, decode.string)
    use created_at <- decode.field(4, decode.string)
    use total_recipients <- decode.field(5, decode.int)
    use sent_count <- decode.field(6, decode.int)
    use failed_count <- decode.field(7, decode.int)
    decode.success(GetSendHistoryRow(
      id:,
      template_name:,
      template_subject:,
      notification_type:,
      created_at:,
      total_recipients:,
      sent_count:,
      failed_count:,
    ))
  }

  "-- Get send history with aggregated stats
SELECT
  id,
  template_name,
  template_subject,
  notification_type,
  to_char(created_at, 'YYYY-MM-DD HH24:MI:SS') as created_at,
  total_recipients,
  sent_count,
  failed_count
FROM send_history
ORDER BY created_at DESC
LIMIT 50
"
  |> pog.query
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `get_template_by_id` query
/// defined in `./src/notifications_platform/sql/get_template_by_id.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type GetTemplateByIdRow {
  GetTemplateByIdRow(id: Uuid, name: String, subject: String, body: String)
}

/// Get a template by ID
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn get_template_by_id(
  db: pog.Connection,
  arg_1: Uuid,
) -> Result(pog.Returned(GetTemplateByIdRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    use name <- decode.field(1, decode.string)
    use subject <- decode.field(2, decode.string)
    use body <- decode.field(3, decode.string)
    decode.success(GetTemplateByIdRow(id:, name:, subject:, body:))
  }

  "-- Get a template by ID
SELECT id, name, subject, body FROM templates WHERE id = $1
"
  |> pog.query
  |> pog.parameter(pog.text(uuid.to_string(arg_1)))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `get_template_by_name` query
/// defined in `./src/notifications_platform/sql/get_template_by_name.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type GetTemplateByNameRow {
  GetTemplateByNameRow(id: Uuid, name: String, subject: String, body: String)
}

/// Get a template by its name
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn get_template_by_name(
  db: pog.Connection,
  arg_1: String,
) -> Result(pog.Returned(GetTemplateByNameRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    use name <- decode.field(1, decode.string)
    use subject <- decode.field(2, decode.string)
    use body <- decode.field(3, decode.string)
    decode.success(GetTemplateByNameRow(id:, name:, subject:, body:))
  }

  "-- Get a template by its name
SELECT id, name, subject, body
FROM templates
WHERE name = $1
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `list_templates` query
/// defined in `./src/notifications_platform/sql/list_templates.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type ListTemplatesRow {
  ListTemplatesRow(id: Uuid, name: String, subject: String, body: String)
}

/// List all templates
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn list_templates(
  db: pog.Connection,
) -> Result(pog.Returned(ListTemplatesRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    use name <- decode.field(1, decode.string)
    use subject <- decode.field(2, decode.string)
    use body <- decode.field(3, decode.string)
    decode.success(ListTemplatesRow(id:, name:, subject:, body:))
  }

  "-- List all templates
SELECT id, name, subject, body FROM templates ORDER BY name ASC
"
  |> pog.query
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `subscribe` query
/// defined in `./src/notifications_platform/sql/subscribe.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type SubscribeRow {
  SubscribeRow(id: Uuid, email: String, confirmed: Bool)
}

/// Subscribe a user by email, returning the subscriber if created or already exists
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn subscribe(
  db: pog.Connection,
  arg_1: String,
) -> Result(pog.Returned(SubscribeRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    use email <- decode.field(1, decode.string)
    use confirmed <- decode.field(2, decode.bool)
    decode.success(SubscribeRow(id:, email:, confirmed:))
  }

  "-- Subscribe a user by email, returning the subscriber if created or already exists
INSERT INTO subscribers (email, confirmed)
VALUES ($1, false)
ON CONFLICT (email) DO UPDATE SET email = EXCLUDED.email
RETURNING id, email, confirmed
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `unsubscribe` query
/// defined in `./src/notifications_platform/sql/unsubscribe.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type UnsubscribeRow {
  UnsubscribeRow(id: Uuid, email: String)
}

/// Unsubscribe a user by email, returning the deleted subscriber if found
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn unsubscribe(
  db: pog.Connection,
  arg_1: String,
) -> Result(pog.Returned(UnsubscribeRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    use email <- decode.field(1, decode.string)
    decode.success(UnsubscribeRow(id:, email:))
  }

  "-- Unsubscribe a user by email, returning the deleted subscriber if found
DELETE FROM subscribers
WHERE email = $1
RETURNING id, email
"
  |> pog.query
  |> pog.parameter(pog.text(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `update_template` query
/// defined in `./src/notifications_platform/sql/update_template.sql`.
///
/// > 🐿️ This type definition was generated automatically using v4.6.0 of the
/// > [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub type UpdateTemplateRow {
  UpdateTemplateRow(id: Uuid, name: String, subject: String, body: String)
}

/// Update an existing template
///
/// > 🐿️ This function was generated automatically using v4.6.0 of
/// > the [squirrel package](https://github.com/giacomocavalieri/squirrel).
///
pub fn update_template(
  db: pog.Connection,
  arg_1: Uuid,
  arg_2: String,
  arg_3: String,
  arg_4: String,
) -> Result(pog.Returned(UpdateTemplateRow), pog.QueryError) {
  let decoder = {
    use id <- decode.field(0, uuid_decoder())
    use name <- decode.field(1, decode.string)
    use subject <- decode.field(2, decode.string)
    use body <- decode.field(3, decode.string)
    decode.success(UpdateTemplateRow(id:, name:, subject:, body:))
  }

  "-- Update an existing template
UPDATE templates
SET name = $2, subject = $3, body = $4, updated_at = now()
WHERE id = $1
RETURNING id, name, subject, body
"
  |> pog.query
  |> pog.parameter(pog.text(uuid.to_string(arg_1)))
  |> pog.parameter(pog.text(arg_2))
  |> pog.parameter(pog.text(arg_3))
  |> pog.parameter(pog.text(arg_4))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

/// A row you get from running the `get_emails_by_day` query
/// defined in `./src/notifications_platform/sql/get_emails_by_day.sql`.
///
pub type GetEmailsByDayRow {
  GetEmailsByDayRow(day: String, emails_sent: Int)
}

/// Get emails sent per day for analytics
///
pub fn get_emails_by_day(
  db: pog.Connection,
  arg_1: Int,
) -> Result(pog.Returned(GetEmailsByDayRow), pog.QueryError) {
  let decoder = {
    use day <- decode.field(0, decode.string)
    use emails_sent <- decode.field(1, decode.int)
    decode.success(GetEmailsByDayRow(day:, emails_sent:))
  }

  "-- Get emails sent per day for analytics
-- Parameter $1: number of days to look back
SELECT
  to_char(created_at::date, 'YYYY-MM-DD') as day,
  COALESCE(SUM(sent_count), 0)::int as emails_sent
FROM send_history
WHERE created_at >= CURRENT_DATE - ($1::int - 1)
GROUP BY created_at::date
ORDER BY created_at::date ASC
"
  |> pog.query
  |> pog.parameter(pog.int(arg_1))
  |> pog.returning(decoder)
  |> pog.execute(db)
}

// --- Encoding/decoding utils -------------------------------------------------

/// A decoder to decode `Uuid`s coming from a Postgres query.
///
fn uuid_decoder() {
  use bit_array <- decode.then(decode.bit_array)
  case uuid.from_bit_array(bit_array) {
    Ok(uuid) -> decode.success(uuid)
    Error(_) -> decode.failure(uuid.v7(), "Uuid")
  }
}
