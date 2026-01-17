-- Create a delivery record for tracking
INSERT INTO notification_deliveries (notification_id, subscriber_id, status)
VALUES ($1, $2, $3)
RETURNING id
