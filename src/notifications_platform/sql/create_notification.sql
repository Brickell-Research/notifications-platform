-- Create a notification record
INSERT INTO notifications (template_id, type)
VALUES ($1, $2)
RETURNING id
