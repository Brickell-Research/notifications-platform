-- Create a new template
INSERT INTO templates (name, subject, body)
VALUES ($1, $2, $3)
RETURNING id, name, subject, body
