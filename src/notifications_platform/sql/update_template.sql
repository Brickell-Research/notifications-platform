-- Update an existing template
UPDATE templates
SET name = $2, subject = $3, body = $4, updated_at = now()
WHERE id = $1
RETURNING id, name, subject, body
