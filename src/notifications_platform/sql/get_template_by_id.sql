-- Get a template by ID
SELECT id, name, subject, body FROM templates WHERE id = $1
