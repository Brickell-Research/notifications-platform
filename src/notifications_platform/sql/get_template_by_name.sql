-- Get a template by its name
SELECT id, name, subject, body
FROM templates
WHERE name = $1
