-- Delete a template by ID
DELETE FROM templates WHERE id = $1 RETURNING id
