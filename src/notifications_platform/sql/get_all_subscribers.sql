-- Get all subscribers with their confirmation status
SELECT id, email, confirmed
FROM subscribers
ORDER BY created_at DESC
