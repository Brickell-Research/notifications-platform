-- Get all confirmed subscribers for sending notifications
SELECT id, email
FROM subscribers
WHERE confirmed = true
ORDER BY created_at ASC
