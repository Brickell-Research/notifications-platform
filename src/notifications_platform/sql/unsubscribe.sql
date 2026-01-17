-- Unsubscribe a user by email, returning the deleted subscriber if found
DELETE FROM subscribers
WHERE email = $1
RETURNING id, email
