-- Confirm a subscriber by email
UPDATE subscribers
SET confirmed = true, updated_at = now()
WHERE email = $1
RETURNING id, email
