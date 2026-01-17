-- Subscribe a user by email, returning the subscriber if created or already exists
INSERT INTO subscribers (email, confirmed)
VALUES ($1, false)
ON CONFLICT (email) DO UPDATE SET email = EXCLUDED.email
RETURNING id, email, confirmed
