-- Admin add subscriber (already confirmed)
INSERT INTO subscribers (email, confirmed)
VALUES ($1, true)
ON CONFLICT (email) DO UPDATE SET confirmed = true
RETURNING id, email
