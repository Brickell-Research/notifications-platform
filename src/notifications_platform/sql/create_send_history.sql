-- Create a send history record
INSERT INTO send_history (template_name, template_subject, notification_type, total_recipients, sent_count, failed_count)
VALUES ($1, $2, $3, $4, $5, $6)
RETURNING id
