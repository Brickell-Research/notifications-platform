-- Get send history with aggregated stats
SELECT
  id,
  template_name,
  template_subject,
  notification_type,
  to_char(created_at, 'YYYY-MM-DD HH24:MI:SS') as created_at,
  total_recipients,
  sent_count,
  failed_count
FROM send_history
ORDER BY created_at DESC
LIMIT 50
