-- Get emails sent per day for analytics
-- Parameter $1: number of days to look back
SELECT
  to_char(created_at::date, 'YYYY-MM-DD') as day,
  COALESCE(SUM(sent_count), 0)::int as emails_sent
FROM send_history
WHERE created_at >= CURRENT_DATE - ($1::int - 1)
GROUP BY created_at::date
ORDER BY created_at::date ASC
