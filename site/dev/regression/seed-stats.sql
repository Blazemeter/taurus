-- Identical synthetic usage data for local stats DBs: ~70 days of launches through the agg_data trigger.
TRUNCATE raw_data, home_users, aggregate_data;
DO $$
DECLARE d date; i int; envs text[] := '{linux,windows,macos,docker,jenkins,amazon}';
BEGIN
  FOR k IN 0..69 LOOP
    d := date '2026-08-15' + k;
    FOR i IN 1..(3 + (k * 7) % 9) LOOP
      INSERT INTO raw_data VALUES (to_char(d, 'DD.MM.YYYY'), to_char(d, 'IW'), to_char(d, 'MM.YYYY'),
        envs[1 + (i + k) % 6] || '-' || ((k * 13 + i * 7) % 40));
    END LOOP;
  END LOOP;
END $$;
SELECT count(*) AS days, sum(desktop_launch) AS desktop, sum(cloud_launch) AS cloud, sum(new_users) AS new FROM aggregate_data;
