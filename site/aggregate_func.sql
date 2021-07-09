-- use this query for add this trigger to DB
-- CREATE TRIGGER agg_data BEFORE INSERT OR UPDATE ON public.raw_data FOR EACH ROW EXECUTE PROCEDURE public.agg_data();

-- for update trigger code in DB
-- 1. DROP TRIGGER IF EXISTS agg_data ON public.raw_data;
-- 2. DROP FUNCTION public.agg_data();
-- 3. run code below for create function
-- 4. CREATE TRIGGER agg_data BEFORE INSERT OR UPDATE ON public.raw_data FOR EACH ROW EXECUTE PROCEDURE public.agg_data();

CREATE FUNCTION public.agg_data() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
    DECLARE
        cloud_users_env TEXT[] := '{"jenkins", "travis", "bamboo", "teamcity", "docker", "amazon", "google_cloud", "azure"}';
        desktop_users_env TEXT[] := '{"linux", "windows", "macos"}';
    BEGIN
        LOCK TABLE aggregate_data IN SHARE ROW EXCLUSIVE MODE;
        -- Checking the existence of this day
        IF NOT EXISTS (SELECT date FROM aggregate_data WHERE date = CAST(NEW.date AS TEXT)) THEN
            INSERT INTO aggregate_data VALUES
            (0,0,0,0,0,0,0,NEW.date,0,0,0,0,0,NEW.week,NEW.month);
        END IF;
        -- Checking the existence of this users
        IF NOT EXISTS (SELECT machine FROM home_users WHERE  machine = NEW.machine) THEN
            INSERT INTO home_users VALUES
            (NEW.machine);
            UPDATE aggregate_data SET new_users = new_users + 1 WHERE date = CAST(NEW.date AS TEXT);
        END IF;
        -- Checking type of this environment
        IF array_position(cloud_users_env, split_part(NEW.machine, '-', 1)) IS NOT NULL THEN
            UPDATE aggregate_data SET cloud_launch = cloud_launch + 1 WHERE date = CAST(NEW.date AS TEXT);
        ELSIF array_position(desktop_users_env, split_part(NEW.machine, '-', 1)) IS NOT NULL THEN
            UPDATE aggregate_data SET  desktop_launch = desktop_launch + 1 WHERE date = CAST(NEW.date AS TEXT);
        END IF;
        -- Count users by env
        IF split_part(NEW.machine, '-', 1) = 'jenkins' THEN
            UPDATE aggregate_data SET jenkins = jenkins + 1 WHERE date = CAST(NEW.date AS TEXT);
        ELSIF split_part(NEW.machine, '-', 1) = 'travis' THEN
            UPDATE aggregate_data SET travis = travis + 1 WHERE date = CAST(NEW.date AS TEXT);
        ELSIF split_part(NEW.machine, '-', 1) = 'bamboo' THEN
            UPDATE aggregate_data SET bamboo = bamboo + 1 WHERE date = CAST(NEW.date AS TEXT);
        ELSIF split_part(NEW.machine, '-', 1) = 'teamcity' THEN
            UPDATE aggregate_data SET teamcity = teamcity + 1 WHERE date = CAST(NEW.date AS TEXT);
        ELSIF split_part(NEW.machine, '-', 1) = 'docker' THEN
            UPDATE aggregate_data SET docker = docker + 1 WHERE date = CAST(NEW.date AS TEXT);
        ELSIF split_part(NEW.machine, '-', 1) = 'amazon' THEN
            UPDATE aggregate_data SET amazon = amazon + 1 WHERE date = CAST(NEW.date AS TEXT);
        ELSIF split_part(NEW.machine, '-', 1) = 'google_cloud' THEN
            UPDATE aggregate_data SET google_cloud = google_cloud + 1 WHERE date = CAST(NEW.date AS TEXT);
        ELSIF split_part(NEW.machine, '-', 1) = 'azure' THEN
            UPDATE aggregate_data SET azure = azure + 1 WHERE date = CAST(NEW.date AS TEXT);
        ELSIF split_part(NEW.machine, '-', 1) = 'linux' THEN
            UPDATE aggregate_data SET linux = linux + 1 WHERE date = CAST(NEW.date AS TEXT);
        ELSIF split_part(NEW.machine, '-', 1) = 'windows' THEN
            UPDATE aggregate_data SET windows = windows + 1 WHERE date = CAST(NEW.date AS TEXT);
        ELSIF split_part(NEW.machine, '-', 1) = 'macos' THEN
            UPDATE aggregate_data SET macos = macos + 1 WHERE date = CAST(NEW.date AS TEXT);
        END IF;
        RETURN NULL;
    END;
$$;