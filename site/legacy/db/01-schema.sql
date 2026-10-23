-- Reconstructed from updates.php, bzt-usage-stats/*.php and aggregate_func.sql.
-- The production schema is not in the repo, column order of aggregate_data is a best guess
-- matching the 15 values inserted by agg_data() (remaining counters default to 0).

CREATE TABLE raw_data (
    date    TEXT,
    week    TEXT,
    month   TEXT,
    machine TEXT
);

CREATE TABLE home_users (
    machine TEXT
);

CREATE TABLE aggregate_data (
    new_users      INTEGER DEFAULT 0,
    cloud_launch   INTEGER DEFAULT 0,
    desktop_launch INTEGER DEFAULT 0,
    jenkins        INTEGER DEFAULT 0,
    travis         INTEGER DEFAULT 0,
    bamboo         INTEGER DEFAULT 0,
    teamcity       INTEGER DEFAULT 0,
    date           TEXT,
    docker         INTEGER DEFAULT 0,
    amazon         INTEGER DEFAULT 0,
    google_cloud   INTEGER DEFAULT 0,
    azure          INTEGER DEFAULT 0,
    linux          INTEGER DEFAULT 0,
    week           TEXT,
    month          TEXT,
    windows        INTEGER DEFAULT 0,
    macos          INTEGER DEFAULT 0
);
