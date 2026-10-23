#!/bin/bash
# Compiles every PWE and site Smarty template inside the running dev container, including templates the
# crawl does not reach (e.g. PWE's default pwe.tpl), and exercises the plugins templates use.
# Runs in the dev container, or in the production candidate with TARGET=prod-candidate.
DIR=$(cd "$(dirname "$0")" && pwd)
. "$DIR/target.sh"
CONTAINER=$(cd "$TARGET_DIR" && docker compose ps -q site)
[ -n "$CONTAINER" ] || { echo "${TARGET:-dev} site container is not running"; exit 2; }
docker cp "$DIR/smarty-compile.php" "$CONTAINER:/tmp/smarty-compile.php" >/dev/null
docker exec "$CONTAINER" sh -c 'rm -rf /tmp/sc; php /tmp/smarty-compile.php'
