#!/bin/bash -e
# Loads identical synthetic usage statistics into the legacy and the dev (or TARGET=prod-candidate) databases,
# replacing their content.
# Run it right before comparing charts, test calls to /updates/ add rows to the databases.
DIR=$(cd "$(dirname "$0")" && pwd)
. "$DIR/target.sh"
for stack in "$DIR/../../legacy" "$TARGET_DIR"; do
    echo "$(basename "$(cd "$stack" && pwd)") (days|desktop|cloud|new): $(cd "$stack" && docker compose exec -T db psql -U taurus -d stats -qtA < "$DIR/seed-stats.sql" | tail -1)"
done
