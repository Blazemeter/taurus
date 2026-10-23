#!/bin/bash
# Runs the whole regression set against the running legacy (:8080) and dev (:8081) stacks,
# or with TARGET=prod-candidate against the production candidate (:8082) instead of dev.
# Usage: [TARGET=prod-candidate] run-all.sh [previous-crawl-dir]
#   previous-crawl-dir: out/crawl-<timestamp> of an earlier run, the target HTML is then also compared with it
# Results go to out/, see README.md for the expected (known) differences.
DIR=$(cd "$(dirname "$0")" && pwd)
. "$DIR/target.sh"
export TARGET=${TARGET:-dev}
OUT=$DIR/out; STAMP=$TARGET-$(date +%Y%m%d-%H%M%S); SINCE=$(date -u +%Y-%m-%dT%H:%M:%SZ)
B=http://host.docker.internal
FILTER='google-analytics.com/g/collect|doubleclick.net|google.com/(rmkt|ccm)/collect|facebook.net//log|youtube(-nocookie)?.com/api/stats'
mkdir -p "$OUT"

echo "### 1. pages: legacy vs $TARGET"
"$DIR/crawl.sh" "$OUT/crawl-$STAMP" "$TARGET_URL"
if [ -n "$1" ]; then
    echo; echo "### 1b. $TARGET pages: $1 vs now"
    "$DIR/snapshot-diff.sh" "$1" "$OUT/crawl-$STAMP"
fi

echo; echo "### 2. URL variants (status and redirect): legacy vs $TARGET"
"$DIR/status-compare.sh" "$TARGET_URL"

echo; echo "### 3. templates"
"$DIR/template-check.sh" | awk '{print "  " $0}'

echo; echo "### 4. browser: legacy vs $TARGET"
"$DIR/browser.sh" browser-check "browser-$STAMP" "$B:8080" "$B:$TARGET_PORT" \
    / /docs/Index/ /docs/JMeter/ /docs/ConfigSyntax/ /kb/Index/ /kb/TeamCity/ /install/Installation/ /docs/NoSuchPage/ \
    | grep -vE "$FILTER"

echo; echo "### 5. usage statistics charts (databases reseeded)"
"$DIR/seed-db.sh"
"$DIR/browser.sh" chart-compare "charts-$STAMP" "$B:8080/bzt-usage-stats/" "$B:$TARGET_PORT/bzt-usage-stats/"

echo; echo "### 6. tracking requests and cookies on $TARGET"
"$DIR/browser.sh" tracking-check "$B:$TARGET_PORT/" "$B:$TARGET_PORT/docs/JMeter/"
"$DIR/browser.sh" cookie-check "$B:$TARGET_PORT/" "$B:$TARGET_PORT/kb/Videos/"

echo; echo "### 7. $TARGET site log"
"$DIR/log-check.sh" "$SINCE"

echo; echo "crawl saved as out/crawl-$STAMP (pass it to the next run to compare before/after)"
