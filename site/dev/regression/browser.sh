#!/bin/bash -e
# Runs a browser check in the Playwright image (headless Chromium), the sites are reached via host.docker.internal.
# Usage: browser.sh <check> [args...]
#   browser.sh browser-check  <out-dir> <old-base> <new-base> <path>...   screenshots, console errors, YAML/JSON switch
#   browser.sh chart-compare  <out-dir> <old-stats-url> <new-stats-url>  usage statistics charts plot the same data
#   browser.sh tracking-check <url>...                                    analytics/tracking requests made by pages
#   browser.sh cookie-check   <url>...                                    cookies set on page load, embedded players
# Paths given as <out-dir> are relative to regression/out/ on the host.
DIR=$(cd "$(dirname "$0")" && pwd)
IMAGE=mcr.microsoft.com/playwright:v1.55.1-noble   # must match the playwright version in package.json
CHECK=$1; shift
[ -f "$DIR/browser/$CHECK.js" ] || { echo "unknown check: $CHECK"; exit 2; }
mkdir -p "$DIR/out" "$DIR/.deps"
cp "$DIR/package.json" "$DIR/.deps/"

docker run --rm --add-host=host.docker.internal:host-gateway \
    -v "$DIR/browser:/checks:ro" -v "$DIR/.deps:/deps" -v "$DIR/out:/out" -w /out "$IMAGE" \
    sh -c '[ -d /deps/node_modules/playwright ] || (cd /deps && npm install --silent --no-audit --no-fund >/dev/null 2>&1); NODE_PATH=/deps/node_modules node /checks/"$0".js "$@"' "$CHECK" "$@"
