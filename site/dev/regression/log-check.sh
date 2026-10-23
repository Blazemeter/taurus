#!/bin/bash
# Summarizes PHP errors, warnings and deprecations logged by the dev site (or TARGET=prod-candidate) since a point in time.
# Usage: log-check.sh <since>   e.g. log-check.sh 10m, or a timestamp from: date -u +%Y-%m-%dT%H:%M:%SZ
DIR=$(cd "$(dirname "$0")" && pwd)
. "$DIR/target.sh"
[ -n "$1" ] || { echo "usage: $0 <since>"; exit 2; }
cd "$TARGET_DIR" && docker compose logs site --since "$1" 2>&1 \
    | grep -oE "(PHP deprecation|PHP Error with code [0-9]+|PHP (Fatal error|Warning|Deprecated|Parse error)|SmartyCompilerException|Smarty\\\\CompilerException):? [^#]{0,200}" \
    | sed -E 's#/src/pwe/#pwe:#g; s#/var/www/html/vendor/#vendor:#g; s#/var/www/html/#site:#g' \
    | sort | uniq -c | sort -rn
echo "($(cd "$TARGET_DIR" && docker compose logs site --since "$1" 2>&1 | grep -cE 'PHP deprecation|PHP Error with code|PHP (Fatal error|Warning|Deprecated|Parse error)|CompilerException') issues)"
