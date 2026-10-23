#!/bin/bash
# Compares HTTP status and redirect target of every URL variant between two sites.
# Usage: status-compare.sh [new-base-url] [old-base-url]   (defaults: dev :8081, legacy :8080)
DIR=$(cd "$(dirname "$0")" && pwd)
NEW=${1:-http://localhost:8081}; OLD=${2:-http://localhost:8080}

while read -r u; do
    a=$(curl -s -o /dev/null -w '%{http_code} %{redirect_url}' --max-time 60 "$OLD$u" | sed "s#$OLD##")
    b=$(curl -s -o /dev/null -w '%{http_code} %{redirect_url}' --max-time 60 "$NEW$u" | sed "s#$NEW##")
    [ "$a" = "$b" ] && s=same || s=DIFF
    printf "%s\t%s\t%s\t%s\n" "$s" "$a" "$b" "$u"
done < "$DIR/urls-variants.txt" > "${TMPDIR:-/tmp}/status-compare.$$"

cut -f1 "${TMPDIR:-/tmp}/status-compare.$$" | sort | uniq -c
grep "^DIFF" "${TMPDIR:-/tmp}/status-compare.$$" | awk -F'\t' '{print "  " $4 ": " $2 " -> " $3}'
rm -f "${TMPDIR:-/tmp}/status-compare.$$"
