#!/bin/bash
# Fetches every page on two sites and compares the HTML, ignoring random ids, quote escaping and trailing whitespace.
# Usage: crawl.sh <out-dir> [new-base-url] [old-base-url]
#   defaults: new = http://localhost:8081 (dev), old = http://localhost:8080 (legacy)
# Writes <out-dir>/old/, <out-dir>/new/ (raw HTML, reusable by snapshot-diff.sh) and <out-dir>/result.tsv.
DIR=$(cd "$(dirname "$0")" && pwd)
OUT=$1; NEW=${2:-http://localhost:8081}; OLD=${3:-http://localhost:8080}
[ -n "$OUT" ] || { echo "usage: $0 <out-dir> [new-base-url] [old-base-url]"; exit 2; }
. "$DIR/normalize.sh"

rm -rf "$OUT"; mkdir -p "$OUT/old" "$OUT/new"; : > "$OUT/result.tsv"
while read -r u; do
    n=$(echo "$u" | tr '/?&=' '____')
    a=$(curl -s -o "$OUT/old/$n" -w '%{http_code}' --max-time 120 "$OLD$u")
    b=$(curl -s -o "$OUT/new/$n" -w '%{http_code}' --max-time 120 "$NEW$u")
    if diff -q <(normalize "$OUT/old/$n") <(normalize "$OUT/new/$n") >/dev/null; then s=same; else s=DIFF; fi
    printf "%s\t%s\t%s\t%s\n" "$a" "$b" "$s" "$u" >> "$OUT/result.tsv"
done < "$DIR/urls-pages.txt"

echo "status old->new and HTML:"
awk -F'\t' '{print $1"->"$2" "$3}' "$OUT/result.tsv" | sort | uniq -c
# status code changes are always listed, HTML differences only when there are few of them
awk -F'\t' '$1 != $2 {print "  status changed: " $1 " -> " $2 " " $4}' "$OUT/result.tsv"
diffs=$(grep -c "	DIFF	" "$OUT/result.tsv")
if [ "$diffs" -le 10 ]; then
    grep "	DIFF	" "$OUT/result.tsv" | awk -F'\t' '{print "  HTML differs: " $4}'
else
    echo "  ($diffs pages with different HTML, see $OUT/result.tsv; use snapshot-diff.sh to compare dev before/after a change)"
fi
