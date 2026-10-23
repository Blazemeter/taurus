#!/bin/bash
# Compares the "new" side of two crawl.sh outputs, e.g. dev before and after a change.
# Usage: snapshot-diff.sh <crawl-dir-before> <crawl-dir-after> [-v]   (-v prints the changed lines)
DIR=$(cd "$(dirname "$0")" && pwd)
A=$1/new; B=$2/new; VERBOSE=$3
[ -d "$A" ] && [ -d "$B" ] || { echo "usage: $0 <crawl-dir-before> <crawl-dir-after> [-v]"; exit 2; }
. "$DIR/normalize.sh"

changed=0; total=0
for f in "$A"/*; do
    b=$(basename "$f"); total=$((total + 1))
    if ! diff -q <(normalize "$f") <(normalize "$B/$b") >/dev/null 2>&1; then
        changed=$((changed + 1)); echo "changed: $b"
        [ "$VERBOSE" = "-v" ] && diff <(normalize "$f") <(normalize "$B/$b") | grep '^[<>]' | cut -c1-200
    fi
done
echo "pages changed: $changed / $total"
