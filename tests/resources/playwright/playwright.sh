#!/bin/sh -xe

# dump cmdline
echo "$@"

REPORT_FILE="$TAURUS_ARTIFACTS_DIR/taurus-playwright-reporter.jsonl"
echo '{"timestamp":1770194006106,"label":"destination of week","ok":true,"concurency":10,"duration":6135,"connectTime":null,"latency":null,"status":"passed","expectedStatus":"passed","error":null,"runDetails":"destination of week:3:1:example2.spec.ts","logs":null,"byte_count":null}' > "$REPORT_FILE"
echo '{"timestamp":1770194006106,"label":"reserve flight","ok":true,"concurency":10,"duration":7257,"connectTime":null,"latency":null,"status":"passed","expectedStatus":"passed","error":null,"runDetails":"reserve flight:0:0:example2.spec.ts","logs":null,"byte_count":null}' >> "$REPORT_FILE"
