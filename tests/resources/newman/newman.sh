#!/bin/sh -xe

# dump cmdline
echo "$@"

REPORT_FILE="$TAURUS_ARTIFACTS_DIR/NewmanExecutor.ldjson"

echo '{"test_case":"should load","test_suite":"demo","status":"PASSED","start_time":1537789085.169,"duration":0.002,"error_msg":null,"error_trace":null}' > "$REPORT_FILE"
