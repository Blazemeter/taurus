#!/bin/sh -xe

# dump cmdline
echo "$@"

REPORT_FILE="$TAURUS_ARTIFACTS_DIR/RobotExecutor.ldjson"

echo '{"test_suite": "Test", "test_case": "User can create an account and log in", "status": "PASSED", "start_time": 1582199961.0, "duration": 0.076, "error_msg": null, "error_trace": null, "extras": {"assertions": []}, "assertions": [], "subsamples": [], "path": []}
{"test_suite": "Test", "test_case": "User cannot log in with bad password", "status": "PASSED", "start_time": 1582199961.0, "duration": 0.073, "error_msg": null, "error_trace": null, "extras": {"assertions": []}, "assertions": [], "subsamples": [], "path": []}
{"test_suite": "Test", "test_case": "User can change password", "status": "PASSED", "start_time": 1582199961.0, "duration": 0.132, "error_msg": null, "error_trace": null, "extras": {"assertions": []}, "assertions": [], "subsamples": [], "path": []}
{"test_suite": "Test", "test_case": "Invalid password", "status": "PASSED", "start_time": 1582199961.0, "duration": 0.221, "error_msg": null, "error_trace": null, "extras": {"assertions": []}, "assertions": [], "subsamples": [], "path": []}
{"test_suite": "Test", "test_case": "User status is stored in database", "status": "PASSED", "start_time": 1582199962.0, "duration": 0.084, "error_msg": null, "error_trace": null, "extras": {"assertions": []}, "assertions": [], "subsamples": [], "path": []}' > "$REPORT_FILE"
