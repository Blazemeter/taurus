@echo off
echo %*

set REPORT_FILE=%TAURUS_ARTIFACTS_DIR%\RobotExecutor.ldjson

echo {"test_suite": "Test", "test_case": "User can create an account and log in", "status": "PASSED", "start_time": 1582199961.0, "duration": 0.076, "error_msg": null, "error_trace": null, "extras": {"assertions": []}, "assertions": [], "subsamples": [], "path": []}  > %REPORT_FILE%

