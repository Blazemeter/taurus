const fs = require("fs");
const WDIOReporter = require ('@wdio/reporter').default;

function epoch() {
    return (new Date()).getTime() / 1000.0;
}

function reportItem(test, status, err) {
    const stateStatus = {
        "passed": "PASSED",
        "failed": "FAILED",
        "pending": "SKIPPED"
    };

    return {
        test_case: test.title,
        test_suite: test.parent,
        status: stateStatus[status],
        start_time: test.startTime,
        duration: epoch() - test.startTime,
        error_msg: err.message || null,
        error_trace: err.stack || null,
        extras: {
            file: test.file || null
        }
    };
}

class CustomReporter extends WDIOReporter {
    constructor(options) {
        super(options);
        this.reporterOptions = {
            totalTests: 0,
            failedTests: 0,
            passedTests: 0
        };
        this.testStartTime = 0;
        this.testStatus = null;
        this.testErr = null;
        this.reportFile = options.reportFile;
    }

    onRunnerStart() {
        this.reportStream = fs.createWriteStream(this.reportFile);
    }

    reportStatusLine(lastTest) {
        const total = this.reporterOptions.totalTests;
        const passed = this.reporterOptions.passedTests;
        const failed = this.reporterOptions.failedTests;
        const line = `${lastTest.title},Total:${total} Passed:${passed} Failed:${failed}`;
        process.stdout.write(line + "\n");
    };
    onTestStart(test) {
        this.testStartTime = epoch();
        this.testStatus = null;
        this.testErr = null;
    };
    onTestPass() {
        this.testStatus = "passed";
    }
    onTestFail(test) {
        this.testStatus = "failed";
        this.testErr = test.err;
    };
    onTestEnd(test) {
        this.reporterOptions.totalTests++;
        if (this.testStatus === "failed") {
            this.reporterOptions.failedTests++;
        } else if (this.testStatus === "passed") {
            this.reporterOptions.passedTests++;
        }

        test.startTime = this.testStartTime;
        const item = reportItem(test, this.testStatus, this.testErr || {});
        try {
            this.reportStream.write(JSON.stringify(item) + "\n");
        } catch(err) {
            process.stderr.write("error while writing: " + err.toString() + "\n");
        }
        this.reportStatusLine(test);
    };
    onRunnerEnd() {
        if (this.reportStream) {
            this.reportStream.end();
        }
        return null;
    }
}

exports.default = CustomReporter;