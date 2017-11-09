var util = require("util"),
    events = require("events"),
    fs = require("fs");
var Launcher = require("webdriverio").Launcher;

function epoch() {
    return (new Date()).getTime() / 1000.0;
}

function reportItem(test, status, err) {
    var stateStatus = {
        "passed": "PASSED",
        "failed": "FAILED",
        "pending": "SKIPPED"
    };

    /*eslint-disable camelcase */
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
    /*eslint-enable camelcase */
}

var reportStream = null;

var TaurusReporter = function(config) {
    config.reporterOptions = {
        totalTests: 0,
        failedTests: 0,
        passedTests: 0
    };

    var reportStatusLine = function(lastTest) {
        var total = config.reporterOptions.totalTests;
        var passed = config.reporterOptions.passedTests;
        var failed = config.reporterOptions.failedTests;
        var line = lastTest.title + ",Total:" + total + " Passed:" + passed + " Failed:" + failed;
        process.stdout.write(line + "\n");
    };

    var testStartTime = 0;
    var testStatus = null;
    var testErr = null;

    this.on("test:start", function(test) {
        testStartTime = epoch();
        testStatus = null;
        testErr = null;
    });

    this.on("test:pass", function(test) {
        testStatus = "passed";
    });

    this.on("test:fail", function(test) {
        testStatus = "failed";
        testErr = test.err;
    });

    this.on("test:pending", function(test) {
        testStatus = "pending";
    });

    this.on("test:end", function(test) {
        config.reporterOptions.totalTests++;
        if (testStatus === "failed") {
            config.reporterOptions.failedTests++;
        } else if (testStatus === "passed") {
            config.reporterOptions.passedTests++;
        }

        test.startTime = testStartTime;
        var item = reportItem(test, testStatus, testErr || {});
        try {
            reportStream.write(JSON.stringify(item) + "\n");
        } catch(err) {
            process.stderr.write("error while writing: " + err.toString() + "\n");
        }
        reportStatusLine(test);
    });

};

TaurusReporter.reporterName = "TaurusReporter";
util.inherits(TaurusReporter, events.EventEmitter);

function usage() {
    process.stdout.write("Taurus Webdriver.io Plugin\n");
    process.stdout.write("\n");
    process.stdout.write("Usage:\n");
    process.stdout.write("--report-file FILE - report file name\n");
    process.stdout.write("--wdio-config FILE - path to WebdriverIO config\n");
    process.stdout.write("--iterations N - Number of iterations over test suite\n");
    process.stdout.write("--hold-for N - Duration limit for a test suite\n");
}

function parseCmdline(argv) {
    var options = {iterations: 0, holdFor: 0, wdioConfig: "wdio.conf.js", reportFile: "report.ldjson"};
    var args = argv.slice(2);
    while (args) {
        var arg = args.shift();
        if (!arg) {
            break;
        }
        switch (arg) {
        case "--report-file":
            options.reportFile = args.shift();
            break;
        case "--iterations":
            options.iterations = parseInt(args.shift());
            break;
        case "--hold-for":
            options.holdFor = parseFloat(args.shift());
            break;
        case "--wdio-config":
            options.wdioConfig = args.shift();
            break;
        case "--help":
            usage();
            process.exit(0);
            break;
        default:
            process.stderr.write("Unknown argument: %s" + arg.toString() + "\n");
            process.exit(1);
            break;
        }
    }

    if (options.iterations === 0) {
        if (options.holdFor > 0) {
            options.iterations = Infinity;
        } else {
            options.iterations = 1;
        }
    }



    return options;
}

function runWDIO() {
    var config = parseCmdline(process.argv);
    reportStream = fs.createWriteStream(config.reportFile);

    var configFile = config.wdioConfig;
    var opts = {
        reporters: [TaurusReporter],
    };

    var wdio = new Launcher(configFile, opts);

    function done(code) {
        if (reportStream) {
            reportStream.end();
        }
        process.exit(code);
    }

    function handleError(error) {
        console.error("Launcher failed to start the test", error);
        done(1);
    }

    var startTime = epoch();

    function loopWDIO(code) {
        config.iterations -= 1;
        if (config.iterations === 0) {
            done(0);
        }
        var offset = epoch() - startTime;
        if (config.holdFor > 0 && offset > config.holdFor) {
            done(0);
        }
        wdio.run().then(loopWDIO, handleError);
    }

    wdio.run().then(loopWDIO, handleError);
}

if (require.main === module) {
    runWDIO();
}