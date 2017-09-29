var util = require('util'),
    events = require('events'),
    fs = require("fs");
var Launcher = require('webdriverio').Launcher;

function epoch() {
    return (new Date()).getTime() / 1000.0;
}

function reportItem(test, err) {
    var stateStatus = {
        "passed": "PASSED",
        "failed": "FAILED",
        "pending": "SKIPPED"
    };

    /*eslint-disable camelcase */
    return {
        test_case: test.title,
        test_suite: test.parent,
        status: stateStatus[test.state],
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

    console.log('initialised custom reporter with the following reporter options:', config);

    var reportStatusLine = function(lastTest) {
        var total = config.reporterOptions.totalTests;
        var passed = config.reporterOptions.passedTests;
        var failed = config.reporterOptions.failedTests;
        var line = lastTest.title + ",Total:" + total + " Passed:" + passed + " Failed:" + failed;
        process.stdout.write(line + "\n");
    };

    var testStartTime = 0;

    this.on('start', function() {
        console.log('start');
    });

    this.on('end', function() {
        console.log('end');
    });

    this.on('suite:start', function(suite) {
        console.log('suite:start');
    });

    this.on('suite:end', function(suite) {
        console.log('suite:end');
    });

    this.on('test:start', function(test) {
        console.log('test:start');
        testStartTime = epoch();
    });

    this.on('test:pass', function(test) {
        console.log('test:pass', test);
        test.state = "passed";
    });

    this.on('test:fail', function(test) {
        console.log('test:fail', test);
        test.state = "failed";
    });

    this.on('test:pending', function(test) {
        console.log('test:pending', test);
        test.state = "pending";
    });

    this.on('test:end', function(test) {
        console.log('test:end', test);
        config.reporterOptions.totalTests++;
        if (test.state === "failed") {
            config.reporterOptions.failedTests++;
        } else if (test.state === "passed") {
            config.reporterOptions.passedTests++;
        }

        test.startTime = testStartTime;
        var item = reportItem(test, test.err || {});
        try {
            reportStream.write(JSON.stringify(item) + "\n");
        } catch(err) {
            process.stderr.write("error while writing: " + err.toString() + "\n");
        }
        reportStatusLine(test);
    });

    this.on('hook:start', function() {
        console.log('hook:start');
    });

    this.on('hook:end', function() {
        console.log('hook:end');
    });
};

TaurusReporter.reporterName = 'TaurusReporter';
util.inherits(TaurusReporter, events.EventEmitter);

function usage() {
    process.stdout.write("Taurus Webdriver.io Plugin\n");
    process.stdout.write("\n");
    process.stdout.write("Usage:\n");
    process.stdout.write("--report-file FILE - report file name\n");
    process.stdout.write("--test-suite FILE - path to test suite\n");
    process.stdout.write("--iterations N - Number of iterations over test suite\n");
    process.stdout.write("--hold-for N - Duration limit for a test suite\n");
}

function parseCmdline(argv) {
    var options = {iterations: 0, holdFor: 0};
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
        case "--test-suite":
            options.testSuite = args.shift();
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
    reportStream = fs.createWriteStream(config.reportFile || "report.ldjson");

    var configFile = "wdio.conf.js";
    var opts = {
        reporters: [TaurusReporter],
    }

    var wdio = new Launcher(configFile, opts);
    wdio.run().then(function (code) {
        if (reportStream) {
            reportStream.end();
        }
        process.exit(code);
    }, function (error) {
        console.error('Launcher failed to start the test', error.stacktrace);
        process.exit(1);
    });
}

if (require.main === module) {
    runWDIO();
}