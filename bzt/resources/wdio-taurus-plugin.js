var fs = require("fs");
var Launcher = require("@wdio/cli").default;

function epoch() {
    return (new Date()).getTime() / 1000.0;
}

function reportItem(test, status, err) {
    var stateStatus = {
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

var reportStream = null;

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
        reporters: [[__dirname + '/wdio-custom-reporter.js', {
            reportFile: config.reportFile,
        }]],
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