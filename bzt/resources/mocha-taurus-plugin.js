var Mocha = require("mocha"),
    fs = require("fs"),
    path = require("path");

function epoch() {
    return (new Date()).getTime() / 1000.0;
}

function reportItem(test, err) {
    // Test properties:
    // "title", "fn", "body", "timedOut", "pending", "type", "file",
    // "parent", "ctx", "callback", "timer", "skip", "duration",
    // "state","speed"

    // Test methods:
    // "isPending", "retries", "currentRetry", "fullTitle",

    var stateStatus = {
        "passed": "PASSED",
        "failed": "FAILED",
        "pending": "SKIPPED"
    };

    /*eslint-disable camelcase */
    return {
        test_case: test.title,
        test_suite: test.fullTitle(),
        status: stateStatus[test.state],
        start_time: test.startTime,
        duration: (test.duration) ? test.duration / 1000.0 : 0.0,
        error_msg: err.message || null,
        error_trace: err.stack || null,
        extras: {
            file: test.file || null
        }
    };
    /*eslint-enable camelcase */
}

function TaurusReporter(runner, config) {
    Mocha.reporters.Base.call(this, runner);

    var reportStream = config.reporterOptions.reportStream;

    var testStartTime = null;

    var reportStatusLine = function(lastTest) {
        var total = config.reporterOptions.totalTests;
        var passed = config.reporterOptions.passedTests;
        var failed = config.reporterOptions.failedTests;
        var line = lastTest.title + ",Total:" + total + " Passed:" + passed + " Failed:" + failed;
        process.stdout.write(line + "\n");
    };

    runner.on("start", function() {

    });

    runner.on("suite", function(suite) {

    });

    runner.on("suite end", function(suite) {

    });

    runner.on("test", function(test) {
        testStartTime = epoch();
    });

    runner.on("test end", function(test) {
        config.reporterOptions.totalTests++;
        if (test.state === "failed") {
            config.reporterOptions.failedTests++;
        } else if (test.state === "passed") {
            config.reporterOptions.passedTests++;
        }

        test.startTime = testStartTime;
        var item = reportItem(test, test.err || {});
        try {
            reportStream.write(JSON.stringify(item) + "\n", function() {
                config.reporterOptions.itemsWritten += 1;
            });
        } catch(err) {
            process.stderr.write("error while writing: " + err.toString() + "\n");
        }
        reportStatusLine(test);
    });

    runner.on("pending", function(test) {
        test.state = "pending";
    });

    runner.on("pass", function(test) {

    });

    runner.on("fail", function(test, err) {

    });

    runner.on("end", function() {

    });
}

function usage() {
    process.stdout.write("Taurus Mocha Plugin\n");
    process.stdout.write("\n");
    process.stdout.write("Usage:\n");
    process.stdout.write("--report-file FILE - path to report file\n");
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

function prepareMocha(config, mochaConfig) {
    // clear 'require' cache to avoid mocha's test rediscovery issues
    // TODO: fix/report?
    Object.keys(require.cache).forEach(function(key) { delete require.cache[key]; });

    var engine = new Mocha(mochaConfig);

    var stat = fs.statSync(config.testSuite);

    if (stat.isFile(config.testSuite)) {
        engine.addFile(config.testSuite);
    }
    else if (stat.isDirectory(config.testSuite)) {
        fs.readdirSync(config.testSuite).filter(function(file){
            return file.substr(-3) === ".js";
        }).forEach(function(file){
            engine.addFile(
                path.join(config.testSuite, file)
            );
        });
    } else {
        process.stderr.write("Error: --test-suite is neither file nor directory\n");
        process.exit(1);
    }
    return engine;
}

function loopMocha(config, mochaConfig, iterations, startTime, done) {
    if (iterations >= config.iterations) {
        done();
        return;
    }
    var offset = epoch() - startTime;
    if (config.holdFor > 0 && offset > config.holdFor) {
        done();
        return;
    }
    var engine = prepareMocha(config, mochaConfig);
    engine.run(function() {
        loopMocha(config, mochaConfig, iterations + 1, startTime, done);
    });
}

function runMocha() {
    var config = parseCmdline(process.argv);
    var reportStream = fs.createWriteStream(config.reportFile || "report.ldjson");

    var mochaConfig = {
        reporter: TaurusReporter,
        reporterOptions: {
            reportStream,
            totalTests: 0,
            passedTests: 0,
            failedTests: 0,
            itemsWritten: 0
        }
    };

    var done = function() {
        if (reportStream) {
            reportStream.end();
        }
        setInterval(function() {
            var totalTests = mochaConfig.reporterOptions.totalTests,
                itemsWritten = mochaConfig.reporterOptions.itemsWritten;
            if (itemsWritten >= totalTests) {
                process.exit(0);
            }
        }, 100);
    };

    loopMocha(config, mochaConfig, 0, epoch(), done);
}

if (require.main === module) {
    runMocha();
}
