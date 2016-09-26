var mocha = require('mocha'),
    fs = require('fs'),
    path = require('path');

function createReporter(config) {
    function TaurusReporter(runner) {
        mocha.reporters.Base.call(this, runner);

        var self = this;
        var total = runner.total;
        var reportFile = null;
        var suiteStack = [];
        var testStartTime = null;

        runner.on('start', function() {
            reportFile = fs.createWriteStream(config.reportFile || 'report.ldjson');
        });

        runner.on('suite', function(suite) {
            suiteStack.push(suite);
        });

        runner.on('suite end', function(suite) {
            suiteStack.pop();
        });

        runner.on('test', function(test) {
            testStartTime = (new Date()).getTime() / 1000.0;
        });

        runner.on('test end', function(test) {
            test.startTime = testStartTime;
            if (!test.duration)
                test.duration = 0.0;
            reportFile.write(JSON.stringify(reportItem(test, test.err || {})) + "\n");
        });

        runner.on('pending', function(test) {
            test.state = "pending";
        });

        runner.on('pass', function(test) {

        });

        runner.on('fail', function(test, err) {

        });

        runner.on('end', function() {
            reportFile.end();
        });
    };
    return TaurusReporter;
};

function parseCmdline(argv) {
    options = {iterations: 0, holdFor: 0};
    args = argv.slice(2);
    while (args) {
        arg = args.shift();
        if (!arg)
            break;
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
            console.log("Unknown argument", arg);
            process.exit(1);
            break;
        }
    }

    if (options.iterations == 0) {
        if (options.holdFor > 0) {
            options.iterations = Infinity;
        } else {
            options.iterations = 1;
        }
    }

    return options;
}

function usage() {
    console.log("Taurus Mocha Plugin");
    console.log();
    console.log("Usage:");
    console.log("--report-file FILE - path to report file");
    console.log("--test-suite FILE - path to test suite");
    console.log("--iterations N - Number of iterations over test suite");
    console.log("--hold-for N - Duration limit for a test suite");
}

function reportItem(test, err) {
    // Test properties:
    // 'title', 'fn', 'body', 'timedOut', 'pending', 'type', 'file',
    // 'parent', 'ctx', 'callback', 'timer', 'skip', 'duration',
    // 'state','speed'

    // Test methods:
    // 'isPending', 'retries', 'currentRetry', 'fullTitle',

    stateStatus = {
        "passed": "PASSED",
        "failed": "FAILED",
        "pending": "SKIPPED"
    };

    return {
        test_case: test.title,
        test_suite: test.fullTitle(),
        status: stateStatus[test.state],
        start_time: test.startTime,
        duration: test.duration / 1000.0,
        error_msg: err.message || null,
        error_trace: err.stack || null,
        extras: {
            file: test.file || null
        }
    };
}

function runMocha() {
    config = parseCmdline(process.argv);

    var engine = new mocha({
        reporter: createReporter(config)
    });

    var stat = fs.statSync(config.testSuite);

    if (stat.isFile(config.testSuite))
        engine.addFile(config.testSuite);
    else if (stat.isDirectory(config.testSuite)) {
        fs.readdirSync(config.testSuite).filter(function(file){
            return file.substr(-3) === '.js';
        }).forEach(function(file){
            engine.addFile(
                path.join(config.testSuite, file)
            );
        });
    } else
        console.log("Error: --test-suite is neither file nor directory.");

    var startTime = (new Date()).getTime() / 1000.0;
    var iterations = options.iterations;
    while (iterations > 0) {
        engine.run();
        var offset = (new Date()).getTime() / 1000.0 - startTime;
        if (config.holdFor > 0 && offset > config.holdFor)
            break;
        iterations -= 1;
    }
}

if (require.main === module) {
    runMocha();
}
