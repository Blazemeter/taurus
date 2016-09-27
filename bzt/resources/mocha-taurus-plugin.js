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
}

function TaurusReporter(runner, config) {
    Mocha.reporters.Base.call(this, runner);

    var self = this;
    var reportStream = config.reporterOptions.reportStream;

    var testStartTime = null;

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
        test.startTime = testStartTime;
        var item = reportItem(test, test.err || {});
        try {
            reportStream.write(JSON.stringify(item) + "\n");
        } catch(err) {
            process.stdout.write("error while writing: " + err.toString() + "\n");
        }
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
            process.stdout.write("Unknown argument: %s" + arg.toString() + "\n");
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

function prepareMocha(config) {
    // clear 'require' cache to avoid mocha's test rediscovery issues
    // TODO: fix/report?
    Object.keys(require.cache).forEach(function(key) { delete require.cache[key]; });

    var engine = new Mocha({
        reporter: TaurusReporter,
        reporterOptions: {
            reportStream: config.reportStream
        }
    });

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
        process.stdout.write("Error: --test-suite is neither file nor directory\n");
        process.exit(1);
    }
    return engine;
}

function loopMocha(config, iterations, startTime, done) {
    if (iterations >= config.iterations) {
        done();
        return;
    }
    var offset = epoch() - startTime;
    if (config.holdFor > 0 && offset > config.holdFor) {
        done();
        return;
    }
    var engine = prepareMocha(config);
    engine.run(function() {
        loopMocha(config, iterations + 1, startTime, done);
    });
}

function runMocha() {
    var config = parseCmdline(process.argv);

    config.reportStream = fs.createWriteStream(config.reportFile || "report.ldjson");

    var done = function() {
        if (config.reportStream) {
            config.reportStream.end();
        }
        process.exit(0);
    };

    loopMocha(config, 0, epoch(), done);
}

if (require.main === module) {
    runMocha();
}
