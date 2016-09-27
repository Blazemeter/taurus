var Mocha = require('mocha'),
    fs = require('fs'),
    path = require('path');

function TaurusReporter(runner, config) {
    console.log("creating reporter");
    Mocha.reporters.Base.call(this, runner);

    var self = this;
    var reportStream = null;

    var testStartTime = null;

    runner.on('start', function() {
        console.log('start');
        reportStream = fs.createWriteStream(config.reportFile || 'report.ldjson');
    });

    runner.on('suite', function(suite) {
        console.log('suite');
    });

    runner.on('suite end', function(suite) {
        console.log('suite end');
    });

    runner.on('test', function(test) {
        console.log('test');
        testStartTime = epoch();
    });

    runner.on('test end', function(test) {
        console.log('test end');
        test.startTime = testStartTime;
        if (!test.duration)
            test.duration = 0.0;
        var item = reportItem(test, test.err || {});
        reportStream.write(JSON.stringify(item) + "\n");
    });

    runner.on('pending', function(test) {
        console.log('pending');
        test.state = "pending";
    });

    runner.on('pass', function(test) {
        console.log('pass');
    });

    runner.on('fail', function(test, err) {
        console.log('fail');
    });

    runner.on('end', function() {
        console.log('end');
        reportStream && reportStream.end();
    });
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

function epoch() {
    return (new Date()).getTime() / 1000.0;
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

function prepareMocha(config) {
    var engine = new Mocha({
        reporter: TaurusReporter,
        reporterOptions: {
            reportFile: config.reportFile
        }
    });

    var stat = fs.statSync(config.testSuite);

    if (stat.isFile(config.testSuite))
        engine.addFile(config.testSuite);
    else if (stat.isDirectory(config.testSuite)) {
        fs.readdirSync(config.testSuite).filter(function(file){
            return file.substr(-3) === '.js';
        }).forEach(function(file){
            console.log('adding test suite file', file);
            engine.addFile(
                path.join(config.testSuite, file)
            );
        });
    } else {
        console.log("Error: --test-suite is neither file nor directory");
        process.exit(1);
    }
    return engine;
}

function runMocha() {
    var config = parseCmdline(process.argv);
    loopMocha(config, 0, epoch());
}

function loopMocha(config, iterations, startTime) {
    console.log(epoch(), config, iterations);
    if (iterations >= config.iterations) {
        console.log(epoch(), "iteration limit reached");
        return;
    }
    var offset = epoch() - startTime;
    if (config.holdFor > 0 && offset > config.holdFor) {
        console.log(epoch(), "duration limit reached");
        return;
    }
    var engine = prepareMocha(config);
    console.log(engine);
    engine.run(function() {
        loopMocha(config, iterations + 1, startTime);
    });
}

if (require.main === module) {
    runMocha();
}
