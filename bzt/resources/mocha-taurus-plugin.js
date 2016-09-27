var Mocha = require('mocha'),
    fs = require('fs'),
    path = require('path'),
    debug = require('debug')('taurus');

function TaurusReporter(runner, config) {
    Mocha.reporters.Base.call(this, runner);

    var self = this;
    var reportStream = config.reporterOptions.reportStream;

    var testStartTime = null;

    runner.on('start', function() {
        debug('start');
    });

    runner.on('suite', function(suite) {
        debug('suite');
    });

    runner.on('suite end', function(suite) {
        debug('suite end');
    });

    runner.on('test', function(test) {
        debug('test');
        testStartTime = epoch();
    });

    runner.on('test end', function(test) {
        debug('test end');
        test.startTime = testStartTime;
        var item = reportItem(test, test.err || {});
        try {
            reportStream.write(JSON.stringify(item) + "\n");
        } catch(err) {
            debug('error while writing', err);
        }
    });

    runner.on('pending', function(test) {
        debug('pending');
        test.state = "pending";
    });

    runner.on('pass', function(test) {
        debug('pass');
    });

    runner.on('fail', function(test, err) {
        debug('fail');
    });

    runner.on('end', function() {
        debug('end');
    });
};

function reportItem(test, err) {
    // Test properties:
    // 'title', 'fn', 'body', 'timedOut', 'pending', 'type', 'file',
    // 'parent', 'ctx', 'callback', 'timer', 'skip', 'duration',
    // 'state','speed'

    // Test methods:
    // 'isPending', 'retries', 'currentRetry', 'fullTitle',

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

function parseCmdline(argv) {
    var options = {iterations: 0, holdFor: 0};
    var args = argv.slice(2);
    while (args) {
        var arg = args.shift();
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

function prepareMocha(config) {
    // clear 'require' cache to avoid mocha's test rediscovery issues
    // TODO: fix/report?
    Object.keys(require.cache).forEach(function(key) { delete require.cache[key] })

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
            return file.substr(-3) === '.js';
        }).forEach(function(file){
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

    config.reportStream = fs.createWriteStream(config.reportFile || 'report.ldjson');

    var done = function() {
        if (config.reportStream)
            config.reportStream.end();
        process.exit(0);
    }

    loopMocha(config, 0, epoch(), done);
}

function loopMocha(config, iterations, startTime, done) {
    if (iterations >= config.iterations) {
        debug('iteration limit reached');
        done();
        return;
    }
    var offset = epoch() - startTime;
    if (config.holdFor > 0 && offset > config.holdFor) {
        debug('duration limit reached');
        done();
        return;
    }
    debug('iteration #%s', iterations);
    var engine = prepareMocha(config);
    engine.run(function() {
        loopMocha(config, iterations + 1, startTime, done);
    });
}

if (require.main === module) {
    runMocha();
}
