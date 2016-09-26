var mocha = require('mocha'),
    fs = require('fs'),
    path = require('path');
module.exports = TaurusReporter;

function TaurusReporter(runner) {
    mocha.reporters.Base.call(this, runner);

    var self = this;
    var total = runner.total;
    var reportFile = null;
    var suiteStack = [];
    var testStartTime = null;

    runner.on('start', function() {
        reportFile = fs.createWriteStream(process.env.SELENIUM_TEST_REPORT || 'report.ldjson');
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


function reportItem(test, err) {
    // Test properties:
    // 'title',
    // 'fn',
    // 'body',
    // 'timedOut',
    // 'pending',
    // 'type',
    // 'file',
    // 'parent',
    // 'ctx',
    // 'callback',
    // 'timer',
    // 'skip',
    // 'duration',
    // 'state',
    // 'speed'

    // Test methods:
    // 'isPending',
    // 'retries',
    // 'currentRetry',
    // 'fullTitle',

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
            file: test.file || null,
        }
    };
}

function runMocha() {
    var engine = new mocha({
        reporter: TaurusReporter
    });

    var testDir = 'test'

    fs.readdirSync(testDir).filter(function(file){
        return file.substr(-3) === '.js';
    }).forEach(function(file){
        engine.addFile(
            path.join(testDir, file)
        );
    });

    engine.run(function(failures){
        process.on('exit', function () {
            process.exit(failures);
        });
    });
}

if (require.main === module) {
    runMocha();
}
