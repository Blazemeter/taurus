const EC = require('eight-colors');
const fs = require('fs');

async function appendLineToFile(file, line) {
  try {
    await fs.appendFileSync(file, line, 'utf8');
  } catch (error) {
    console.error('Error appending data to file:', error);
  }
}

class TaurusReporter {

  constructor(userOptions = {}) {
    this.timestampStart = this.lastStatTimestamp = Date.now();
    this.testMap = new Map();

    const defaultOptions = {
      outputFile: 'taurus-playwright-reporter.jsonl',
      maxDuration: -1,
      tickTime: 1000,
      statTime: 15000,
      verbose: false,
    };
    this.options = { ...defaultOptions, ...userOptions };

    this.tickStart(this.options.maxDuration || -1);
  }

  tickStart(maxDuration) {
    this.tick_time_id = setTimeout(async () => {
      const duration = Date.now() - this.timestampStart;
      if (maxDuration > 0 && duration > maxDuration) {
        console.log(EC.red(`Maximum duration of ${maxDuration}ms exceeded, stopping the process.`));
        process.exit(0);
      }
      const statDuration = Date.now() - this.lastStatTimestamp;
      if (statDuration >= (this.options.statTime || 15000)) {
        const allTests = this.root ? this.root.allTests() : [];
        const expected = allTests.filter(t => t.outcome() === 'expected').length;
        const unexpected = allTests.filter(t => t.outcome() === 'unexpected').length;
        const flaky = allTests.filter(t => t.outcome() === 'flaky').length;
        const remaining = allTests.filter(t => t.outcome() === 'skipped').length;
        console.log(`Finished tests: ${allTests.length - remaining}/${allTests.length}. Results expected: ${expected}, unexpected: ${unexpected}, flaky: ${flaky}`);
        this.lastStatTimestamp = Date.now();
      }
      this.tickStart(maxDuration);
    }, this.options.tickTime || 1000);
  }

  tickStop() {
    clearTimeout(this.tick_time_id);
  }

  printsToStdio() {
    return true;
  }

  getTest(testId) {
    return this.testMap.get(testId);
  }

  addTestLog(test, log) {
    if (test && test.logs) {
      // log could be Buffer
      test.logs.push(`${log}`);
    }
  }

  onBegin(config, suite) {
    this.config = config;
    this.root = suite;
    console.log(`Starting the run with ${suite.allTests().length} tests.`);
  }

  onTestBegin(test, result) {
    // For logs when no test is running
    this.lastTest = test;

    this.testMap.set(test.id, test);

    if (!test.timestamps) {
      test.timestamps = [];
    }
    test.timestamps.push(Date.now());

    if (test.logs) {
      const retryLogs = ['\n', EC.yellow(`Retry #${result.retry}`), '\n'].join('');
      this.addTestLog(test, retryLogs);

    } else {
      test.logs = [];
    }
  }

  onTestEnd(test, result) {

    if (this.options.verbose === true) {
      console.log(`Finished test ${test.title}: ${result.status} in ${result.duration}ms`);
    }

    const timestamp = test.timestamps ? test.timestamps[test.timestamps.length - 1] : Date.now();

    const line = {
      "timestamp": timestamp,
      "label": test.title,
      "ok": test.ok() && result.status !== 'interrupted',
      "concurency": this.config?.workers || 1,
      "duration": result.duration,
      "connectTime": null,
      "latency": null,
      "status": result.status,
      "expectedStatus": test.expectedStatus,
      "error": result.status === 'passed' ? null : "Test failed: " + (result.error ? result.error.message : (result.status === 'interrupted' ? 'Interrupted' : 'Unknown error')),
      "runDetails": test.title + ":" + result.parallelIndex + ":" + test.repeatEachIndex
          + ":" + test.parent.parent?.title,
      "logs": test.logs && test.logs.length > 0 ? test.logs.join('\n') : null,
      "byte_count": null,
    };
    if (this.options.outputFile) {
      // TODO: configure outputFile to be full path with artifacts dir
      appendLineToFile(test.parent.project()?.outputDir + "/../" + this.options.outputFile, JSON.stringify(line) + '\n');
    }
    if (this.options.verbose === true) {
      console.log(`Test result: ${JSON.stringify(line)}`);
    }
  }


  onStdErr(chunk, test, result) {
    // Note that output may happen when no test is running, in which case this will be void.
    this.addTestLog(test || this.lastTest, EC.red(`${chunk}`));
    console.log(EC.red(`${chunk}`));
  }

  onStdOut(chunk, test, result) {
    // Note that output may happen when no test is running, in which case this will be void.
    this.addTestLog(test || this.lastTest, `${chunk}`);
    console.log(`${chunk}`);
  }

  // Called on some global error, for example unhandled exception in the worker process.
  onError(error) {
    // add the error to test logs
    this.addTestLog(this.lastTest, EC.red(error.message || "Unknown Error"));
    console.log(EC.red(error.message || "Unknown Error"));
  }

  async onEnd(result) {
    this.tickStop();
    const allTests = this.root ? this.root.allTests() : [];
    const expected = allTests.filter(t => t.outcome() === 'expected').length;
    const unexpected = allTests.filter(t => t.outcome() === 'unexpected').length;
    const flaky = allTests.filter(t => t.outcome() === 'flaky').length;
    const remaining = allTests.filter(t => t.outcome() === 'skipped').length;
    console.log(`Final results expected: ${expected}, unexpected: ${unexpected}, flaky: ${flaky}, skipped: ${remaining}`);
    console.log(`Finished the run: ${result.status}`);
    this.lastTest = undefined;
    this.testMap.clear();
  }
}

module.exports = TaurusReporter;