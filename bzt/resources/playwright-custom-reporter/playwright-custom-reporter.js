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
      consoleLog: true,
      granularity: 'STEP',
    };
    this.options = {...defaultOptions, ...userOptions};

    // ENV overrides of options from configuration file (and defaults)
    if (typeof process.env.TAURUS_PWREPORT_DIR !== 'undefined') {
      this.options.outputFile = process.env.TAURUS_PWREPORT_DIR + '/' + 'taurus-playwright-reporter.jsonl';
    }
    if (typeof process.env.TAURUS_PWREPORT_VERBOSE !== 'undefined') {
      this.options.verbose = process.env.TAURUS_PWREPORT_VERBOSE?.toLowerCase() === 'true'
    }
    if (typeof process.env.TAURUS_PWREPORT_STDOUT !== 'undefined') {
      this.options.consoleLog = process.env.TAURUS_PWREPORT_STDOUT?.toLowerCase() === 'true'
    }
    if (typeof process.env.TAURUS_PWREPORT_DURATION !== 'undefined') {
      this.options.maxDuration = parseInt(process.env.TAURUS_PWREPORT_DURATION);
    }
    if (typeof process.env.TAURUS_PWREPORT_GRANULARITY !== 'undefined') {
      this.options.granularity = process.env.TAURUS_PWREPORT_GRANULARITY;
    }
    const normalizedGranularity = `${this.options.granularity}`.toUpperCase();
    this.options.granularity = ['TEST', 'STEP', 'STEP_LEAF'].includes(normalizedGranularity)
        ? normalizedGranularity : 'STEP';

    if (fs.existsSync(this.options.outputFile)) {
      fs.rmSync(this.options.outputFile, {
        force: true,
        maxRetries: 2
      });
    }

    this.tickStart(this.options.maxDuration || -1);
  }

  tickStart(maxDuration) {
    this.tick_time_id = setTimeout(async () => {
      const duration = Date.now() - this.timestampStart;
      if (maxDuration > 0 && duration > maxDuration) {
        if (this.options.consoleLog === true) {
          console.log(EC.red(`Maximum duration of ${maxDuration / 1000.0} seconds exceeded, stopping the process.`));
        }
        process.exit(0);
      }
      const statDuration = Date.now() - this.lastStatTimestamp;
      if (statDuration >= (this.options.statTime || 15000)) {
        const allTests = this.root ? this.root.allTests() : [];
        const expected = allTests.filter(t => t.outcome() === 'expected').length;
        const unexpected = allTests.filter(t => t.outcome() === 'unexpected').length;
        const flaky = allTests.filter(t => t.outcome() === 'flaky').length;
        const remaining = allTests.filter(t => t.outcome() === 'skipped').length;
        if (this.options.consoleLog === true) {
          console.log(`Finished tests: ${allTests.length
          - remaining}/${allTests.length}. Results expected: ${expected}, unexpected: ${unexpected}, flaky: ${flaky}`);
        }
        this.lastStatTimestamp = Date.now();
      }
      this.tickStart(maxDuration);
    }, this.options.tickTime || 1000);
  }

  tickStop() {
    clearTimeout(this.tick_time_id);
  }

  printsToStdio() {
    return this.options.consoleLog === true;
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
    if (this.options.consoleLog === true) {
      if ((this.options.maxDuration || -1) > 0) {
        console.log(`Starting the run with ${suite.allTests().length} tests, max duration: ${this.options.maxDuration / 1000.0} seconds`);
      } else {
        console.log(`Starting the run with ${suite.allTests().length} tests`);
      }
    }
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

    if (this.options.consoleLog === true && this.options.verbose === true) {
      console.log(`Finished test ${test.title}: ${result.status} in ${result.duration}ms`);
    }

    if (this.options.granularity !== 'TEST') {
      return;
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
      "error": result.status === 'passed' ? null : "Test failed: " + (result.error ? result.error.message : (result.status === 'interrupted'
          ? 'Interrupted' : 'Unknown error')),
      "runDetails": test.title + ":" + result.parallelIndex + ":" + test.repeatEachIndex
          + ":" + test.parent.parent?.title,
      "logs": test.logs && test.logs.length > 0 ? test.logs.join('\n') : null,
      "byte_count": null,
    };
    if (this.options.outputFile) {
      appendLineToFile(this.options.outputFile, JSON.stringify(line) + '\n');
    }
    if (this.options.consoleLog === true && this.options.verbose === true) {
      console.log(`Test result: ${JSON.stringify(line)}`);
    }
  }

  // Only first-level test.step() calls are reported (nested test.step calls are skipped).
  // Playwright's own category enum reserves 'test.step' exclusively for explicit
  // test.step() calls, so this already excludes hooks, fixtures, expects and pw:api steps.
  isFirstLevelStep(step) {
    if (!step || step.category !== 'test.step') {
      return false;
    }
    if (step.parent) {
      return false;
    }
    return true;
  }

  // Only leaf test.step() calls are reported - i.e. steps that do not themselves
  // contain a nested test.step() call, regardless of how deep they are in the hierarchy.
  isLeafStep(step) {
    if (!step || step.category !== 'test.step') {
      return false;
    }
    const hasNestedStep = Array.isArray(step.steps) && step.steps.some(child => child.category === 'test.step');
    return !hasNestedStep;
  }

  onStepEnd(test, result, step) {
    if (this.options.granularity === 'STEP') {
      if (!this.isFirstLevelStep(step)) {
        return;
      }
    } else if (this.options.granularity === 'STEP_LEAF') {
      if (!this.isLeafStep(step)) {
        return;
      }
    } else {
      return;
    }

    if (this.options.consoleLog === true && this.options.verbose === true) {
      console.log(`Finished step ${step.title}: ${step.error ? 'failed' : 'passed'} in ${step.duration}ms`);
    }

    const line = {
      "timestamp": step.startTime.getTime(),
      "label": step.title,
      "ok": !step.error,
      "concurency": this.config?.workers || 1,
      "duration": step.duration,
      "connectTime": null,
      "latency": null,
      "status": step.error ? "failed" : "passed",
      "expectedStatus": test.expectedStatus,
      "error": step.error ? "Step failed: " + step.error.message : null,
      "runDetails": test.title + ":" + result.parallelIndex + ":" + test.repeatEachIndex
          + ":" + test.parent.parent?.title,
      "logs": null,
      "byte_count": null,
    };
    if (this.options.outputFile) {
      appendLineToFile(this.options.outputFile, JSON.stringify(line) + '\n');
    }
    if (this.options.consoleLog === true && this.options.verbose === true) {
      console.log(`Step result: ${JSON.stringify(line)}`);
    }
  }

  onStdErr(chunk, test, result) {
    // Note that output may happen when no test is running, in which case this will be void.
    this.addTestLog(test || this.lastTest, EC.red(`${chunk}`));
    if (this.options.consoleLog === true) {
      console.log(EC.red(`${chunk}`));
    }
  }

  onStdOut(chunk, test, result) {
    // Note that output may happen when no test is running, in which case this will be void.
    this.addTestLog(test || this.lastTest, `${chunk}`);
    if (this.options.consoleLog === true) {
      console.log(`${chunk}`);
    }
  }

  // Called on some global error, for example unhandled exception in the worker process.
  onError(error) {
    // add the error to test logs
    this.addTestLog(this.lastTest, EC.red(error.message || "Unknown Error"));
    if (this.options.consoleLog === true) {
      console.log(EC.red(error.message || "Unknown Error"));
    }
  }

  async onEnd(result) {
    this.tickStop();
    const allTests = this.root ? this.root.allTests() : [];
    const expected = allTests.filter(t => t.outcome() === 'expected').length;
    const unexpected = allTests.filter(t => t.outcome() === 'unexpected').length;
    const flaky = allTests.filter(t => t.outcome() === 'flaky').length;
    const remaining = allTests.filter(t => t.outcome() === 'skipped').length;
    if (this.options.consoleLog === true) {
      console.log(
          `Final results expected: ${expected}, unexpected: ${unexpected}, flaky: ${flaky}, skipped: ${remaining}`);
      console.log(`Finished the run: ${result.status}`);
    }
    this.lastTest = undefined;
    this.testMap.clear();
  }
}

module.exports = TaurusReporter;