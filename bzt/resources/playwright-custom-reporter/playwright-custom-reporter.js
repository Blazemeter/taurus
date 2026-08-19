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
      granularity: 'AUTO',
      noReportPrefix: '',
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
    this.options.granularity = ['TEST', 'STEP', 'STEP_LEAF', 'AUTO'].includes(normalizedGranularity)
        ? normalizedGranularity : 'AUTO';
    if (typeof process.env.TAURUS_PWREPORT_NOREPORT_PREFIX !== 'undefined') {
      this.options.noReportPrefix = process.env.TAURUS_PWREPORT_NOREPORT_PREFIX;
    }

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

  addStepLog(step, log) {
    if (step && step.logs) {
      // log could be Buffer
      step.logs.push(`${log}`);
    }
  }

  // Empty prefix (the default) means nothing is excluded.
  isSkippedByNoReportPrefix(title) {
    if (!this.options.noReportPrefix) {
      return false;
    }
    return typeof title === 'string' && title.startsWith(this.options.noReportPrefix);
  }

  // The no-report prefix disables the whole tree under the step/test it is found on:
  // checks the test title and every step from the given one up through its parents.
  isExcludedByNoReportPrefix(test, step) {
    if (!this.options.noReportPrefix) {
      return false;
    }
    if (this.isSkippedByNoReportPrefix(test?.title)) {
      return true;
    }
    for (let current = step; current; current = current.parent) {
      if (this.isSkippedByNoReportPrefix(current.title)) {
        return true;
      }
    }
    return false;
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
    this.lastStep = undefined;
    // Tracks whether any step already reported this attempt's failure, so onTestEnd
    // knows whether a fallback whole-test line is needed under STEP/STEP_LEAF granularity.
    test.reportedFailure = false;
    // Tracks whether any step was reported at all this attempt, so onTestEnd can decide,
    // under AUTO granularity, whether to report this test like STEP (steps already
    // covered it) or fall back to one whole-test line (this test used no test.step()).
    test.reportedAnyStep = false;

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

  buildTestLine(test, result) {
    const timestamp = test.timestamps ? test.timestamps[test.timestamps.length - 1] : Date.now();
    return {
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
          + ":" + test.parent?.parent?.title,
      "logs": test.logs && test.logs.length > 0 ? test.logs.join('\n') : null,
      "byte_count": null,
    };
  }

  // Generic marker line used when STEP/STEP_LEAF granularity has no step-level failure
  // to attribute a problem to (whole-test timeout, failure outside any test.step, etc).
  // Uses a fixed, clearly-synthetic label (not the test title) so it reads as one bucket
  // across all tests, and zeroes out the fields that would otherwise be treated as real
  // performance measurements - only the "error" message carries the actual test title.
  buildUntrackedFailureLine(test, result) {
    const reason = result.error ? result.error.message : (result.status === 'interrupted' ? 'Interrupted' : 'Unknown error');
    return {
      "timestamp": test.timestamps ? test.timestamps[test.timestamps.length - 1] : Date.now(),
      "label": "__untracked_failure__",
      "ok": false,
      "concurency": this.config?.workers || 1,
      "duration": 0,
      "connectTime": null,
      "latency": null,
      "status": result.status,
      "expectedStatus": test.expectedStatus,
      "error": `Test "${test.title}" ${result.status}, no step captured it: ${reason}`,
      "runDetails": test.title + ":" + result.parallelIndex + ":" + test.repeatEachIndex
          + ":" + test.parent?.parent?.title,
      "logs": test.logs && test.logs.length > 0 ? test.logs.join('\n') : null,
      "byte_count": 0,
    };
  }

  onTestEnd(test, result) {

    if (this.options.consoleLog === true && this.options.verbose === true) {
      console.log(`Finished test ${test.title}: ${result.status} in ${result.duration}ms`);
    }

    if (this.isSkippedByNoReportPrefix(test.title)) {
      return;
    }

    if (this.options.granularity === 'TEST') {
      const line = this.buildTestLine(test, result);
      if (this.options.outputFile) {
        appendLineToFile(this.options.outputFile, JSON.stringify(line) + '\n');
      }
      if (this.options.consoleLog === true && this.options.verbose === true) {
        console.log(`Test result: ${JSON.stringify(line)}`);
      }
      return;
    }

    // AUTO granularity, and this test never reported a step (either it doesn't use
    // test.step() at all, or every step was excluded by report-exclude-prefix): report
    // it like TEST, unconditionally (pass or fail) - so it's never silently dropped.
    if (this.options.granularity === 'AUTO' && !test.reportedAnyStep) {
      const line = this.buildTestLine(test, result);
      if (this.options.outputFile) {
        appendLineToFile(this.options.outputFile, JSON.stringify(line) + '\n');
      }
      if (this.options.consoleLog === true && this.options.verbose === true) {
        console.log(`Test result: ${JSON.stringify(line)}`);
      }
      return;
    }

    // STEP / STEP_LEAF granularity, or AUTO with at least one reported step: if the
    // test didn't pass and no step already reported the failure (e.g. a whole-test
    // timeout with every step showing passed, or a failure outside any test.step),
    // fall back to a synthetic marker line - otherwise this failure would be
    // completely invisible at this granularity.
    if (result.status !== 'passed' && !test.reportedFailure) {
      const line = this.buildUntrackedFailureLine(test, result);
      if (this.options.outputFile) {
        appendLineToFile(this.options.outputFile, JSON.stringify(line) + '\n');
      }
      if (this.options.consoleLog === true) {
        console.log(EC.yellow(`Untracked test failure, emitting synthetic marker line: ${JSON.stringify(line)}`));
      }
    }
  }

  // Only first-level test.step() calls are reported (nested test.step calls are skipped).
  // "First-level" means no ancestor in the parent chain is itself a test.step. A parent
  // that is a hook/fixture/pw:api step does not count as nesting - it isn't something the
  // test author wrote. This matters because Playwright can attribute still-running
  // test.step() calls to its own teardown fixture (e.g. "Fixture \"context\"") as their
  // parent when a whole-test timeout races with in-flight user code, which would otherwise
  // make a genuinely top-level step look nested and get silently dropped.
  isFirstLevelStep(step) {
    if (!step || step.category !== 'test.step') {
      return false;
    }
    for (let current = step.parent; current; current = current.parent) {
      if (current.category === 'test.step') {
        return false;
      }
    }
    return true;
  }

  // Only leaf test.step() calls are reported - i.e. steps that do not themselves
  // contain a nested test.step() call, regardless of how deep they are in the hierarchy.
  // A step also counts as a leaf when every one of its test.step children is excluded by
  // the no-report prefix: that step becomes the effective leaf for whatever survived under it.
  isLeafStep(test, step) {
    if (!step || step.category !== 'test.step') {
      return false;
    }
    if (this.isExcludedByNoReportPrefix(test, step)) {
      return false;
    }
    const children = Array.isArray(step.steps) ? step.steps : [];
    if (!this.options.noReportPrefix) {
      // No prefix configured - nothing can be excluded, skip checking children for it.
      return !children.some(child => child.category === 'test.step');
    }
    const hasReportableChildStep = children.some(child =>
        child.category === 'test.step' && !this.isExcludedByNoReportPrefix(test, child));
    return !hasReportableChildStep;
  }

  onStepBegin(test, result, step) {
    this.lastStep = step;
    if (!step.logs) {
      step.logs = [];
    }
  }

  onStepEnd(test, result, step) {
    let shouldReport;
    if (this.options.granularity === 'STEP' || this.options.granularity === 'AUTO') {
      // AUTO decides per-test (in onTestEnd) whether to behave like STEP or TEST, based
      // on whether the test reports any step at all - so while a step is being reported,
      // it's always evaluated with plain first-level STEP rules.
      shouldReport = this.isFirstLevelStep(step) && !this.isExcludedByNoReportPrefix(test, step);
    } else if (this.options.granularity === 'STEP_LEAF') {
      // isLeafStep() already accounts for the no-report prefix (both self- and
      // children-exclusion), no separate isExcludedByNoReportPrefix check needed here.
      shouldReport = this.isLeafStep(test, step);
    } else {
      shouldReport = false;
    }

    if (shouldReport) {
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
            + ":" + test.parent?.parent?.title,
        "logs": step.logs && step.logs.length > 0 ? step.logs.join('\n') : null,
        "byte_count": null,
      };
      if (step.error && test) {
        test.reportedFailure = true;
      }
      if (test) {
        test.reportedAnyStep = true;
      }
      if (this.options.outputFile) {
        appendLineToFile(this.options.outputFile, JSON.stringify(line) + '\n');
      }
      if (this.options.consoleLog === true && this.options.verbose === true) {
        console.log(`Step result: ${JSON.stringify(line)}`);
      }
    }

    // Leaving this step's scope - current step becomes its parent (or none, at top level).
    this.lastStep = step ? step.parent : this.lastStep;
  }

  onStdErr(chunk, test, result) {
    // Note that output may happen when no test is running, in which case this will be void.
    this.addTestLog(test || this.lastTest, EC.red(`${chunk}`));
    this.addStepLog(this.lastStep, EC.red(`${chunk}`));
    if (this.options.consoleLog === true) {
      console.log(EC.red(`${chunk}`));
    }
  }

  onStdOut(chunk, test, result) {
    // Note that output may happen when no test is running, in which case this will be void.
    this.addTestLog(test || this.lastTest, `${chunk}`);
    this.addStepLog(this.lastStep, `${chunk}`);
    if (this.options.consoleLog === true) {
      console.log(`${chunk}`);
    }
  }

  // Called on some global error, for example unhandled exception in the worker process.
  onError(error) {
    // add the error to test and step logs
    this.addTestLog(this.lastTest, EC.red(error.message || "Unknown Error"));
    this.addStepLog(this.lastStep, EC.red(error.message || "Unknown Error"));
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
    this.lastStep = undefined;
    this.testMap.clear();
  }
}

module.exports = TaurusReporter;