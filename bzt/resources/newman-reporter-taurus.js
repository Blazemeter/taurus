"use strict";

const fs = require("fs");

function epoch() {
    return (new Date()).getTime() / 1000.0;
}

class TaurusReporter {
    constructor(emitter, reporterOptions, options) {
        this.reporterOptions = reporterOptions;
        this.options = options;
        this.err = null;
        const events = "start beforeIteration iteration beforeItem item beforePrerequest prerequest beforeScript script beforeRequest request beforeTest test beforeAssertion assertion console exception beforeDone done".split(" ");
        events.forEach((e) => {
            if (typeof this[e] === "function") { emitter.on(e, (err, args) => this[e](err, args)); }
        });
    }

    start(err, args) {
        if (err) {
            this.currItem.passed = false;
            this.err = err;
        }
        // eslint-disable-next-line no-console
        console.log(`[testSuiteStarted name='${this.options.collection.name}']`);
        this.reportStream = fs.createWriteStream(this.reporterOptions.filename);
    }

    beforeItem(err, args) {
        this.err = null;
        this.currItem = {
            name: this.itemName(args.item),
            passed: true,
            assertions: [],
            startTime: epoch()
        };
        if (err) {
            this.currItem.passed = false;
            this.err = err;
        }
        // eslint-disable-next-line no-console
        console.log(`[testStarted name='${this.currItem.name}' captureStandardOutput='true']`);
    }

    request(err, args) {
        if (err) {
            this.currItem.passed = false;
            this.err = err;
        }
        this.currItem.request = args.request;
        this.currItem.response = args.response;
        this.currItem.cookies = args.cookies;
    }

    assertion(err, args) {
        this.currItem.assertions.push({
            message: args.assertion,
            isFailed: (err) ? true : false,
            error: args.error
        });
        if (err) {
            this.currItem.passed = false;
        }
    }

    item(err, args) {
        // eslint-disable-next-line no-console
        console.log(`[testFinished name='${this.currItem.name}']`);

        try {
            const item = this.reportItem(this.currItem);
            this.reportStream.write(JSON.stringify(item) + "\n", function () {
            //config.reporterOptions.itemsWritten += 1;
            });
        } catch (err) {
            // eslint-disable-next-line no-console
            console.error("error while writing: " + err.toString() + "\n");
        }
    }

    done(err, args) {
        // eslint-disable-next-line no-console
        console.log(`[testSuiteFinished name='${this.options.collection.name}']`);
        this.reportStream.end();
    }

    /* HELPERS */
    itemName(item) {
        const parentName = item.parent() && item.parent().name ? item.parent().name : "";
        const folderOrEmpty = parentName === this.options.collection.name ? "" : parentName + "/";
        return (folderOrEmpty + item.name);
    }

    extractAssertions(item, sample) {
        var assertions = [];
        for (var i = 0; i < item.assertions.length; i++) {
            var assertion = item.assertions[i];
            if (assertion.isFailed) {
                /*eslint-disable camelcase */
                if (!sample.error_msg) {
                    sample.error_msg = assertion.message;
                    sample.error_trace = assertion.error.stack;
                }
                /*eslint-enable camelcase */
            }
            assertions.push({
                name: assertion.message,
                isFailed: assertion.isFailed,
                errorMessage: (assertion.error) ? assertion.error.name + ": " + assertion.message : null
            });
        }
        return assertions;
    }

    reportItem(item) {
        /*eslint-disable camelcase */
        var sample = {
            test_case: item.name,
            test_suite: this.options.collection.name,
            start_time: item.startTime,
            duration: (item.response && item.response.responseTime) / 1000.0 || 0,
            error_msg: null,
            error_trace: null,
            extras: {}
        };
        /*eslint-enable camelcase */
        var assertions = this.extractAssertions(item, sample);
        if (item.response) {
            var requestHeaders = item.request.headers.toObject(false, true);
            var responseHeaders = item.response.headers.toObject(false, true);
            var requestCookies = {};
            item.cookies.members.forEach(function (elem) { requestCookies[elem.name] = elem.value; });
            sample.extras = {
                responseCode: item.response.code,
                responseMessage: item.response.status,
                responseTime: item.response.responseTime,
                connectTime: 0,
                latency: 0,
                responseSize: item.response.responseSize,
                requestMethod: item.request.method,
                requestURI: item.request.url.toString(),
                requestHeaders,
                responseHeaders,
                requestCookies,
                assertions,

                // TODO
                requestBody: "",
                responseBody: "",
                requestCookiesRaw: "",
                requestSize: 0,
                requestBodySize: 0,
                responseBodySize: 0,
                requestCookiesSize: 0
            };
            sample.assertions = assertions;
        }

        // Calculate status from assertions
        const assertionFailed = sample.assertions.some((ast) => ast.isFailed);
        sample.status = item.passed && !assertionFailed ? "PASSED" : "FAILED";

        if (!item.passed || assertionFailed) {
            const msg = item.assertions.filter((a) => a.isFailed).map((a) => a.name).join(", ");
            const responseCode = (item.response && item.response.responseCode) || "-";
            const reason = (item.response && item.response.reason()) || "-";
            const details = (`Response code: ${responseCode}, reason: ${reason}`);
            // eslint-disable-next-line no-console
            console.log(`[testFailed name='${this.currItem.name}' message='${msg}' details='${msg} - ${details}']`);
        }

        return sample;
    }
}

module.exports = TaurusReporter;
