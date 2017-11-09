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
        const events = 'start beforeIteration iteration beforeItem item beforePrerequest prerequest beforeScript script beforeRequest request beforeTest test beforeAssertion assertion console exception beforeDone done'.split(' ');
        events.forEach((e) => {
            if (typeof this[e] === 'function') emitter.on(e, (err, args) => this[e](err, args))
        });
    }

    start(err, args) {
        if (err) {
            this.currItem.passed = false;
            this.err = err;
        }
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
        console.log(`[testFinished name='${this.currItem.name}']`);

        try {
            const item = this.reportItem(this.currItem);
            this.reportStream.write(JSON.stringify(item) + "\n", function () {
            //config.reporterOptions.itemsWritten += 1;
            });
        } catch (err) {
            console.error("error while writing: " + err.toString() + "\n");
        }
    }

    done(err, args) {
        console.log(`[testSuiteFinished name='${this.options.collection.name}']`);
        this.reportStream.end();
    }

    /* HELPERS */
    itemName(item) {
        const parentName = item.parent() && item.parent().name ? item.parent().name : "";
        const folderOrEmpty = parentName === this.options.collection.name ? "" : parentName + "/";
        return (folderOrEmpty + item.name);
    }

    reportItem(item) {
        /*eslint-disable camelcase */
        var item = {
            test_case: item.name,
            test_suite: this.options.collection.name,
            status: item.passed ? "PASSED" : "FAILED",
            start_time: item.startTime,
            duration: (item.response && item.response.responseTime) / 1000.0 || 0,
            error_msg: null,
            error_trace: null,
            extras: {}
        };
        var assertions = [];
        for (var i = 0; i < this.currItem.assertions.length; i++) {
            var assertion = this.currItem.assertions[i];
            if (assertion.isFailed) {
                if (!item.error_msg) {
                    item.error_msg = assertion.message;
                    item.error_trace = assertion.error.stack;
                }
            }
            assertions.push({
                name: assertion.message,
                isFailed: assertion.isFailed,
                errorMessage: (assertion.error) ? assertion.error.name + ": " + assertion.message : null
            })
        }
        /*eslint-enable camelcase */

        if (this.currItem.response) {
            var requestHeaders = this.currItem.request.headers.toObject(false, true);
            var responseHeaders = this.currItem.response.headers.toObject(false, true);
            var requestCookies = {};
            this.currItem.cookies.forEach(function (elem) { requestCookies[elem.name] = elem.value; })
            item.extras = {
                responseCode: this.currItem.response.code,
                responseMessage: this.currItem.response.status,
                responseTime: this.currItem.response.responseTime,
                connectTime: 0,
                latency: 0,
                responseSize: this.currItem.response.responseSize,
                requestMethod: this.currItem.request.method,
                requestURI: this.currItem.request.url.toString(),
                requestHeaders: requestHeaders,
                responseHeaders: responseHeaders,
                requestCookies: requestCookies,
                assertions: assertions,

                // TODO
                requestBody: "",
                responseBody: "",
                requestCookiesRaw: "",
                requestSize: 0,
                requestBodySize: 0,
                responseBodySize: 0,
                requestCookiesSize: 0
            }
        }

        if (!this.currItem.passed) {
            const msg = this.currItem.assertions.filter((a) => a.isFailed).map(a => a.name).join(", ");
            const responseCode = (this.currItem.response && this.currItem.response.responseCode) || "-";
            const reason = (this.currItem.response && this.currItem.response.reason()) || "-";
            const details = (`Response code: ${responseCode}, reason: ${reason}`);
            console.log(`[testFailed name='${this.currItem.name}' message='${msg}' details='${msg} - ${details}']`);
        }

        return item;
    }
}

module.exports = TaurusReporter;