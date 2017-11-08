"use strict";

const fs = require("fs");

function epoch() {
    return (new Date()).getTime() / 1000.0;
}

class TaurusReporter {
    constructor(emitter, reporterOptions, options) {
        this.reporterOptions = reporterOptions;
        this.options = options;
        const events = 'start beforeIteration iteration beforeItem item beforePrerequest prerequest beforeScript script beforeRequest request beforeTest test beforeAssertion assertion console exception beforeDone done'.split(' ');
        events.forEach((e) => {
            if (typeof this[e] === 'function') emitter.on(e, (err, args) => this[e](err, args))
        });
    }

    start(err, args) {
        console.log(`[testSuiteStarted name='${this.options.collection.name}']`);
        this.reportStream = fs.createWriteStream(this.reporterOptions.filename);
    }

    beforeItem(err, args) {
        this.currItem = {name: this.itemName(args.item), passed: true, failedAssertions: []};
        console.log(`[testStarted name='${this.currItem.name}' captureStandardOutput='true']`);
        //console.log(args);
    }

    request(err, args) {
        if (!err) {
            this.currItem.response = args.response;
        }
    }

    assertion(err, args) {
        if (err) {
            this.currItem.passed = false;
            this.currItem.failedAssertions.push(args.assertion);
        }
    }

    item(err, args) {
        console.log(`[testFinished name='${this.currItem.name}']`);
        console.log(this.currItem);

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


    reportItem(test) {
        /*eslint-disable camelcase */
        return {
            test_suite: this.options.collection.name,
            test_case: test.name,
            status: this.currItem.passed ? "PASSED" : "FAILED",
            start_time: epoch() - this.currItem.response.responseTime,
            duration: (this.currItem.response && this.currItem.response.responseTime) / 1000.0 || 0,
            error_msg: this.currItem.failedAssertions.join(", ") || null,
            // error_trace: err.stack || null,
            extras: {
                // file: test.file || null
                // TODO: put response bytes here
                // TODO: request info, headers/cookies etc
            }
        };
        /*eslint-enable camelcase */

        if (!this.currItem.passed) {
            const responseCode = (this.currItem.response && this.currItem.response.responseTime) || "-";
            const reason = (this.currItem.response && this.currItem.response.reason()) || "-";
            const details = (`Response code: ${responseCode}, reason: ${reason}`);
            console.log(`[testFailed name='${this.currItem.name}' message='${msg}' details='${msg} - ${details}']`);
        }

        const a = {
            name: 'A simple POST request with JSON body',
            passed: true,
            failedAssertions: [],
            response:
                {
                    id: 'dabec10b-6046-41ef-8740-9c53e7bb4592',
                    _details:
                        {
                            name: 'OK',
                            detail: 'Standard response for successful HTTP requests. The actual response will depend on the request method used. In a GET request, the response will contain an entity corresponding to the requested resource. In a POST request the response will contain an entity describing or containing the result of the action.',
                            code: 200,
                            standardName: 'OK'
                        },
                    status: 'OK',
                    code: 200,
                    headers:
                        {
                            members: [Object],
                            reference: [Object],
                            Type: [Object],
                            _postman_listIndexKey: 'key',
                            _postman_listIndexCaseInsensitive: true,
                            _postman_listAllowsMultipleValues: true
                        },
                    stream: "",
                    cookies:
                        {
                            members: [],
                            reference: {},
                            Type: [Object],
                            _postman_listIndexKey: 'name',
                            _postman_listIndexCaseInsensitive: true,
                            _postman_listAllowsMultipleValues: true
                        },
                    responseTime: 171,
                    responseSize: 586
                }
        }
    }
}

module.exports = TaurusReporter;