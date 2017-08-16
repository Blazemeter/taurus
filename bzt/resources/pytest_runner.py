"""
Copyright 2017 BlazeMeter Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""
import json
import sys
import time
import traceback
from optparse import OptionParser

import pytest

from bzt.modules.python import Sample


class RecordingPlugin(object):
    def __init__(self, report_path):
        self._report_path = report_path
        self._report_fds = None
        self.test_count = 0
        self.failed_tests = 0
        self.passed_tests = 0
        self._sample = None

    def prepare(self):
        if self._report_fds is None:
            self._report_fds = open(self._report_path, 'w')

    def post_process(self):
        if self._report_fds is not None:
            self._report_fds.close()
            self._report_fds = None

    def _write_sample(self, sample):
        if self._report_fds is None:
            raise ValueError("Plugin wasn't prepared")

        self._report_fds.write("%s\n" % json.dumps(sample.to_dict()))
        self._report_fds.flush()

    def _write_stdout_report(self, label):
        report_pattern = "%s,Total:%d Passed:%d Failed:%d\n"
        sys.stdout.write(report_pattern % (label, self.test_count, self.passed_tests, self.failed_tests))
        sys.stdout.flush()

    def _fill_sample(self, report, call, item, status):
        filename, lineno, _ = report.location

        if self._sample is None:
            self._sample = Sample()
            self._sample.test_case = item.name
            self._sample.test_suite = item.module.__name__
            self._sample.start_time = call.start
            self._sample.extras = {"filename": filename, "lineno": lineno}

        self._sample.status = status
        self._sample.duration = time.time() - self._sample.start_time

        if call.excinfo is not None:
            self._sample.error_msg = call.excinfo.exconly().strip()
            self._sample.error_trace = "\n".join(traceback.format_tb(call.excinfo.tb)).strip()

    def _report_sample(self, label):
        self.test_count += 1
        if self._sample.status == "PASSED":
            self.passed_tests += 1
        elif self._sample.status in ["BROKEN", "FAILED"]:
            self.failed_tests += 1

        self._write_sample(self._sample)
        self._write_stdout_report(label)
        self._sample = None

    @pytest.mark.hookwrapper
    def pytest_runtest_makereport(self, item, call):
        outcome = (yield)
        report = outcome.get_result()
        print("runtest makereport: %s %s %s" % (item, call.excinfo, report))
        print("report: skipped=%s failed=%s passed=%s" % (report.skipped, report.failed, report.passed))
        filename, lineno, test_name = report.location
        if report.when == 'call':
            if report.passed:
                self._fill_sample(report, call, item, "PASSED")
            elif report.failed:
                self._fill_sample(report, call, item, "FAILED")
            elif report.skipped:
                self._fill_sample(report, call, item, "SKIPPED")
        elif report.when == 'setup':
            if report.failed:
                self._fill_sample(report, call, item, "BROKEN")
            elif report.skipped:
                self._fill_sample(report, call, item, "SKIPPED")
        elif report.when == 'teardown':
            if not report.passed:
                if self._sample.status not in ["FAILED", "BROKEN"]:
                    self._fill_sample(report, call, item, "BROKEN")
                else:
                    self._sample.status = "BROKEN"
            self._report_sample(test_name)

    def pytest_runtest_logreport(self, report):
        print(report)


def run_pytest(targets, report_path, iteration_limit, duration_limit):
    plugin = RecordingPlugin(report_path)
    plugin.prepare()
    start_time = int(time.time())
    iteration = 0
    try:
        while True:
            pytest.main(['-s'] + targets, plugins=[plugin])
            iteration += 1
            if 0 < duration_limit < int(time.time()) - start_time:
                break
            if iteration >= iteration_limit:
                break
    finally:
        plugin.post_process()
        if plugin.test_count == 0:
            raise ValueError("Nothing to test. No tests were found.")

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option('-r', '--report-file', action='store', default='report.ldjson')
    parser.add_option('-i', '--iterations', action='store', default=0)
    parser.add_option('-d', '--duration', action='store', default=0)
    opts, args = parser.parse_args()

    opts.iterations = int(opts.iterations)
    opts.duration = float(opts.duration)

    if opts.iterations == 0:
        if opts.duration > 0:
            opts.iterations = sys.maxsize
        else:
            opts.iterations = 1

    run_pytest(args, opts.report_file, int(opts.iterations), float(opts.duration))

