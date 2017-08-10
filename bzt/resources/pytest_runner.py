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
from optparse import OptionParser

import pytest

from bzt.modules.python import Sample


class RecordingPlugin(object):
    def __init__(self, report_path):
        self._report_path = report_path
        self._report_fds = None

    def prepare(self):
        if self._report_fds is None:
            print("opening file")
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

    def write_stdout_report(self, label):
        # report_pattern = "%s,Total:%d Passed:%d Failed:%d\n"
        # failed = self.test_count - self.success_count
        # sys.stdout.write(report_pattern % (label, self.test_count, self.success_count, failed))
        # sys.stdout.flush()
        pass

    def pytest_collection_modifyitems(self, config, items):
        # you can count the tests from len(items)
        print("items collected: %s" % items)
        pass

    def pytest_runtest_setup(self, item):
        # when test is set up, before the test is started
        print("runtest setup %s" % item)
        pass

    def pytest_runtest_teardown(self, item, nextitem):
        # after test is finished
        print("runtest teardown %s" % item)
        pass

    def pytest_runtest_makereport(self, item, call):
        print("runtest makereport: %s %s" % (item, call))
        if call.when == 'call':
            module_name = item.module.__name__
            duration = call.stop - call.start
            status = 'FAILED' if call.excinfo is not None else 'PASSED'
            sample = Sample(test_case=item.name,
                            test_suite=module_name,
                            start_time=call.start,
                            duration=duration,
                            status=status)
            self._write_sample(sample)


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

