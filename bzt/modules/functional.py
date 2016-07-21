"""
Copyright 2016 BlazeMeter Inc.

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
import os
import time
from datetime import datetime

from bzt.engine import Reporter
from bzt.modules.aggregator import ResultsProvider, ResultsReader, DataPoint, AggregatorListener, KPISet


class LDJSONReader(object):
    def __init__(self, filename, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        self.partial_buffer = ""

    def read(self, last_pass=False):
        if not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            return

        if last_pass:
            lines = self.fds.readlines()  # unlimited
        else:
            lines = self.fds.readlines(1024 * 1024)

        for line in lines:
            if not line.endswith("\n"):
                self.partial_buffer += line
                continue
            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""
            yield json.loads(line)

    def __open_fds(self):
        if not os.path.isfile(self.filename):
            return False
        fsize = os.path.getsize(self.filename)
        if not fsize:
            return False
        self.fds = open(self.filename)
        return True

    def __del__(self):
        if self.fds is not None:
            self.fds.close()


class ReportReader(ResultsReader):
    REPORT_ITEM_KEYS = ["label", "status", "description", "start_time", "duration", "error_msg", "error_trace"]
    TEST_STATUSES = ("PASSED", "FAILED", "BROKEN", "SKIPPED")
    FAILING_TESTS_STATUSES = ("FAILED", "BROKEN")

    def __init__(self, filename, parent_logger):
        super(ReportReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.json_reader = LDJSONReader(filename, self.log)
        self.read_records = 0

    def _read(self, last_pass=False):
        for row in self.json_reader.read(last_pass):
            self.read_records += 1
            yield row


class FunctionalStatsReporter(Reporter, AggregatorListener):
    def __init__(self):
        super(FunctionalStatsReporter, self).__init__()
        self.last_sec = None

    def prepare(self):
        super(FunctionalStatsReporter, self).prepare()
        if isinstance(self.engine.aggregator, ResultsProvider):
            self.engine.aggregator.add_listener(self)

    def aggregated_second(self, data):
        self.last_sec = data

    def post_process(self):
        super(FunctionalStatsReporter, self).post_process()
        if self.last_sec:
            cumulative = self.last_sec[DataPoint.CUMULATIVE][""]
            self.__report_count(cumulative)
            if self.settings.get('print-stack-trace', False):
                self.__report_failed_tests(cumulative)

    def __report_count(self, cumulative_kpi):
        self.log.info("Test count: %s", cumulative_kpi[KPISet.TEST_COUNT])
        for status in ReportReader.TEST_STATUSES:
            self.log.info("%s: %s test(s)", status, cumulative_kpi[KPISet.TEST_STATUSES][status])

    def __report_failed_tests(self, cumulative_kpi):
        for test in cumulative_kpi[KPISet.TESTS]:
            if test['status'] in ReportReader.FAILING_TESTS_STATUSES:
                self.log.info("Test %s is %s: %s", test['label'], test['status'], test['error_msg'])
                self.log.info("Stack trace: %s" % test['error_trace'])
