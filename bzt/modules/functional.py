# coding=utf-8
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
import logging
import os
import time

from urwid import WEIGHT, CENTER, CLIP, GIVEN, RIGHT, Pile, Filler, Divider, ListBox, SimpleListWalker, LineBox
from urwid import Columns, Text, Padding

from bzt.engine import Reporter
from bzt.modules.aggregator import ResultsProvider, ResultsReader, DataPoint, AggregatorListener, KPISet
from bzt.modules.console import ConsoleStatusReporter, TaurusConsole
from bzt.modules.console import ScrollingLog, LatestStats, CumulativeStats, ThreeGraphs, TaurusLogo, StatsColumn
from bzt.utils import humanize_time


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
        self.fds = open(self.filename, 'rt', buffering=1)
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


class FunctionalConsoleReporter(ConsoleStatusReporter):
    def __init__(self):
        super(FunctionalConsoleReporter, self).__init__()

    def _create_console(self, sidebar_widgets):
        return FunctionalConsole(sidebar_widgets)


class FunctionalConsole(TaurusConsole):
    palette = TaurusConsole.palette  + [
        ('test-hdr', 'light gray', 'dark blue'),
        ('test-txt', '', ''),
        ('stat-passed', 'light green', ''),
        ('stat-failed', 'dark red', ''),
        ('stat-broken', 'brown', ''),
        ('stat-skipped', 'light gray', ''),
    ]

    def __init__(self, sidebar_widgets):
        self.log_widget = ScrollingLog()
        self.cumulative_stats = CumulativeTestStats()
        self.logo = TaurusLogo()

        stats_pane = Pile([(WEIGHT, 1.0, self.cumulative_stats)])
        ordered_widgets = sorted(sidebar_widgets, key=lambda x: x.priority)
        right_widgets = ListBox(SimpleListWalker([Pile([x, Divider()]) for x in ordered_widgets]))
        widget_pile = Pile([(7, self.logo), right_widgets])

        log_block = Pile([(1, Filler(Divider('─'))), self.log_widget])

        right_pane = Pile([(WEIGHT, 0.667, widget_pile),
                           (WEIGHT, 0.333, log_block)])

        columns = [(WEIGHT, 0.25, Pile([])),
                   (WEIGHT, 0.50, stats_pane),
                   (WEIGHT, 0.25, right_pane)]
        super(TaurusConsole, self).__init__(columns)

    def add_data(self, data):
        """
        :type data: bzt.modules.aggregator.DataPoint
        """
        self.cumulative_stats.add_data(data)


class CumulativeTestStats(LineBox):
    def __init__(self):
        self.data = DataPoint(0)
        self._start_time = None
        self.statuses = StatusesTable()
        self.failed_tests_pile = FailedTestsPile()
        original_widget = Pile([
            (WEIGHT, 0.2, Columns([self.statuses])),
            (WEIGHT, 0.8, self.failed_tests_pile),
        ])
        padded = Padding(original_widget, align=CENTER)
        super(CumulativeTestStats, self).__init__(padded, " Test Stats ")

    def add_data(self, data):
        """
        Append datapoint

        :type data: bzt.modules.aggregator.DataPoint
        """
        self.data = data
        self.statuses.add_data(data)
        self.failed_tests_pile.add_data(data)

        if not self._start_time:
            self._start_time = data.get('ts')
        duration = humanize_time(time.time() - self._start_time)

        self.title_widget.set_text(" Test Stats after %s " % duration)


class StatusesTable(ListBox):
    def __init__(self):
        super(StatusesTable, self).__init__(SimpleListWalker([]))

    def add_data(self, data):
        """
        Append data

        :type data: bzt.modules.aggregator.DataPoint
        """
        while len(self.body):
            self.body.pop(0)

        self.body.append(Text(("stat-hdr", " Statuses: "), align=RIGHT))
        overall = data.get(DataPoint.CUMULATIVE).get('', KPISet())
        for key in ReportReader.TEST_STATUSES:
            color = "stat-" + key.lower()
            dat = (key, overall[KPISet.TEST_STATUSES][key])
            self.body.append(Text((color, "%s: %d" % dat), align=RIGHT))
        self.body.append(Text(('stat-txt', "Overall: %d" % overall[KPISet.TEST_COUNT]), align=RIGHT))


class FailedTestsPile(Pile):
    def __init__(self):
        self.label_columns = LabelStatsTable()
        self.rows = [self.label_columns]
        super(FailedTestsPile, self).__init__(self.rows)

    def add_data(self, data):
        self.label_columns.add_data(data)

    def render(self, size, focus=False):
        labels_height = self.label_columns.get_height() + 1
        self.contents[0] = (self.contents[0][0], (GIVEN, labels_height))
        return super(FailedTestsPile, self).render(size)


class LabelStatsTable(Columns):
    def __init__(self):
        self.labels = SampleLabelsNames()
        self.stats_table = StatsTable()
        self.columns = [self.labels, self.stats_table]

        super(LabelStatsTable, self).__init__(self.columns, dividechars=1)

    def add_data(self, data):
        """
        Append data

        :type data: bzt.modules.aggregator.DataPoint
        """
        self.labels.flush_data()
        self.stats_table.flush_data()

        overall = data.get(DataPoint.CUMULATIVE)

        for label in overall.keys():
            if label != "":
                failed_tests = overall.get(label).get(KPISet.TEST_FAILED)
                for test in failed_tests:
                    self.labels.add_data(test['label'])
                    self.stats_table.add_data(test['status'])

    def render(self, size, focus=False):
        max_width = size[0]
        stat_table_max_width = self.stats_table.get_width()
        label_names_width = self.labels.get_width()
        if stat_table_max_width + label_names_width <= max_width:
            self.contents[0] = (self.contents[0][0], (GIVEN, label_names_width, False))
        else:
            self.contents[0] = (self.contents[0][0], (GIVEN, max_width - stat_table_max_width, False))
        return super(LabelStatsTable, self).render(size)

    def get_height(self):
        return self.labels.get_height()


class StatsTable(Columns):
    def __init__(self):
        self.statuses = SampleLabelsStatuses()
        super(StatsTable, self).__init__([self.statuses], dividechars=1)

    def flush_data(self):
        self.statuses.flush_data()

    def add_data(self, statuses):
        self.statuses.add_data(statuses)

    def get_width(self):
        table_size = self.statuses.get_width()
        return table_size

    def render(self, size, focus=False):
        hits_size = self.statuses.get_width()
        self.contents[0] = (self.contents[0][0], (GIVEN, hits_size, False))
        return super(StatsTable, self).render(size)


class SampleLabelsNames(StatsColumn):
    def __init__(self):
        super(SampleLabelsNames, self).__init__(SimpleListWalker([]))
        self.header = Text(("test-hdr", " Failed tests "))
        self.body.append(self.header)

    def add_data(self, data):
        data_widget = Text(("test-txt", "%s" % data), wrap=CLIP)
        self.body.append(data_widget)


class SampleLabelsStatuses(StatsColumn):
    def __init__(self):
        super(SampleLabelsStatuses, self).__init__(SimpleListWalker([]))
        self.header = Text(("stat-hdr", " Status "), align=RIGHT)
        self.body.append(self.header)

    def add_data(self, data):
        data_widget = Text(("stat-txt", "%s" % data), align=RIGHT)
        self.body.append(data_widget)
