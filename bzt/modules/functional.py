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
from abc import abstractmethod
from collections import namedtuple

from bzt.engine import Aggregator
from bzt.modules.aggregator import ResultsReader
from bzt.six import string_types
from bzt.utils import BetterDict, iteritems, LDJSONReader


class FunctionalAggregator(Aggregator):
    """
    :type listeners: list[FunctionalAggregatorListener]
    :type underlings: list[FunctionalResultsReader]
    :type cumulative_results: ResultsTree
    """

    def __init__(self):
        super(FunctionalAggregator, self).__init__(is_functional=True)
        self.underlings = []
        self.listeners = []
        self.cumulative_results = ResultsTree()

    def add_underling(self, reader):
        assert isinstance(reader, FunctionalResultsReader)
        self.underlings.append(reader)

    def add_listener(self, listener):
        assert isinstance(listener, FunctionalAggregatorListener)
        self.listeners.append(listener)

    def prepare(self):
        # TODO: setup, read settings
        pass

    def process_readers(self, last_pass=False):
        new_results = ResultsTree()

        for reader in self.underlings:
            for sample in reader.read(last_pass):
                new_results.add_sample(sample)

        if new_results:
            self.cumulative_results.merge(new_results)
            for listener in self.listeners:
                listener.aggregated_results(new_results, self.cumulative_results)

    def check(self):
        self.process_readers()
        return False

    def post_process(self):
        self.process_readers(last_pass=True)


FunctionalSample = namedtuple('Sample',
                              'test_case,test_suite,status,start_time,duration,error_msg,error_trace,extras,subsamples')
# test_case: str - name of test case (method)
# test_suite: str - name of test suite (class)
# status: str - test status (PASSED / FAILED / BROKEN / SKIPPED)
# start_time: float - epoch
# duration: float - test duration (in seconds)
# error_msg: str - one-line error message
# error_trace: str - error stacktrace
# extras: dict - additional test info (description, file, full_name)
# subsamples: list - list of subsamples


class ResultsTree(BetterDict):
    def __init__(self):
        super(ResultsTree, self).__init__()

    def add_sample(self, sample):
        """
        :type sample: FunctionalSample
        """
        test_suite = sample.test_suite
        self.get(test_suite, []).append(sample)

    def test_suites(self):
        return [key for key, _ in iteritems(self)]

    def test_cases(self, suite_name):
        return self.get(suite_name, [])


class FunctionalResultsReader(object):
    @abstractmethod
    def read(self, last_pass=False):
        "Yields functional samples"
        yield


class FunctionalAggregatorListener(object):
    @abstractmethod
    def aggregated_results(self, results, cumulative_results):
        """
        Callback that gets called every time aggregator processes new test results.
        :type results: ResultsTree
        :type cumulative_results: ResultsTree
        """
        pass


class TestReportReader(object):
    REPORT_ITEM_KEYS = ["test_case", "test_suite", "status", "start_time", "duration",
                        "error_msg", "error_trace", "extras", "subsamples"]
    TEST_STATUSES = ("PASSED", "FAILED", "BROKEN", "SKIPPED")
    FAILING_TESTS_STATUSES = ("FAILED", "BROKEN")

    def __init__(self, filename, parent_logger):
        super(TestReportReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.json_reader = LDJSONReader(filename, self.log)

    def process_label(self, label):
        if isinstance(label, string_types):
            if label.startswith('test_') and label[5:10].isdigit():
                return label[11:]

        return label

    def read(self, last_pass=False):
        for row in self.json_reader.read(last_pass):
            row["test_case"] = self.process_label(row["test_case"])
            yield row


class LoadSamplesReader(ResultsReader):
    STATUS_TO_CODE = {
        "PASSED": "200",
        "SKIPPED": "300",
        "FAILED": "400",
        "BROKEN": "500",
    }

    def __init__(self, filename, parent_logger):
        super(LoadSamplesReader, self).__init__()
        self.report_reader = TestReportReader(filename, parent_logger)
        self.read_records = 0

    def extract_sample(self, item):
        tstmp = int(item["start_time"])
        label = item["test_case"]
        concur = 1
        rtm = item["duration"]
        cnn = 0
        ltc = 0
        rcd = self.STATUS_TO_CODE.get(item["status"], "UNKNOWN")
        error = item["error_msg"] if item["status"] in TestReportReader.FAILING_TESTS_STATUSES else None
        trname = ""
        byte_count = None
        return tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname, byte_count

    def _read(self, last_pass=False):
        for row in self.report_reader.read(last_pass):
            self.read_records += 1
            sample = self.extract_sample(row)
            yield sample


class FuncSamplesReader(FunctionalResultsReader):
    FIELDS_EXTRACTED_TO_ARTIFACTS = ["requestBody", "responseBody", "requestCookiesRaw"]

    def __init__(self, filename, engine, parent_logger):
        self.report_reader = TestReportReader(filename, parent_logger)
        self.engine = engine
        self.read_records = 0

    def _write_sample_data_to_artifacts(self, sample_extras):
        if not sample_extras:
            return
        for file_field in self.FIELDS_EXTRACTED_TO_ARTIFACTS:
            if file_field not in sample_extras:
                continue
            contents = sample_extras.pop(file_field)
            if contents:
                filename = "sample-%s" % file_field
                artifact = self.engine.create_artifact(filename, ".bin")
                with open(artifact, 'wb') as fds:
                    fds.write(contents.encode('utf-8'))
                sample_extras[file_field] = artifact

    def _sample_from_row(self, row):
        subsamples = [self._sample_from_row(item) for item in row.get("subsamples", [])]
        return FunctionalSample(test_case=row["test_case"], test_suite=row["test_suite"],
                                status=row["status"], start_time=row["start_time"], duration=row["duration"],
                                error_msg=row["error_msg"], error_trace=row["error_trace"],
                                extras=row.get("extras", {}), subsamples=subsamples)

    def read(self, last_pass=False):
        for row in self.report_reader.read(last_pass):
            self.read_records += 1
            sample = self._sample_from_row(row)
            self._write_sample_data_to_artifacts(sample.extras)
            yield sample
