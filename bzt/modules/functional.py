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

from bzt.engine import Aggregator
from bzt.modules.aggregator import ResultsReader
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


class FunctionalSample(object):
    def __init__(
            self, test_case, test_suite, status,
            start_time, duration, error_msg, error_trace,
            extras=None, subsamples=None, path=None,
    ):
        # test_case: str - name of test case (method)
        # test_suite: str - name of test suite (class)
        # status: str - test status (PASSED / FAILED / BROKEN / SKIPPED)
        # start_time: float - epoch
        # duration: float - test duration (in seconds)
        # error_msg: str - one-line error message
        # error_trace: str - error stacktrace
        # extras: dict - additional test info (description, file, full_name)
        # subsamples: list - list of subsamples
        # path: list - list of path components: [{"type": str, "value": str}]
        self.test_case = test_case
        self.test_suite = test_suite
        self.status = status
        self.start_time = start_time
        self.duration = duration
        self.error_msg = error_msg
        self.error_trace = error_trace
        self.extras = extras or {}
        self.subsamples = subsamples or []
        self.path = path or []

    def get_fqn(self):
        if self.path:
            return '.'.join(comp["value"] for comp in self.path)
        else:
            return self.test_suite + '.' + self.test_case

    def get_short_name(self):
        if self.path:
            return '.'.join(comp["value"] for comp in self.path[-2:])
        else:
            return self.test_suite + '.' + self.test_case

    def get_type(self):
        if self.path:
            return self.path[-1]["type"]
        return None


class ResultsTree(BetterDict):
    def __init__(self):
        super(ResultsTree, self).__init__()

    def add_sample(self, sample):
        """
        :type sample: FunctionalSample
        """
        test_suite = sample.test_suite
        self.get(test_suite, [], force_set=True).append(sample)

    def test_suites(self):
        return [key for key, _ in iteritems(self)]

    def test_cases(self, suite_name):
        return self.get(suite_name, [])


class FunctionalResultsReader(object):
    @abstractmethod
    def read(self, last_pass=False):
        """Yields functional samples"""
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
    SAMPLE_KEYS = [
        "test_case",  # str
        "test_suite",  # str
        "status",  # str
        "start_time",  # float, epoch
        "duration",  # float, in seconds
        "error_msg",  # short string
        "error_trace",  # multiline string
        "extras",  # dict
        "subsamples",  # list of samples
        "assertions",  # list of dicts, {"name": str, "failed": bool, "error_msg": str, "error_trace": str}
        "path"  # list of components, [{"value": "test_Something", "type": "module"},
        #                      {"value": "TestAPI", "type": "class"},
        #                      {"value": "test_heartbeat", "type": "method"}
        #                      {"value": "index page": "type": "transaction"}
        #                      {"value": "http://blazedemo.com/": "type": "request"}
    ]
    TEST_STATUSES = ("PASSED", "FAILED", "BROKEN", "SKIPPED")
    FAILING_TESTS_STATUSES = ("FAILED", "BROKEN")

    def __init__(self, filename, parent_logger):
        super(TestReportReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.json_reader = LDJSONReader(filename, self.log)

    @staticmethod
    def process_label(label):
        if isinstance(label, str):
            parts = label.split('_', 2)  # 'test_01_feeling_good'
            if len(parts) == 3 and parts[0] == 'test' and parts[1].isdigit():
                return parts[2]
        return label

    def process_path(self, path):
        if isinstance(path, dict):
            test_suite = ".".join(part["value"] for part in path[:-1])
            test_case = path[-1]["value"]
            return test_suite, test_case
        return None

    def read(self, last_pass=False):
        for row in self.json_reader.read(last_pass):
            if "path" in row:
                processed_path = self.process_path(row["path"])
                if processed_path is not None:
                    row["test_suite"], row["test_case"] = processed_path
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
        if item["status"] == 'SKIPPED':
            return  # we ignore skipped samples to not skew results

        tstmp = int(item["start_time"])
        label = item["test_case"]
        concur = 1
        rtm = item["duration"]
        cnn = 0
        ltc = 0
        rcd = self.STATUS_TO_CODE.get(item["status"], "UNKNOWN")
        error = item["error_msg"] if item["status"] in TestReportReader.FAILING_TESTS_STATUSES else None
        trname = item.get("workerID", "")
        byte_count = None
        return tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname, byte_count

    def _read(self, last_pass=False):
        for row in self.report_reader.read(last_pass):
            self.read_records += 1
            sample = self.extract_sample(row)
            if sample:
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

    def _samples_from_row(self, row):
        result = []
        subsamples = [sample for item in row.get("subsamples", []) for sample in self._samples_from_row(item)]
        if any(subsample.get_type() == 'transaction' for subsample in subsamples):
            result.extend([sub for sub in subsamples if sub.get_type() == 'transaction'])
        else:
            sample = FunctionalSample(test_case=row["test_case"], test_suite=row["test_suite"],
                                      status=row["status"], start_time=row["start_time"], duration=row["duration"],
                                      error_msg=row["error_msg"], error_trace=row["error_trace"],
                                      extras=row.get("extras", {}), subsamples=subsamples, path=row.get("path", []))
            result.append(sample)
        return result

    def read(self, last_pass=False):
        for row in self.report_reader.read(last_pass):
            self.read_records += 1
            samples = self._samples_from_row(row)
            for sample in samples:
                self._write_sample_data_to_artifacts(sample.extras)
                yield sample
