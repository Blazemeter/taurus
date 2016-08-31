from abc import abstractmethod
from collections import namedtuple

from bzt.engine import Aggregator
from bzt.utils import BetterDict, iteritems


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


FunctionalSample = namedtuple('Sample', 'test_case,test_suite,status,start_time,duration,error_msg,error_trace,extras')
# test_case: str - name of test case (method)
# test_suite: str - name of test suite (class)
# status: str - test status (PASSED / FAILED / BROKEN / SKIPPED)
# start_time: float - epoch
# duration: float - test duration (in seconds)
# error_msg: str - one-line error message
# error_trace: str - error stacktrace
# extras: dict - additional test info (description, file, full_name)


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
