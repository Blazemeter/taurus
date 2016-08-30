from abc import abstractmethod
from pprint import pformat

from bzt.engine import EngineModule
from bzt.utils import BetterDict, iteritems


class FunctionalAggregator(EngineModule):
    def __init__(self):
        super(FunctionalAggregator, self).__init__()
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

    def process_samples(self, last_pass=False):
        new_results = ResultsTree()

        for reader in self.underlings:
            for sample in reader.read(last_pass):
                new_results.add_sample(sample)

        self.cumulative_results.merge(new_results)

        for listener in self.listeners:
            listener.new_results(new_results, self.cumulative_results)

    def check(self):
        self.process_samples()
        return False

    def post_process(self):
        self.process_samples(last_pass=True)

        suites = self.cumulative_results.test_suites()
        cases = [case for suite in suites for case in self.cumulative_results.test_cases(suite)]
        self.log.info("%d tests from %d test suites were executed", len(cases), len(suites))


class ResultsTree(BetterDict):
    def __init__(self):
        super(ResultsTree, self).__init__()

    def add_sample(self, sample):
        test_suite = sample["full_name"].split(".")[-2]
        self.get(test_suite, []).append(sample)

    def test_suites(self):
        return [key for key, _ in iteritems(self)]

    def test_cases(self, suite_name):
        return self.get(suite_name, [])


class FunctionalResultsReader(object):
    @abstractmethod
    def read(self, last_pass=False):
        "Yields functional samples"
        pass


class FunctionalAggregatorListener(object):
    @abstractmethod
    def new_results(self, results, cumulative_results):
        """
        Callback that gets called every time aggregator processes new test results.
        :type results: ResultsTree
        :type cumulative_results: ResultsTree
        """
        pass
