from abc import abstractmethod
from pprint import pformat

from bzt.engine import EngineModule
from bzt.utils import BetterDict, iteritems


class FunctionalAggregator(EngineModule):
    def __init__(self):
        super(FunctionalAggregator, self).__init__()
        self.underlings = []
        self.listeners = []
        self.results_tree = ResultsTree()

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
        new_samples = []
        for reader in self.underlings:
            for sample in reader.read(last_pass):
                new_samples.append(sample)

        new_tree = ResultsTree.from_samples(new_samples)
        self.results_tree.merge(new_tree)

        for listener in self.listeners:
            listener.new_results(new_tree, self.results_tree)

    def check(self):
        self.process_samples()
        return False

    def post_process(self):
        self.process_samples(last_pass=True)

        def to_dict(value):
            if isinstance(value, dict):
                return {
                    k: to_dict(v) for k, v in value.items()
                }
            elif isinstance(value, list):
                return [to_dict(v) for v in value]
            else:
                return value

        self.log.debug("Test results:\n" + pformat(to_dict(self.results_tree)))
        suites = self.results_tree.test_suites()
        cases = [case for suite in suites for case in self.results_tree.test_cases(suite)]
        self.log.info("%d tests were executed", len(cases))


class ResultsTree(BetterDict):
    def __init__(self):
        super(ResultsTree, self).__init__()

    def add_sample(self, sample):
        test_suite = sample["full_name"].split(".")[-2]
        self.get(test_suite, []).append(sample)

    @classmethod
    def from_samples(cls, samples):
        tree = cls()
        for sample in samples:
            tree.add_sample(sample)
        return tree

    def test_suites(self):
        return [key for key, value in iteritems(self)]

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
