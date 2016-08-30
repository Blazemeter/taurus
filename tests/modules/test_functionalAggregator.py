from bzt.modules.functional import FunctionalAggregator, FunctionalAggregatorListener

from tests import BZTestCase
from tests.mocks import MockFunctionalReader


class MockListener(FunctionalAggregatorListener):
    def __init__(self):
        self.results = []

    def aggregated_results(self, result, cumulative_results):
        self.results.append(result)


class TestFunctionalAggregator(BZTestCase):
    def get_reader(self, offset=0):
        mock = MockFunctionalReader()
        mock.data = [
            {"start_time": 1 + offset, "label": "test1", "full_name": "Tests1.test1", "status": "PASSED"},
            {"start_time": 2 + offset, "label": "test2", "full_name": "Tests1.test2", "status": "FAILED"},
            {"start_time": 2 + offset, "label": "test3", "full_name": "Tests2.test3", "status": "BROKEN"},
            {"start_time": 3 + offset, "label": "test1", "full_name": "Tests1.test1", "status": "PASSED"},
            {"start_time": 3 + offset, "label": "test3", "full_name": "Tests2.test3", "status": "SKIPPED"},
            {"start_time": 4 + offset, "label": "test2", "full_name": "Tests1.test2", "status": "PASSED"},
            {"start_time": 4 + offset, "label": "test1", "full_name": "Tests1.test1", "status": "BROKEN"},
            {"start_time": 6 + offset, "label": "test1", "full_name": "Tests1.test1", "status": "SKIPPED"},
            {"start_time": 6 + offset, "label": "test3", "full_name": "Tests2.test3", "status": "FAILED"},
            {"start_time": 6 + offset, "label": "test2", "full_name": "Tests1.test2", "status": "PASSED"},
            {"start_time": 5 + offset, "label": "test1", "full_name": "Tests1.test1", "status": "BROKEN"},
        ]
        return mock

    def test_aggregation(self):
        reader = self.get_reader()
        obj = FunctionalAggregator()
        obj.prepare()
        obj.add_underling(reader)
        obj.process_readers()
        tree = obj.cumulative_results
        self.assertEqual({"Tests2", "Tests1"}, set(tree.test_suites()))
        self.assertEqual(len(tree.test_cases("Tests1")), 8)
        self.assertEqual(len(tree.test_cases("Tests2")), 3)
        obj.post_process()

    def test_listeners(self):
        listener = MockListener()
        obj = FunctionalAggregator()
        obj.prepare()
        obj.add_underling(self.get_reader())
        obj.add_listener(listener)
        obj.check()
        obj.post_process()
        self.assertEqual(len(listener.results), 1)
