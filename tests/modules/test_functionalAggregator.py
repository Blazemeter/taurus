from tests import BZTestCase

from bzt.modules.functional import FunctionalAggregator, FunctionalSample
from tests.mocks import MockFunctionalReader, MockFunctionalListener


class TestFunctionalAggregator(BZTestCase):
    def get_reader(self):
        mock = MockFunctionalReader()
        mock.data = [
            FunctionalSample(test_case="test1", test_suite="Tests1", status="PASSED", start_time=1, duration=1,
                             error_msg=None, error_trace=None, extras=None, subsamples=[]),
            FunctionalSample(test_case="test2", test_suite="Tests1", status="BROKEN", start_time=2, duration=1,
                             error_msg="Something broke", error_trace=None, extras=None, subsamples=[]),
            FunctionalSample(test_case="test3", test_suite="Tests2", status="PASSED", start_time=2, duration=1,
                             error_msg=None, error_trace=None, extras=None, subsamples=[]),
            FunctionalSample(test_case="test2", test_suite="Tests1", status="FAILED", start_time=3, duration=1,
                             error_msg="Something failed", error_trace=None, extras=None, subsamples=[]),
            FunctionalSample(test_case="test1", test_suite="Tests1", status="SKIPPED", start_time=3, duration=1,
                             error_msg="Disabled by user", error_trace=None, extras=None, subsamples=[]),
            FunctionalSample(test_case="test3", test_suite="Tests2", status="PASSED", start_time=4, duration=1,
                             error_msg=None, error_trace=None, extras=None, subsamples=[]),
            FunctionalSample(test_case="test1", test_suite="Tests1", status="BROKEN", start_time=4, duration=1,
                             error_msg="Broken", error_trace=None, extras=None, subsamples=[]),
            FunctionalSample(test_case="test1", test_suite="Tests1", status="PASSED", start_time=5, duration=1,
                             error_msg=None, error_trace=None, extras=None, subsamples=[]),
            FunctionalSample(test_case="test2", test_suite="Tests1", status="PASSED", start_time=4, duration=1,
                             error_msg=None, error_trace=None, extras=None, subsamples=[]),
            FunctionalSample(test_case="test3", test_suite="Tests2", status="FAILED", start_time=6, duration=1,
                             error_msg="Really failed", error_trace=None, extras=None, subsamples=[]),
            FunctionalSample(test_case="test1", test_suite="Tests1", status="PASSED", start_time=6, duration=1,
                             error_msg=None, error_trace=None, extras=None, subsamples=[]),
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
        listener = MockFunctionalListener()
        obj = FunctionalAggregator()
        obj.prepare()
        obj.add_underling(self.get_reader())
        obj.add_listener(listener)
        obj.check()
        obj.post_process()
        self.assertEqual(len(listener.results), 1)
