import time
from os.path import join

from bzt import ToolError, TaurusConfigError
from bzt.utils import EXE_SUFFIX
from bzt.modules.siege import SiegeExecutor, DataLogReader
from bzt.modules.aggregator import ConsolidatingAggregator

from tests import BZTestCase, ExecutorTestCase, RESOURCES_DIR, close_reader_file, ROOT_LOGGER

TOOL_NAME = 'siege' + EXE_SUFFIX
TOOL_PATH = join(RESOURCES_DIR, "siege", TOOL_NAME)


class TestSiegeExecutor(ExecutorTestCase):
    EXECUTOR = SiegeExecutor

    def setUp(self):
        super(TestSiegeExecutor, self).setUp()
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.engine.aggregator.engine = self.obj.engine
        self.obj.settings.merge({"path": TOOL_PATH})

    def tearDown(self):
        close_reader_file(self.obj.reader)
        super(TestSiegeExecutor, self).tearDown()

    def test_iter(self):
        self.configure({"execution": {
            "concurrency": 2,
            "iterations": 3,
            "scenario": {
                "think-time": "1s",
                "requests": [
                    "http://blazedemo.com",
                    "http://ya.ru"]}}})
        self.obj.prepare()
        self.obj.get_widget()
        self.obj.startup()

    def test_hold(self):
        self.configure({"execution": {
            "concurrency": 2,
            "hold-for": '2s',
            "scenario": {
                "headers": {
                    'h1': 'value1',
                    'h2': 'value2'},
                "variables": {
                    'v1': 1,
                    'v2': 'TWO'},
                "script": join(RESOURCES_DIR, "siege", "url-file")}}})
        self.obj.prepare()
        self.assertNotEqual(len(self.obj.resource_files()), 0)
        self.obj.get_widget()
        self.obj.startup()

    def test_url_exceptions(self):
        self.obj.execution.merge({
            "concurrency": 2,
            "hold-for": '2s',
            "scenario": {}})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_check_install_exceptions(self):
        self.obj.settings.merge({"path": '*'})
        self.obj.execution.merge({
            "concurrency": 2,
            "hold-for": '2s',
            "scenario": {}})
        self.assertRaises(ToolError, self.obj.prepare)

    def test_repetition_exceptions(self):
        self.configure({"execution": {
            "concurrency": 2,
            "ramp-up": "1h",
            "scenario": {
                "requests": [
                    "http://blazedemo.com",
                    "http://ya.ru"]}}})
        self.obj.prepare()
        self.assertEqual(len(self.obj.resource_files()), 0)
        self.assertRaises(TaurusConfigError, self.obj.startup)

    def test_full_execution(self):
        self.configure({"execution": {
            "concurrency": 2,
            "iterations": 3,
            "rc-file": join(RESOURCES_DIR, "siege", "siegerc"),
            "scenario": {
                "requests": [
                    "http://blazedemo.com",
                    "http://ya.ru"]}}})
        self.obj.prepare()
        try:
            self.obj.startup()
            self.obj.engine.aggregator.check()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()

        self.obj.post_process()
        self.assertNotEquals(self.obj.process, None)

    def test_diagnostics(self):
        self.obj.execution.merge({
            "iterations": 1,
            "scenario": {
                "requests": [
                    "http://blazedemo.com"]}})
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertIsNotNone(self.obj.get_error_diagnostics())


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = join(RESOURCES_DIR, "siege", "siege.out")
        obj = DataLogReader(log_path, ROOT_LOGGER)
        list_of_values = list(obj.datapoints(True))

        self.assertEqual(len(list_of_values), 8)

        for values in list_of_values:
            self.assertTrue(1400000000 < values['ts'] < 1500000000)
            self.assertEqual(len(values), 5)
