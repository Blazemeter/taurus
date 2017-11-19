import logging
import time
from os.path import join

from bzt import ToolError, TaurusConfigError
from bzt.modules.siege import SiegeExecutor, DataLogReader
from bzt.utils import EXE_SUFFIX
from tests import BZTestCase, RESOURCES_DIR
from tests.mocks import EngineEmul
from bzt.modules.aggregator import ConsolidatingAggregator

TOOL_NAME = 'siege' + EXE_SUFFIX
TOOL_PATH = join(RESOURCES_DIR, "siege", TOOL_NAME)


class TestSiegeExecutor(BZTestCase):
    def setUp(self):
        super(TestSiegeExecutor, self).setUp()
        self.obj = SiegeExecutor()
        self.obj.engine = EngineEmul()
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.settings.merge({"path": TOOL_PATH})

    def tearDown(self):
        if self.obj.stdout_file:
            self.obj.stdout_file.close()
        if self.obj.stderr_file:
            self.obj.stderr_file.close()
        if self.obj.reader and self.obj.reader.fds:
            self.obj.reader.fds.close()
        super(TestSiegeExecutor, self).tearDown()

    def test_iter(self):
        self.obj.execution.merge({
            "concurrency": 2,
            "iterations": 3,
            "scenario": {
                "think-time": "1s",
                "requests": [
                    "http://blazedemo.com",
                    "http://ya.ru"]}})
        self.obj.prepare()
        self.obj.get_widget()
        self.obj.startup()

    def test_hold(self):
        self.obj.execution.merge({
            "concurrency": 2,
            "hold-for": '2s',
            "scenario": {
                "headers": {
                    'h1': 'value1',
                    'h2': 'value2'},
                "variables": {
                    'v1': 1,
                    'v2': 'TWO'},
                "script": join(RESOURCES_DIR, "siege", "url-file")}})
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
        self.obj.execution.merge({
            "concurrency": 2,
            "scenario": {
                "requests": [
                    "http://blazedemo.com",
                    "http://ya.ru"]}})
        self.obj.prepare()
        self.assertEqual(len(self.obj.resource_files()), 0)
        self.assertRaises(TaurusConfigError, self.obj.startup)

    def test_full_execution(self):
        self.obj.execution.merge({
            "concurrency": 2,
            "iterations": 3,
            "scenario": {
                "requests": [
                    "http://blazedemo.com",
                    "http://ya.ru"]}})
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
        obj = DataLogReader(log_path, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))

        self.assertEqual(len(list_of_values), 8)

        for values in list_of_values:
            self.assertTrue(1400000000 < values['ts'] < 1500000000)
            self.assertEqual(len(values), 5)
