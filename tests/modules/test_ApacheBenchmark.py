import time
import os

from bzt.modules.ab import ApacheBenchmarkExecutor, TSVDataReader
from tests import BZTestCase, ExecutorTestCase, RESOURCES_DIR, close_reader_file, ROOT_LOGGER
from bzt.utils import EXE_SUFFIX
from bzt import ToolError, TaurusConfigError


class TestApacheBenchExecutor(ExecutorTestCase):
    EXECUTOR = ApacheBenchmarkExecutor

    def setUp(self):
        super(TestApacheBenchExecutor, self).setUp()
        path = os.path.abspath(RESOURCES_DIR + "ab/ab" + EXE_SUFFIX)
        self.obj.settings.merge({"path": path})

    def tearDown(self):
        close_reader_file(self.obj.reader)
        super(TestApacheBenchExecutor, self).tearDown()

    def test_iter(self):
        "Ensures that executor doesn't fail with minimal configuration."
        self.configure({"execution": {
            "scenario": {
                "requests": ["http://blazedemo.com"]}}})
        self.obj.prepare()
        self.obj.get_widget()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertNotEquals(self.obj.process, None)

    def test_no_request_exception(self):
        "Checks that executor.startup fails if there's no request specified."
        self.configure({"execution": {"scenario": {}}})
        self.obj.prepare()
        self.assertRaises(TaurusConfigError, self.obj.startup)

    def test_non_get_request_exception(self):
        """
        Checks that executor.startup fails if
        request with non-GET method is specified.
        """
        self.obj.execution.merge({
            "scenario": {
                "requests": [{
                    "url": "http://blazedemo.com",
                    "method": "POST"}]}})
        self.obj.prepare()
        self.assertRaises(TaurusConfigError, self.obj.startup)

    def test_no_apache_benchmark(self):
        "Checks that prepare() fails if ApacheBenchmark is not installed."
        self.obj.settings.merge({"path": "*"})
        self.obj.execution.merge({
            "scenario": {
                "requests": ["http://blazedemo.com"]}})
        self.assertRaises(ToolError, self.obj.prepare)

    def test_full_execution(self):
        self.configure({"execution": {
            "concurrency": 2,
            "iterations": 3,
            "headers": {
                "Content-Type": "text/plain"
            },
            "scenario": {
                "keepalive": True,
                "requests": [{
                    "url": "http://blazedemo.com",
                    "headers": {"X-Answer": "42"},
                    "keepalive": False,
                    "method": "GET"}]}}})
        self.obj.prepare()
        self.obj.get_widget()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertNotEquals(self.obj.process, None)

    def test_diagnostics(self):
        self.configure({"execution": {
            "concurrency": 1,
            "iterations": 1,
            "scenario": {
                "requests": ["http://blazedemo.com"]}}})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.obj.post_process()
        self.assertIsNotNone(self.obj.get_error_diagnostics())


class TestApacheBenchmarkCmd(ExecutorTestCase):
    EXECUTOR = ApacheBenchmarkExecutor
    CMD_LINE = None

    def setUp(self):
        super(TestApacheBenchmarkCmd, self).setUp()
        path = os.path.abspath(RESOURCES_DIR + "ab/ab" + EXE_SUFFIX)
        self.obj.settings.merge({"path": path})

    def start_subprocess(self, args, env, cwd=None, **kwargs):
        self.CMD_LINE = args

    def test_timeout(self):
        self.configure({
            "execution": {
                "concurrency": 1,
                "iterations": 1,
                "scenario": "simple"},
            "scenarios": {
                "simple": {
                    "timeout": 5,
                    "requests": ["http://blazedemo.com"]
                }
            }})
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.assertTrue('-s' in self.CMD_LINE)
        delay_val = self.CMD_LINE[self.CMD_LINE.index('-s') + 1]
        self.assertEqual(float(delay_val), 5.0)


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = os.path.abspath(RESOURCES_DIR + "ab/ab.tsv")
        obj = TSVDataReader(log_path, ROOT_LOGGER)
        list_of_values = list(obj.datapoints(True))

        self.assertEqual(len(list_of_values), 3)

        for values in list_of_values:
            self.assertTrue(1400000000 < values['ts'] < 1500000000)
            self.assertEqual(len(values), 5)
