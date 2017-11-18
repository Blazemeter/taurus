import logging
import time
import os

from bzt.modules.ab import ApacheBenchmarkExecutor, TSVDataReader
from tests import BZTestCase, RESOURCES_DIR
from tests.mocks import EngineEmul
from bzt.utils import EXE_SUFFIX
from bzt import ToolError, TaurusConfigError


def get_ab():
    path = os.path.abspath(RESOURCES_DIR + "ab/ab" + EXE_SUFFIX)
    obj = ApacheBenchmarkExecutor()
    obj.engine = EngineEmul()
    obj.settings.merge({"path": path})
    return obj


class TestApacheBenchExecutor(BZTestCase):
    def setUp(self):
        self.obj = get_ab()
        super(TestApacheBenchExecutor, self).setUp()

    def tearDown(self):
        if self.obj.stdout_file:
            self.obj.stdout_file.close()
        if self.obj.stderr_file:
            self.obj.stderr_file.close()
        if self.obj.reader and self.obj.reader.fds:
            self.obj.reader.fds.close()
        super(TestApacheBenchExecutor, self).tearDown()

    def test_iter(self):
        "Ensures that executor doesn't fail with minimal configuration."
        self.obj.execution.merge({
            "scenario": {
                "requests": ["http://blazedemo.com"]}})
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
        self.obj.execution.merge({"scenario": {}})
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
        self.obj.execution.merge({
            "concurrency": 2,
            "iterations": 3,
            "headers": {
                "Content-Type": "text/plain"
            },
            "scenario": {
                "keepalive": True,
                "requests": [{
                    "url": "http://blazedemo.com",
                    "headers": [{"X-Answer": "42"}],
                    "keepalive": False,
                    "method": "GET"}]}})
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
        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 1,
            "scenario": {
                "requests": ["http://blazedemo.com"]}})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.obj.post_process()
        self.assertIsNotNone(self.obj.get_error_diagnostics())


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = os.path.abspath(RESOURCES_DIR + "ab/ab.tsv")
        obj = TSVDataReader(log_path, logging.getLogger(""))
        list_of_values = list(obj.datapoints(True))

        self.assertEqual(len(list_of_values), 3)

        for values in list_of_values:
            self.assertTrue(1400000000 < values['ts'] < 1500000000)
            self.assertEqual(len(values), 5)
