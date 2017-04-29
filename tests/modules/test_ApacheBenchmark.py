import logging
import time
from os import path

from bzt.modules.ab import ApacheBenchmarkExecutor, TSVDataReader
from tests import BZTestCase
from tests.mocks import EngineEmul
from bzt.utils import EXE_SUFFIX
from bzt import ToolError, TaurusConfigError

TOOL_NAME = 'ab' + EXE_SUFFIX


def get_res_path(resource):
    return path.join(path.dirname(__file__), '..', 'resource', 'ab', resource)


class TestApacheBenchExecutor(BZTestCase):
    def test_iter(self):
        "Ensures that executor doesn't fail with minimal configuration."
        obj = ApacheBenchmarkExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "scenario": {
                "requests": ["http://blazedemo.com"]
            }
        })
        obj.prepare()
        obj.get_widget()
        try:
            obj.startup()
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()
        obj.post_process()
        self.assertNotEquals(obj.process, None)

    def test_no_request_exception(self):
        "Checks that executor.startup fails if there's no request specified."
        obj = ApacheBenchmarkExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "scenario": {}})
        obj.prepare()
        self.assertRaises(TaurusConfigError, obj.startup)

    def test_non_get_request_exception(self):
        """
        Checks that executor.startup fails if
        request with non-GET method is specified.
        """
        obj = ApacheBenchmarkExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "scenario": {
                "requests": [
                    {
                        "url": "http://blazedemo.com",
                        "method": "POST",
                    }
                ]
            }})
        obj.prepare()
        self.assertRaises(TaurusConfigError, obj.startup)

    def test_no_apache_benchmark(self):
        "Checks that prepare() fails if ApacheBenchmark is not installed."
        obj = ApacheBenchmarkExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": '*',})
        obj.execution.merge({
            "scenario": {
                "requests": ["http://blazedemo.com"]
            }})
        self.assertRaises(ToolError, obj.prepare)

    def test_full_execution(self):
        obj = ApacheBenchmarkExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "concurrency": 2,
            "iterations": 3,
            "headers": {
                "Content-Type": "text/plain"
            },
            "scenario": {
                "keepalive": True,
                "requests": [
                    {
                        "url": "http://blazedemo.com",
                        "headers": [
                            {"X-Answer": "42"},
                        ],
                        "keepalive": False,
                        "method": "GET",
                    }
                ],
            }
        })
        obj.prepare()
        obj.get_widget()
        try:
            obj.startup()
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()
        obj.post_process()
        self.assertNotEquals(obj.process, None)


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = path.join(get_res_path('ab.tsv'))
        obj = TSVDataReader(log_path, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))

        self.assertEqual(len(list_of_values), 3)

        for values in list_of_values:
            self.assertTrue(1400000000 < values['ts'] < 1500000000)
            self.assertEqual(len(values), 5)
