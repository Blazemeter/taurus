import logging
import time
from os import path

from bzt.modules.apachebench import ApacheBenchExecutor, TSVDataReader
from tests import BZTestCase
from tests.mocks import EngineEmul
from bzt.utils import EXE_SUFFIX


TOOL_NAME = 'ab' + EXE_SUFFIX


def get_res_path(resource):
    return path.join(path.dirname(__file__), '..', 'apachebench', resource)


class TestApacheBenchExecutor(BZTestCase):
    def test_iter(self):
        obj = ApacheBenchExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "concurrency": 2,
            "iterations": 3,
            "scenario": {
                "requests": ["http://blazedemo.com"]
            }
        })
        obj.prepare()
        obj.startup()

    def test_url_exceptions(self):
        obj = ApacheBenchExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "concurrency": 2,
            "scenario": {}})
        obj.prepare()
        self.assertRaises(ValueError, obj.startup)

    def test_check_install_exceptions(self):
        obj = ApacheBenchExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": '*',})
        obj.execution.merge({
            "concurrency": 2,
            "scenario": {}})
        self.assertRaises(RuntimeError, obj.prepare)

    def test_full_execution(self):
        obj = ApacheBenchExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "concurrency": 2,
            "iterations": 3,
            "scenario": {
                "requests": ["http://blazedemo.com"],
            }
        })
        obj.prepare()
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
        log_path = path.join(get_res_path('apachebench.tsv'))
        obj = TSVDataReader(log_path, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))

        self.assertEqual(len(list_of_values), 3)

        for values in list_of_values:
            self.assertTrue(1400000000 < values['ts'] < 1500000000)
            self.assertEqual(len(values), 5)
