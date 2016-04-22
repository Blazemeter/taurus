import logging
import time
from os import path

from bzt.modules.tsung import TsungExecutor, TsungStatsReader
from tests import BZTestCase
from tests.mocks import EngineEmul
from bzt.utils import EXE_SUFFIX


TOOL_NAME = 'tsung' + EXE_SUFFIX


def get_res_path(resource):
    return path.join(path.dirname(__file__), '..', 'tsung', resource)


class TestTsungExecutor(BZTestCase):
    def setUp(self):
        self.obj = TsungExecutor()
        self.obj.engine = EngineEmul()
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})

    def test_prepare_no_script_no_requests(self):
        self.obj.execution.merge({"scenario": {}})
        self.assertRaises(ValueError, self.obj.prepare)

    def test_check_install(self):
        self.obj.settings.merge({"path": "*"})
        self.obj.execution.merge({"scenario": {"script": get_res_path("http_simple.xml")}})
        self.assertRaises(RuntimeError, self.obj.prepare)

    def test_script(self):
        self.obj.execution.merge({"scenario": {"script": get_res_path("http_simple.xml")}})
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()

    def test_requests(self):
        self.fail("not implemented")

    def test_resource_files(self):
        self.fail("not implemented")

    def test_widget(self):
        self.obj.execution.merge({"scenario": {"script": get_res_path("http_simple.xml")}})
        self.obj.prepare()
        self.obj.get_widget()

    def test_full_requests(self):
        self.obj.execution.merge({
            "concurrency": 2,
            "iterations": 3,
            "scenario": {
                "think-time": "1s",
                "requests": ["http://blazedemo.com",
                             "http://ya.ru"]}
        })
        self.obj.prepare()
        self.obj.get_widget()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()

    def test_full_script(self):
        self.obj.execution.merge({
            "concurrency": 2,
            "iterations": 3,
            "scenario": {
                "script": get_res_path("http_simple.xml")
            }
        })
        self.obj.prepare()
        self.obj.get_widget()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()



class TestStatsReader(BZTestCase):
    def test_read(self):
        log_path = path.join(get_res_path('siege.out'))
        obj = TsungStatsReader(log_path, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))

        self.assertEqual(len(list_of_values), 8)

        for values in list_of_values:
            self.assertTrue(1400000000 < values['ts'] < 1500000000)
            self.assertEqual(len(values), 5)
