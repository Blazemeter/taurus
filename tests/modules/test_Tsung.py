import logging
import time
from os import path

from bzt.modules.tsung import TsungExecutor, TsungStatsReader, TsungConfig
from tests import BZTestCase
from tests.mocks import EngineEmul
from bzt.utils import EXE_SUFFIX, BetterDict


TOOL_NAME = 'tsung' + EXE_SUFFIX


def get_res_path(resource):
    return path.join(path.dirname(__file__), '..', 'tsung', resource)


class TestTsungExecutor(BZTestCase):
    def setUp(self):
        self.obj = TsungExecutor()
        self.obj.engine = EngineEmul()
        self.obj.settings = BetterDict()
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.execution = BetterDict()

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

    def test_resource_files(self):
        self.fail("not implemented")

    def test_widget(self):
        self.obj.execution.merge({"scenario": {"script": get_res_path("http_simple.xml")}})
        self.obj.prepare()
        self.obj.get_widget()

    def test_full_requests(self):
        self.obj.execution.merge({
            "throughput": 10,
            "hold-for": "20s",
            "scenario": {
                "default-address": "http://blazedemo.com",
                "requests": ["/",
                             "/reserve.php"]}
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

    def test_generated_config_no_default_address(self):
        self.obj.execution.merge({
            "throughput": 2,
            "hold-for": "10s",
            "scenario": {
                "requests": ["http://example.com/", "http://blazedemo.com/"],
            }
        })
        self.assertRaises(ValueError, self.obj.prepare)


class TestTsungConfig(BZTestCase):
    def test_load(self):
        self.fail("not implemented")

    def test_clients(self):
        self.fail("not implemented")

    def test_servers(self):
        self.fail("not implemented")

    def test_sessions_requests(self):
        obj = TsungExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "throughput": 2,
            "hold-for": "10s",
            "scenario": {
                "default-address": "http://example.com",
                "requests": ["/", "/reserve.php"],
            }
        })
        obj.prepare()
        config = TsungConfig()
        config.load(obj.tsung_config)
        requests = config.find('//request')
        self.assertEquals(2, len(requests))


class TestStatsReader(BZTestCase):
    def test_read(self):
        self.fail("not implemented")
