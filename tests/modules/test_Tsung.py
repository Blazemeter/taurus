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
    def test_servers(self):
        obj = TsungExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "hold-for": "10s",
            "scenario": {
                "default-address": "http://example.com:8080",
                "requests": ["/"],
            }
        })
        obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        obj.prepare()
        config = TsungConfig()
        config.load(obj.tsung_config)
        servers = config.find('//servers/server')
        self.assertEquals(1, len(servers))
        server = servers[0]
        self.assertEqual(server.get('host'), 'example.com')
        self.assertEqual(server.get('port'), '8080')

    def test_sessions_requests(self):
        obj = TsungExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "throughput": 2,
            "hold-for": "10s",
            "scenario": {
                "default-address": "http://example.com",
                "requests": ["/", "/reserve.php"],
            }
        })
        obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        obj.prepare()
        config = TsungConfig()
        config.load(obj.tsung_config)
        requests = config.find('//request')
        self.assertEquals(2, len(requests))

    def test_load_rampup(self):
        obj = TsungExecutor()
        obj.engine = EngineEmul()
        throughput = 50
        rampup = 30
        obj.execution.merge({
            "throughput": throughput,
            "ramp-up": rampup,
            "scenario": {
                "default-address": "http://example.com",
                "requests": ["/", "/reserve.php"],
            }
        })
        obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        obj.prepare()
        config = TsungConfig()
        config.load(obj.tsung_config)
        users = config.find('//user')
        estimated_count = rampup  * throughput / 2.0
        error = abs(len(users) - estimated_count) / len(users)
        self.assertLess(error, 0.05)

    def test_sessions_thinktime(self):
        obj = TsungExecutor()
        obj.engine = EngineEmul()
        throughput = 50
        rampup = 30
        obj.execution.merge({
            "throughput": throughput,
            "ramp-up": rampup,
            "scenario": {
                "default-address": "http://example.com",
                "requests": [{
                    "url": "/",
                    "think-time": "1s",
                }, {
                    "url": "/reserve.php",
                    "think-time": "2s",
                }],
            }
        })
        obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        obj.prepare()
        config = TsungConfig()
        config.load(obj.tsung_config)
        thinktimes = config.find('//thinktime')
        self.assertEqual(len(thinktimes), 2)
        self.assertEqual(thinktimes[0].get("value"), "1")
        self.assertEqual(thinktimes[1].get("value"), "2")

    def test_requests_custom(self):
        obj = TsungExecutor()
        obj.engine = EngineEmul()
        throughput = 50
        rampup = 30
        obj.execution.merge({
            "throughput": throughput,
            "ramp-up": rampup,
            "scenario": {
                "default-address": "http://example.com",
                "requests": [{
                    "url": "/",
                    "think-time": "1s",
                    "method": "GET"
                }, {
                    "url": "/reserve.php",
                    "think-time": "2s",
                    "method": "POST",
                    "body": "123",
                }, {
                    "url": "/reserve.php",
                    "think-time": "3s",
                    "method": "PUT",
                    "body-file": get_res_path("http_simple.xml"),
                }],
            }
        })
        obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        obj.prepare()
        config = TsungConfig()
        config.load(obj.tsung_config)
        urls = config.find('//http')
        self.assertEqual(len(urls), 3)
        self.assertEqual(urls[0].get("method"), "GET")
        self.assertEqual(urls[1].get("method"), "POST")
        self.assertEqual(urls[1].get("contents"), "123")
        self.assertEqual(urls[2].get("method"), "PUT")
        self.assertEqual(urls[2].get("contents"), open(get_res_path('http_simple.xml')).read())

    def test_requests_headers(self):
        obj = TsungExecutor()
        obj.engine = EngineEmul()
        throughput = 50
        rampup = 30
        obj.execution.merge({
            "throughput": throughput,
            "ramp-up": rampup,
            "scenario": {
                "default-address": "http://example.com",
                "requests": [{
                    "url": "/",
                    "headers": [
                        {"X-Answer": "42"},
                        {"X-Jedi": "Luke Skywalker"}
                    ],
                }],
            }
        })
        obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        obj.prepare()
        config = TsungConfig()
        config.load(obj.tsung_config)
        headers = config.find('//http/http_header')
        self.assertEqual(len(headers), 2)
        self.assertEqual(headers[0].get("name"), "X-Answer")
        self.assertEqual(headers[0].get("value"), "42")
        self.assertEqual(headers[1].get("name"), "X-Jedi")
        self.assertEqual(headers[1].get("value"), "Luke Skywalker")


class TestStatsReader(BZTestCase):
    def test_read(self):
        stats_basedir = get_res_path('stats')
        obj = TsungStatsReader(stats_basedir, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 16)
