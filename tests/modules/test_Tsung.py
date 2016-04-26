import logging
import time
from os import path

from bzt.modules.tsung import TsungExecutor, TsungStatsReader, TsungConfig
from bzt.six import etree
from bzt.utils import EXE_SUFFIX, BetterDict
from tests import BZTestCase
from tests.mocks import EngineEmul


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

    def test_resource_files_script(self):
        script = get_res_path("http_simple.xml")
        self.obj.execution.merge({"scenario": {"script": script}})
        resources = self.obj.resource_files()
        self.assertEqual(len(resources), 1)
        self.assertIn(script, resources)

    def test_resource_files_requests(self):
        self.obj.execution.merge({"scenario": {"default-address": "http://blazedemo.com",
                                               "requests": ["/"]}})
        resources = self.obj.resource_files()
        self.assertEqual(len(resources), 0)

    def test_widget(self):
        self.obj.execution.merge({"scenario": {"script": get_res_path("http_simple.xml")}})
        self.obj.prepare()
        self.obj.get_widget()

    def test_no_hold_for(self):
        self.obj.execution.merge({
            "scenario": {
                "default-address": "http://blazedemo.com",
                "requests": ["/"]
            }
        })
        self.assertRaises(ValueError, self.obj.prepare)

    def test_full_requests(self):
        self.obj.execution.merge({
            "concurrency": 10,
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
            "concurrency": 200,
            "hold-for": "20s",
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

    def test_no_requests(self):
        self.obj.execution.merge({
            "hold-for": "10s",
            "scenario": {
                "requests": [],
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
            "concurrency": 2,
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

    def test_sessions_thinktime(self):
        obj = TsungExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "concurrency": 50,
            "hold-for": "30s",
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
        obj.execution.merge({
            "concurrency": 50,
            "hold-for": "30s",
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
        obj.execution.merge({
            "concurrency": 50,
            "hold-for": "30s",
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

    def test_load_modification(self):
        obj = TsungExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "concurrency": 50,
            "hold-for": "30s",
            "scenario": {
                "script": get_res_path("http_simple.xml"),
            }
        })
        obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        obj.prepare()
        original_config = TsungConfig()
        original_config.load(get_res_path("http_simple.xml"))
        original_load = original_config.find('//tsung/load')[0]
        modified_config = TsungConfig()
        modified_config.load(obj.tsung_config)
        loads = modified_config.find('//tsung/load')
        self.assertEqual(len(loads), 1)
        modified_load = loads[0]
        self.assertNotEqual(etree.tostring(original_load), etree.tostring(modified_load))
        self.assertEqual(modified_load.get('duration'), '30')

    def test_no_load_no_modication(self):
        # if load profile is not specified - original tsung config's <load> shouldn't be modified
        obj = TsungExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "scenario": {
                "script": get_res_path("http_simple.xml"),
            }
        })
        obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        obj.prepare()
        original_config = TsungConfig()
        original_config.load(get_res_path("http_simple.xml"))
        original_load = original_config.find('//tsung/load')[0]
        modified_config = TsungConfig()
        modified_config.load(obj.tsung_config)
        loads = modified_config.find('//tsung/load')
        self.assertEqual(len(loads), 1)
        modified_load = loads[0]
        self.assertEqual(etree.tostring(original_load), etree.tostring(modified_load))

    def test_modify_dumpstats(self):
        obj = TsungExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "scenario": {
                "script": get_res_path("http_simple.xml"),
            }
        })
        obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        obj.prepare()
        config = TsungConfig()
        config.load(obj.tsung_config)
        elements = config.find('//tsung')
        self.assertEqual(len(elements), 1)
        root = elements[0]
        self.assertEqual(root.get('dumptraffic'), 'protocol')





class TestStatsReader(BZTestCase):
    def test_read(self):
        stats_basedir = get_res_path('stats')
        obj = TsungStatsReader(stats_basedir, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 16)
