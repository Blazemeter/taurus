import logging
import time
import unittest
from os import path

from bzt import TaurusConfigError, ToolError
from bzt.modules.tsung import TsungExecutor, TsungStatsReader, TsungConfig, Tsung
from bzt.six import etree
from bzt.utils import EXE_SUFFIX, BetterDict, is_windows
from tests import BZTestCase
from tests.mocks import EngineEmul


TOOL_NAME = 'tsung' + EXE_SUFFIX


def get_res_path(resource):
    return path.join(path.dirname(__file__), '..', 'resources', 'tsung', resource)


class TestTsungExecutor(BZTestCase):
    def setUp(self):
        super(TestTsungExecutor, self).setUp()
        self.obj = TsungExecutor()
        self.obj.engine = EngineEmul()
        self.obj.env = self.obj.engine.env
        self.obj.settings = BetterDict()
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.execution = BetterDict()

    def test_prepare_no_script_no_requests(self):
        self.obj.execution.merge({"scenario": {}})
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_check_install(self):
        self.obj.settings.merge({"path": "*"})
        self.obj.execution.merge({"scenario": {"script": get_res_path("http_simple.xml")}})
        self.assertRaises(ToolError, self.obj.prepare)

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
        self.assertRaises(TaurusConfigError, self.obj.prepare)

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
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_multiexec_controller_id(self):
        self.obj.execution.merge({
            "hold-for": "10s",
            "scenario": {
                "requests": ["http://blazedemo.com/"],
            }
        })
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        stdout = open(path.join(self.obj.engine.artifacts_dir, 'tsung.out')).read()
        cid_param = '-i %s' % self.obj.tsung_controller_id
        self.assertIn(cid_param, stdout)

    def test_diagnostics(self):
        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 1,
            "hold-for": "5s",
            "scenario": {
                "default-address": "http://blazedemo.com",
                "requests": ["/"]}
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.obj.post_process()
        self.assertIsNotNone(self.obj.get_error_diagnostics())


class TestTsungConfig(BZTestCase):
    def setUp(self):
        super(TestTsungConfig, self).setUp()
        self.obj = TsungExecutor()
        self.obj.engine = EngineEmul()

    def test_servers(self):
        self.obj.execution.merge({
            "hold-for": "10s",
            "scenario": {
                "default-address": "http://example.com:8080",
                "requests": ["/"],
            }
        })
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.prepare()
        config = TsungConfig(None)
        config.load(self.obj.tsung_config)
        servers = config.find('//servers/server')
        self.assertEquals(1, len(servers))
        server = servers[0]
        self.assertEqual(server.get('host'), 'example.com')
        self.assertEqual(server.get('port'), '8080')

    def test_sessions_requests(self):
        self.obj.execution.merge({
            "concurrency": 2,
            "hold-for": "10s",
            "scenario": {
                "default-address": "http://example.com",
                "requests": ["/", "/reserve.php"],
            }
        })
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.prepare()
        config = TsungConfig(None)
        config.load(self.obj.tsung_config)
        requests = config.find('//request')
        self.assertEquals(2, len(requests))

    def test_sessions_thinktime(self):
        self.obj.execution.merge({
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
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.prepare()
        config = TsungConfig(None)
        config.load(self.obj.tsung_config)
        thinktimes = config.find('//thinktime')
        self.assertEqual(len(thinktimes), 2)
        self.assertEqual(thinktimes[0].get("value"), "1")
        self.assertEqual(thinktimes[1].get("value"), "2")

    def test_requests_custom(self):
        self.obj.execution.merge({
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
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.prepare()
        config = TsungConfig(None)
        config.load(self.obj.tsung_config)
        urls = config.find('//http')
        self.assertEqual(len(urls), 3)
        self.assertEqual(urls[0].get("method"), "GET")
        self.assertEqual(urls[1].get("method"), "POST")
        self.assertEqual(urls[1].get("contents"), "123")
        self.assertEqual(urls[2].get("method"), "PUT")
        self.assertEqual(urls[2].get("contents"), open(get_res_path('http_simple.xml')).read())

    def test_requests_headers(self):
        self.obj.execution.merge({
            "concurrency": 50,
            "hold-for": "30s",
            "scenario": {
                "default-address": "http://example.com",
                "headers": {
                    "X-Answer": "42",
                },
                "requests": [{
                    "url": "/",
                    "headers": {
                        "X-Jedi": "Luke Skywalker",
                    },
                }],
            }
        })
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.prepare()
        config = TsungConfig(None)
        config.load(self.obj.tsung_config)
        headers = config.find('//http/http_header')
        self.assertEqual(len(headers), 2)
        headers_list = [(h.get('name'), h.get('value')) for h in headers]
        self.assertIn(("X-Jedi", "Luke Skywalker"), headers_list)
        self.assertIn(("X-Answer", "42"), headers_list)

    def test_load_modification(self):
        self.obj.execution.merge({
            "concurrency": 50,
            "hold-for": "30s",
            "scenario": {
                "script": get_res_path("http_simple.xml"),
            }
        })
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.prepare()
        original_config = TsungConfig(None)
        original_config.load(get_res_path("http_simple.xml"))
        original_load = original_config.find('//tsung/load')[0]
        modified_config = TsungConfig(None)
        modified_config.load(self.obj.tsung_config)
        loads = modified_config.find('//tsung/load')
        self.assertEqual(len(loads), 1)
        modified_load = loads[0]
        self.assertNotEqual(etree.tostring(original_load), etree.tostring(modified_load))
        self.assertEqual(modified_load.get('duration'), '30')

    def test_no_load_no_modication(self):
        # if load profile is not specified - original tsung config's <load> shouldn't be modified
        self.obj.execution.merge({
            "scenario": {
                "script": get_res_path("http_simple.xml"),
            }
        })
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.prepare()
        original_config = TsungConfig(None)
        original_config.load(get_res_path("http_simple.xml"))
        original_load = original_config.find('//tsung/load')[0]
        modified_config = TsungConfig(None)
        modified_config.load(self.obj.tsung_config)
        loads = modified_config.find('//tsung/load')
        self.assertEqual(len(loads), 1)
        modified_load = loads[0]
        self.assertEqual(etree.tostring(original_load), etree.tostring(modified_load))

    def test_modify_dumpstats(self):
        self.obj.execution.merge({
            "scenario": {
                "script": get_res_path("http_simple.xml"),
            }
        })
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.prepare()
        config = TsungConfig(None)
        config.load(self.obj.tsung_config)
        elements = config.find('//tsung')
        self.assertEqual(len(elements), 1)
        root = elements[0]
        self.assertEqual(root.get('dumptraffic'), 'protocol')
        self.assertEqual(root.get('backend'), 'text')

    def test_scenario_thinktime(self):
        self.obj.execution.merge({
            "hold-for": "10s",
            "scenario": {
                "think-time": "3s",
                "requests": ["http://blazedemo.com/"]
            }
        })
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.prepare()
        config = TsungConfig(None)
        config.load(self.obj.tsung_config)
        elements = config.find('//options/option[@name="thinktime"]')
        self.assertEqual(len(elements), 1)
        thinktime = elements[0]
        self.assertEqual(thinktime.get('value'), '3')

    def test_scenario_timeout(self):
        self.obj.execution.merge({
            "hold-for": "10s",
            "scenario": {
                "timeout": "1s",
                "requests": ["http://blazedemo.com/"]
            }
        })
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.prepare()
        config = TsungConfig(None)
        config.load(self.obj.tsung_config)
        elements = config.find('//options/option[@name="connect_timeout"]')
        self.assertEqual(len(elements), 1)
        thinktime = elements[0]
        self.assertEqual(thinktime.get('value'), '1000')

    def test_scenario_max_retries(self):
        self.obj.execution.merge({
            "hold-for": "10s",
            "scenario": {
                "max-retries": "5",
                "requests": ["http://blazedemo.com/"]
            }
        })
        self.obj.settings.merge({"path": get_res_path(TOOL_NAME),})
        self.obj.prepare()
        config = TsungConfig(None)
        config.load(self.obj.tsung_config)
        elements = config.find('//options/option[@name="max_retries"]')
        self.assertEqual(len(elements), 1)
        thinktime = elements[0]
        self.assertEqual(thinktime.get('value'), '5')


class TestStatsReader(BZTestCase):
    def test_read(self):
        stats_basedir = get_res_path('stats')
        obj = TsungStatsReader(stats_basedir, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 16)


@unittest.skipIf(is_windows(), "Tsung is not supported in Windows")
class TestTool(BZTestCase):
    def test_prefix_local(self):
        prefix = Tsung.get_tool_prefix("/usr/local/bin/tsung")
        self.assertEqual(prefix, "/usr/local")

    def test_prefix(self):
        prefix = Tsung.get_tool_prefix("/usr/bin/tsung")
        self.assertEqual(prefix, "/usr")
