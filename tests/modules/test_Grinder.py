import re
import shutil
import time

import os
from bzt import ToolError
from tests import BZTestCase, RESOURCES_DIR, BUILD_DIR, close_reader_file, ROOT_LOGGER

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.grinder import GrinderExecutor, DataLogReader, Grinder, GrinderMirrorsManager
from bzt.modules.provisioning import Local
from bzt.utils import EXE_SUFFIX, get_full_path
from tests.mocks import EngineEmul


def get_grinder():
    obj = GrinderExecutor()
    obj.engine = EngineEmul()
    obj.env = obj.engine.env
    obj.settings.merge({'path': RESOURCES_DIR + "grinder/fake_grinder.jar"})
    return obj


class TestGrinderExecutor(BZTestCase):
    def setUp(self):
        super(TestGrinderExecutor, self).setUp()
        self.obj = get_grinder()

    def tearDown(self):
        if self.obj.stdout_file:
            self.obj.stdout_file.close()
        if self.obj.stderr_file:
            self.obj.stderr_file.close()
        close_reader_file(self.obj.reader)
        super(TestGrinderExecutor, self).tearDown()

    def test_install_Grinder(self):
        path = os.path.abspath(BUILD_DIR + "grinder-taurus/lib/grinder.jar")
        shutil.rmtree(get_full_path(path, step_up=2), ignore_errors=True)

        grinder_link = GrinderMirrorsManager.DOWNLOAD_LINK
        grinder_version = Grinder.VERSION
        mirrors_source = GrinderMirrorsManager.MIRRORS_SOURCE
        try:
            GrinderMirrorsManager.DOWNLOAD_LINK = "file:///" + RESOURCES_DIR + \
                                            "grinder/grinder-{version}_{version}-binary.zip"
            Grinder.VERSION = "3.11"
            GrinderMirrorsManager.MIRRORS_SOURCE = "file:///" + RESOURCES_DIR + "jmeter/unicode_file"

            self.assertFalse(os.path.exists(path))

            self.obj.settings.merge({"path": path})
            self.obj.settings.merge({"properties-file": RESOURCES_DIR + "grinder/grinder.base.properties",
                                     "properties": {"sample_prop": "some_val"}})
            self.obj.execution.merge({"scenario": {
                "script": RESOURCES_DIR + "grinder/helloworld.py",
                "properties-file": RESOURCES_DIR + "grinder/grinder.properties",
                "properties": {"grinder.useConsole": "false"}}})
            self.obj.prepare()

            self.assertTrue(os.path.exists(path))
        finally:
            GrinderMirrorsManager.DOWNLOAD_LINK = grinder_link
            Grinder.VERSION = grinder_version
            GrinderMirrorsManager.MIRRORS_SOURCE = mirrors_source

    def test_install_Grinder_link(self):
        path = os.path.abspath(BUILD_DIR + "grinder-taurus/lib/grinder.jar")
        shutil.rmtree(get_full_path(path, step_up=2), ignore_errors=True)
        self.assertFalse(os.path.exists(path))

        link = "file:///" + RESOURCES_DIR + "grinder/grinder-3.11_3.11-binary.zip"
        self.obj.settings.merge({"download-link": link})
        self.obj.settings.merge({"path": path})
        self.obj.settings.merge({"properties-file": RESOURCES_DIR + "grinder/grinder.base.properties",
                                 "properties": {"sample_prop": "some_val"}})
        self.obj.execution.merge({"scenario": {
            "script": RESOURCES_DIR + "grinder/helloworld.py",
            "properties-file": RESOURCES_DIR + "grinder/grinder.properties",
            "properties": {"grinder.useConsole": "false"}}})
        self.obj.prepare()

        self.assertTrue(os.path.exists(path))

    def test_grinder_widget(self):
        self.obj.engine.config.merge({"provisioning": 'local'})
        self.obj.execution.merge({"concurrency": {"local": 2},
                                  "ramp-up": 2,
                                  "hold-for": 2,
                                  "scenario": {"script": RESOURCES_DIR + "grinder/helloworld.py"}})
        self.obj.prepare()
        self.obj.get_widget()
        self.assertEqual(self.obj.widget.widgets[0].text, "Grinder: helloworld.py")

    def test_resource_files_collection_basic(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "grinder/helloworld.py"}})
        res_files = self.obj.resource_files()
        self.assertEqual(len(res_files), 1)

    def test_fail_on_zero_results(self):
        self.obj.execution.merge({"concurrency": {"local": 2},
                                  "scenario": {"script": RESOURCES_DIR + "grinder/helloworld.py"}})
        self.obj.prepare()
        self.obj.engine.prepared = [self.obj]
        self.obj.engine.started = [self.obj]
        self.obj.engine.provisioning = Local()
        self.obj.engine.provisioning.engine = self.obj.engine
        self.obj.engine.provisioning.executors = [self.obj]
        self.assertRaises(ToolError, self.obj.engine.provisioning.post_process)

    def test_with_results(self):
        self.obj.execution.merge({
            "concurrency": {"local": 2},
            "scenario": {"script": RESOURCES_DIR + "grinder/helloworld.py"}})
        self.obj.prepare()
        self.obj.engine.prepared = [self.obj]
        self.obj.engine.started = [self.obj]
        prov = Local()
        prov.engine = self.obj.engine
        prov.executors = [self.obj]
        self.obj.engine.provisioning = prov
        self.obj.reader.buffer = ['some info']
        self.obj.engine.provisioning.post_process()

    def test_requests(self):
        self.obj.execution.merge({"scenario": {"requests": ['http://blazedemo.com']}})
        self.obj.prepare()

    def test_full_Grinder(self):
        self.obj.execution.merge({"concurrency": {"local": 2},
                                  "hold-for": 5,
                                  "scenario": {"keepalive": False, "requests": ['http://blazedemo.com']}})
        old_cp = self.obj.env.get("CLASSPATH")
        self.obj.prepare()

        self.assertEqual(len(self.obj.cmd_line), 3)
        self.assertNotEqual(old_cp, self.obj.env.get('CLASSPATH'))
        self.assertIn('net.grinder.Grinder', self.obj.cmd_line)

        try:
            self.obj.cmd_line = RESOURCES_DIR + "grinder/grinder" + EXE_SUFFIX
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertFalse(self.obj.has_results())

    def test_script_generation(self):
        self.obj.execution.merge({
            "scenario": {
                "default-address": "http://blazedemo.com",
                "headers": {
                    "My-Header": "Its-Value",
                    "Another-Header": "Another-Value",
                },
                "timeout": "30s",
                "think-time": "2s",
                "store-cookie": True,
                "requests": [
                    '/',
                    {'url': 'http://example.com/',
                     'method': 'POST',
                     'think-time': "1s",
                     'headers': {
                         'Custom': 'Header',
                     }},
                ]
            }
        })
        self.obj.prepare()
        script = open(os.path.join(self.obj.engine.artifacts_dir, 'grinder_requests.py')).read()

        default_addr = re.findall(r"url='http://blazedemo.com'", script)
        self.assertEquals(1, len(default_addr))

        requests = re.findall(r"request\.([A-Z]+)\('(.+?)'", script)
        self.assertEquals(2, len(requests))
        self.assertEquals(requests[0], ('GET', '/'))
        self.assertEquals(requests[1], ('POST', 'http://example.com/'))

        sleeps = re.findall(r"grinder\.sleep\((.+)\)", script)
        self.assertEquals(3, len(sleeps))
        self.assertEquals(sleeps[0], 'sleep_time, 0')
        self.assertEquals(sleeps[1], '2000')
        self.assertEquals(sleeps[2], '1000')

        headers = re.findall(r"NVPair\('(.+)', '(.+)'\)", script)
        self.assertEquals(3, len(headers))
        self.assertIn(("My-Header", "Its-Value"), headers)
        self.assertIn(("Another-Header", "Another-Value"), headers)
        self.assertIn(("Custom", "Header"), headers)

        timeout = re.findall(r"defaults.setTimeout\((\d+)\)", script)
        self.assertEquals(1, len(timeout))
        self.assertEquals(timeout[0], '30000')

        cookies = re.findall(r"defaults.setUseCookies\(1\)", script)
        self.assertEquals(1, len(cookies))

    def test_diagnostics(self):
        self.obj.kpi_file = os.path.abspath(RESOURCES_DIR + 'grinder/test.log')
        self.obj.execution.merge({"hold-for": 2,
                                  "scenario": {"keepalive": False, "requests": ['http://blazedemo.com']}})
        self.obj.prepare()
        try:
            self.obj.cmd_line = RESOURCES_DIR + "grinder/grinder" + EXE_SUFFIX
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertIsNotNone(self.obj.get_error_diagnostics())


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = RESOURCES_DIR + 'grinder/grinder-bzt-kpi.log'
        obj = DataLogReader(log_path, ROOT_LOGGER)
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 20)
        self.assertIn('Test #1', list_of_values[-1][DataPoint.CUMULATIVE])

    def test_read_empty_kpi(self):
        log_path = RESOURCES_DIR + 'grinder/grinder.sh'
        obj = DataLogReader(log_path, ROOT_LOGGER)
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 0)

    def test_read_test_names(self):
        log_path = RESOURCES_DIR + 'grinder/grinder-bzt-1-kpi.log'
        obj = DataLogReader(log_path, ROOT_LOGGER)
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 21)
        self.assertIn('requests_sample', list_of_values[-1][DataPoint.CUMULATIVE])

    def test_read_by_url(self):
        log_path = RESOURCES_DIR + 'grinder/grinder-bzt-kpi.log'
        obj = DataLogReader(log_path, ROOT_LOGGER)
        obj.report_by_url = True
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 20)
        last = list_of_values[-1]
        self.assertIn('http://blazedemo.com/payment.php', last[DataPoint.CUMULATIVE].keys())

    def test_read_errors(self):
        log_path = RESOURCES_DIR + 'grinder/grinder-bzt-1-kpi.log'
        obj = DataLogReader(log_path, ROOT_LOGGER)
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 21)

        last = list_of_values[-1]
        self.assertEquals(2, len(last[DataPoint.CUMULATIVE][''][KPISet.ERRORS]))
        self.assertIn('Not Found', last[DataPoint.CUMULATIVE][''][KPISet.ERRORS][0]['msg'])
        self.assertIn('Java exception', last[DataPoint.CUMULATIVE][''][KPISet.ERRORS][1]['msg'])
