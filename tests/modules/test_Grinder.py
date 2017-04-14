import logging
import os
import re
import shutil
import time

from bzt.modules.aggregator import DataPoint
from bzt.modules.grinder import GrinderExecutor, DataLogReader
from bzt.modules.provisioning import Local
from bzt import ToolError
from bzt.utils import EXE_SUFFIX
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestGrinderExecutor(BZTestCase):
    def test_install_Grinder(self):
        path = os.path.abspath(__dir__() + "/../../build/tmp/grinder-taurus/lib/grinder.jar")
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)

        grinder_link = GrinderExecutor.DOWNLOAD_LINK
        grinder_version = GrinderExecutor.VERSION
        mirrors_source = GrinderExecutor.MIRRORS_SOURCE
        GrinderExecutor.DOWNLOAD_LINK = "file:///" + __dir__() + "/../data/grinder-{version}_{version}-binary.zip"
        GrinderExecutor.VERSION = "3.11"
        GrinderExecutor.MIRRORS_SOURCE = "file:///" + __dir__() + "/../data/unicode_file"

        self.assertFalse(os.path.exists(path))

        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})
        obj.settings.merge({"properties-file": __dir__() + "/../grinder/grinder.base.properties",
                            "properties": {"sample_prop": "some_val"}})
        obj.execution.merge({"scenario": {
            "script": __dir__() + "/../grinder/helloworld.py",
            "properties-file": __dir__() + "/..//grinder/grinder.properties",
            "properties": {"grinder.useConsole": "false"}}})
        obj.prepare()

        self.assertTrue(os.path.exists(path))

        GrinderExecutor.DOWNLOAD_LINK = grinder_link
        GrinderExecutor.VERSION = grinder_version
        GrinderExecutor.MIRRORS_SOURCE = mirrors_source

    def test_grinder_widget(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({'path': __dir__() + "/../grinder/fake_grinder.jar"})
        obj.engine.config.merge({"provisioning": 'local'})
        obj.execution.merge({"concurrency": {"local": 2},
                             "ramp-up": 2,
                             "hold-for": 2,
                             "scenario": {"script": __dir__() + "/../grinder/helloworld.py"}})
        obj.prepare()
        obj.get_widget()
        self.assertEqual(obj.widget.widgets[0].text, "Grinder: helloworld.py")

    def test_resource_files_collection_basic(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../grinder/helloworld.py"}})
        res_files = obj.resource_files()
        self.assertEqual(len(res_files), 1)

    def test_fail_on_zero_results(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({'path': __dir__() + "/../grinder/fake_grinder.jar"})
        obj.execution.merge({"concurrency": {"local": 2},
                             "scenario": {"script": __dir__() + "/../grinder/helloworld.py"}})
        obj.prepare()
        obj.engine.prepared = [obj]
        obj.engine.started = [obj]
        obj.engine.provisioning = Local()
        obj.engine.provisioning.engine = obj.engine
        obj.engine.provisioning.executors = [obj]
        self.assertRaises(ToolError, obj.engine.provisioning.post_process)

    def test_with_results(self):
        obj = GrinderExecutor()

        obj.engine = EngineEmul()
        obj.settings.merge({'path': __dir__() + "/../grinder/fake_grinder.jar"})
        obj.execution.merge({
            "concurrency": {"local": 2},
            "scenario": {"script": __dir__() + "/../grinder/helloworld.py"}})
        obj.prepare()
        obj.engine.prepared = [obj]
        obj.engine.started = [obj]
        prov = Local()
        prov.engine = obj.engine
        prov.executors = [obj]
        obj.engine.provisioning = prov
        obj.reader.buffer = ['some info']
        obj.engine.provisioning.post_process()

    def test_requests(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({'path': __dir__() + "/../grinder/fake_grinder.jar"})
        obj.execution.merge({"scenario": {"requests": ['http://blazedemo.com']}})
        obj.prepare()

    def test_full_Grinder(self):
        obj = GrinderExecutor()
        obj.kpi_file = os.path.abspath(__dir__() + '/../grinder/test.log')
        obj.engine = EngineEmul()
        obj.settings.merge({'path': __dir__() + "/../grinder/fake_grinder.jar"})
        obj.execution.merge({"concurrency": {"local": 2},
                             "hold-for": 5,
                             "scenario": {"keepalive": False, "requests": ['http://blazedemo.com']}})
        obj.prepare()

        self.assertEqual(len(obj.cmd_line), 5)
        cmd_line = ' '.join(obj.cmd_line)
        self.assertTrue(cmd_line.startswith('java -classpath'))
        self.assertNotEqual(cmd_line.find('net.grinder.Grinder'), -1)

        try:
            obj.cmd_line = __dir__() + "/../grinder/grinder" + EXE_SUFFIX
            obj.startup()
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()
        obj.post_process()
        self.assertFalse(obj.has_results())

    def test_script_generation(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({'path': __dir__() + "/../grinder/fake_grinder.jar"})
        obj.execution.merge({
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
        obj.prepare()
        script = open(os.path.join(obj.engine.artifacts_dir, 'grinder_requests.py')).read()

        default_addr = re.findall(r"url='http://blazedemo.com'", script)
        self.assertEquals(1, len(default_addr))

        requests = re.findall(r"request\.([A-Z]+)\('(.+?)'", script)
        self.assertEquals(2, len(requests))
        self.assertEquals(requests[0], ('GET', '/'))
        self.assertEquals(requests[1], ('POST', 'http://example.com/'))

        sleeps = re.findall(r"grinder\.sleep\((.+)\)", script)
        self.assertEquals(2, len(sleeps))
        self.assertEquals(sleeps[0], '2000')
        self.assertEquals(sleeps[1], '1000')

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


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = os.path.join(os.path.dirname(__file__), '..', 'grinder', 'grinder-bzt-kpi.log')
        obj = DataLogReader(log_path, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 12)
        self.assertIn('Test #1', list_of_values[-1][DataPoint.CUMULATIVE])

    def test_read_empty_kpi(self):
        log_path = os.path.join(os.path.dirname(__file__), '..', 'grinder', 'grinder.sh')
        obj = DataLogReader(log_path, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 0)

    def test_read_test_names(self):
        log_path = os.path.join(os.path.dirname(__file__), '..', 'grinder', 'grinder-bzt-1-kpi.log')
        obj = DataLogReader(log_path, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 5)
        self.assertIn('BZT Requests', list_of_values[-1][DataPoint.CUMULATIVE])

    def test_read_by_url(self):
        log_path = os.path.join(os.path.dirname(__file__), '..', 'grinder', 'grinder-bzt-kpi.log')
        obj = DataLogReader(log_path, logging.getLogger(''))
        obj.report_by_url=True
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 12)
        self.assertIn('http://blazedemo.com/', list_of_values[0][DataPoint.CUMULATIVE])
