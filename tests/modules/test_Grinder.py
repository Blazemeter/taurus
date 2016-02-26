import logging
import os
import shutil
import time

from bzt.modules.grinder import GrinderExecutor, Grinder, DataLogReader
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
        obj.engine.config.merge({"provisioning": 'local'})
        obj.execution.merge({"concurrency": {"local": 2},
                             "ramp-up": 2,
                             "hold-for": 2,
                             "scenario": {"script": __dir__() + "/../grinder/helloworld.py"}})
        obj.prepare()
        obj.get_widget()
        self.assertEqual(obj.widget.widgets[0].text, "Script: helloworld.py")

    def test_resource_files_collection_basic(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../grinder/helloworld.py"}})
        res_files = obj.resource_files()
        self.assertEqual(len(res_files), 1)

    def test_fail_on_zero_results(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"concurrency": {"local": 2},
                             "scenario": {"script": __dir__() + "/../grinder/helloworld.py"}})
        obj.prepare()
        self.assertRaises(RuntimeWarning, obj.post_process)

    def test_grinder_mirrors(self):
        path = os.path.abspath(__dir__() + "/../../build/tmp/grinder-taurus/lib/grinder.jar")
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)
        obj = GrinderExecutor()
        grinder_tool = Grinder(path, obj.log, GrinderExecutor.VERSION)
        grinder_tool.install()

    def test_requests(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"requests": ['http://blazedemo.com']}})
        obj.prepare()

    def test_full_Grinder(self):
        obj = GrinderExecutor()
        obj.kpi_file = os.path.abspath(__dir__() + '/../grinder/test.log')
        obj.engine = EngineEmul()
        obj.execution.merge({"concurrency": {"local": 2},
                             "hold-for": 5,
                             "scenario": {"requests": ['http://blazedemo.com']}})
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
        self.assertRaises(RuntimeWarning, obj.post_process)


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = os.path.join(os.path.dirname(__file__), '..', 'grinder', 'grinder-bzt-kpi.log')
        obj = DataLogReader(log_path, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 10)
