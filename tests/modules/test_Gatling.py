import os
import re
import shutil
import time
import logging

from bzt.modules.gatling import GatlingExecutor, DataLogReader, Gatling
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul
from bzt.utils import EXE_SUFFIX


class TestGatlingExecutor(BZTestCase):
    def getGatling(self):
        path = os.path.abspath(__dir__() + "/../../build/gatling-taurus/bin/gatling" + EXE_SUFFIX)
        obj = GatlingExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})
        return obj

    def test_gatling_mirrors(self):
        path = os.path.abspath(__dir__() + "/../../build/tmp/gatling-taurus/bin/gatling" + EXE_SUFFIX)
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)
        obj = GatlingExecutor()
        gatling_tool = Gatling(path, obj.log, GatlingExecutor.VERSION)
        gatling_tool.install()

    def test_install_Gatling(self):
        path = os.path.abspath(__dir__() + "/../../build/tmp/gatling-taurus/bin/gatling" + EXE_SUFFIX)
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)

        GatlingExecutor.DOWNLOAD_LINK = "file:///" + __dir__() + "/../data/gatling-dist-{version}_{version}.zip"
        GatlingExecutor.VERSION = '2.1.4'
        GatlingExecutor.MIRRORS_SOURCE = "file:///" + __dir__() + "/../data/unicode_file"

        self.assertFalse(os.path.exists(path))
        obj = self.getGatling()
        obj.settings.merge({"path": path})

        obj.execution.merge({"scenario": {"script": __dir__() + "/../gatling/BasicSimulation.scala",
                                          "simulation": "mytest.BasicSimulation"}})
        obj.prepare()
        self.assertTrue(os.path.exists(path))

    def test_gatling_widget(self):
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../gatling/BasicSimulation.scala"}})
        obj.prepare()
        obj.get_widget()
        self.assertEqual(obj.widget.widgets[0].text, "Script: BasicSimulation.scala")

    def __check_path_resource_files(self, scala_script_path):
        with open(scala_script_path, 'rt') as fds:
            script_contents = fds.read()
        search_patterns = [re.compile('\.formUpload\(".*?"\)'),
                           re.compile('RawFileBody\(".*?"\)'),
                           re.compile('RawFileBodyPart\(".*?"\)'),
                           re.compile('ELFileBody\(".*?"\)'),
                           re.compile('ELFileBodyPart\(".*?"\)'),
                           re.compile('csv\(".*?"\)'),
                           re.compile('tsv\(".*?"\)'),
                           re.compile('ssv\(".*?"\)'),
                           re.compile('jsonFile\(".*?"\)'),
                           re.compile('separatedValues\(".*?"\)')]
        for search_pattern in search_patterns:
            found_samples = search_pattern.findall(script_contents)
            for found_sample in found_samples:
                param_list = found_sample.split(",")
                param_index = 0 if "separatedValues" in search_pattern.pattern else -1  # first or last param
                file_path = re.compile('\".*?\"').findall(param_list[param_index])[0].strip('"')
                self.assertEqual("", os.path.dirname(file_path))

    def test_resource_files_collection_remote(self):
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../gatling/LocalBasicSimulation.scala"}})
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 14)  # file "gatling_" will be not found
        self.assertEqual(len(artifacts), 12)
        self.__check_path_resource_files(os.path.join(obj.engine.artifacts_dir, "LocalBasicSimulation.scala"))

    def test_resource_files_collection_local(self):
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../gatling/LocalBasicSimulation.scala"}})
        obj.prepare()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(artifacts), 12)
        self.__check_path_resource_files(os.path.join(obj.engine.artifacts_dir, "LocalBasicSimulation.scala"))

    def test_requests_defaddr_iter_headers(self):
        obj = self.getGatling()
        obj.execution.merge({
            "concurrency": 10,
            "iterations": 5,
            "scenario": {
                "think-time": 1,
                "default-address": "http://blazedemo.com",
                "headers": {'SH1': 'SV1', 'SH2': 'SV2'},
                "requests": [{'url': '/reserve.php',
                              'headers': {'RH1': 'RV1', 'RH2': 'RV2'},
                              'method': 'POST'},
                             {'url': '/'}]
            }
        })
        obj.prepare()
        
        self.assertEqualFiles(__dir__() + "/../gatling/generated1.scala",
                              obj.engine.artifacts_dir + "/TaurusSimulation.scala")


    def test_requests_noiter_noramp(self):
        obj = self.getGatling()
        obj.execution.merge({
            "scenario": {
                "concurrency": 10,
                "hold-for": 110,
                "ramp-up": 30,
                "requests": ['http://blazedemo.com', 'http://google.com']
            }
        })
        obj.prepare()

        self.assertEqualFiles(__dir__() + "/../gatling/generated2.scala",
                              obj.engine.artifacts_dir + "/TaurusSimulation.scala")

    def assertEqualFiles(self, name1, name2):
        with open(name1, 'rt') as file1:
            with open(name2, 'rt') as file2:
                lines1 = [line.rstrip() for line in file1.readlines()]
                lines2 = [line.rstrip() for line in file2.readlines()]
        self.assertEqual(lines1, lines2)

    def test_fail_on_zero_results(self):
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../gatling/BasicSimulation.scala"}})
        obj.prepare()
        self.assertRaises(RuntimeWarning, obj.post_process)

    def test_no_simulation(self):
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../gatling/BasicSimulation.scala"}})
        obj.prepare()
        self.assertRaises(ValueError, obj.startup)

    def test_full_Gatling(self):
        obj = self.getGatling()
        obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../gatling/BasicSimulation.scala",
                "simulation": "fake"
            }
        })
        obj.prepare()

        obj.settings.merge({"path": __dir__() + "/../gatling/gatling" + EXE_SUFFIX})

        try:
            obj.startup()

            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = os.path.join(os.path.dirname(__file__), '..', 'gatling')
        obj = DataLogReader(log_path, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 23)
