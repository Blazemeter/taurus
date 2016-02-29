import logging
import os
import re
import shutil
import time

from bzt.modules.gatling import GatlingExecutor, DataLogReader, Gatling
from bzt.utils import EXE_SUFFIX
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


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
                "default-address": "blazedemo.com",
                "headers": {'H1': 'V1'},
                "requests": [{'url': '/reserve.php',
                              'headers': {'H2': 'V2'},
                              'method': 'POST',
                              'body': 'Body Content'},
                             {'url': '/'}]
            }
        })
        obj.prepare()
        scala_file = obj.engine.artifacts_dir + '/' + obj.get_scenario().get('simulation') + '.scala'
        self.assertEqualFiles(__dir__() + "/../gatling/generated1.scala", scala_file)

    def test_requests_noiter_noramp(self):
        obj = self.getGatling()
        obj.execution.merge({
            "concurrency": 10,
            "hold-for": 110,
            "ramp-up": 30,
            "scenario": {
                'keepalive': 'false',
                'timeout': '100ms',
                "requests": ['http://blazedemo.com', 'google.com']
            }
        })
        obj.prepare()

        scala_file = obj.engine.artifacts_dir + '/' + obj.get_scenario().get('simulation') + '.scala'
        self.assertEqualFiles(__dir__() + "/../gatling/generated2.scala", scala_file)

    def assertEqualFiles(self, name1, name2):
        def without_id(lines):
            id_mark = 'TaurusSimulation'
            id_pos = lines.find(id_mark)
            space_pos = lines.find(' ', id_pos)
            return lines[:id_pos + len(id_mark)] + lines[space_pos:]

        with open(name1, 'rt') as file1:
            with open(name2, 'rt') as file2:
                lines1 = without_id(file1.read())
                lines2 = without_id(file2.read())
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

    def test_interactive_request(self):
        obj = self.getGatling()
        obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../gatling",
                "simulation": "fake"
            }
        })
        obj.prepare()
        try:
            obj.startup()
        finally:
            pass


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = os.path.join(os.path.dirname(__file__), '..', 'gatling')
        obj = DataLogReader(log_path, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 23)
