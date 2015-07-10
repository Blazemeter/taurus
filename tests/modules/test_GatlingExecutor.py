import shutil
import os
import re

from tests import setup_test_logging, BZTestCase, __dir__
from bzt.modules.gatling import GatlingExecutor
from tests.mocks import EngineEmul
from bzt.utils import BetterDict

setup_test_logging()


class TestGatlingExecutor(BZTestCase):
    def test_install_Gatling(self):
        path = os.path.abspath(__dir__() + "/../../build/tmp/gatling-taurus/bin/gatling.sh")
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)

        # backup download link and version
        gatling_link = GatlingExecutor.DOWNLOAD_LINK
        gatling_ver = GatlingExecutor.VERSION

        GatlingExecutor.DOWNLOAD_LINK = "file://" + __dir__() + "/../data/gatling-dist-{version}_{version}.zip"
        GatlingExecutor.VERSION = '2.1.4'

        self.assertFalse(os.path.exists(path))
        obj = GatlingExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})

        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": "tests/gatling/BasicSimulation.scala",
                                          "simulation": "mytest.BasicSimulation"}})
        obj.prepare()
        self.assertTrue(os.path.exists(path))
        obj.prepare()
        GatlingExecutor.DOWNLOAD_LINK = gatling_link
        GatlingExecutor.VERSION = gatling_ver


    def test_gatling_widget(self):
        obj = GatlingExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": "tests/gatling/BasicSimulation.scala"}})
        obj.prepare()
        obj.get_widget()
        self.assertEqual(obj.widget.script_name.text, "Script: BasicSimulation.scala")

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
        obj = GatlingExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": "tests/gatling/LocalBasicSimulation.scala"}})
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 15)  # file "gatling_" will be not found
        self.assertEqual(len(artifacts), 14)
        self.__check_path_resource_files(os.path.join(obj.engine.artifacts_dir, "LocalBasicSimulation.scala"))

    def test_resource_files_collection_local(self):
        obj = GatlingExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": "tests/gatling/LocalBasicSimulation.scala"}})
        obj.prepare()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(artifacts), 14)
        self.__check_path_resource_files(os.path.join(obj.engine.artifacts_dir, "LocalBasicSimulation.scala"))