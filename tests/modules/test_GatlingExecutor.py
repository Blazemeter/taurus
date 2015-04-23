'''
Created on Mar 20, 2015

@author: Coeurl
'''

from tests import setup_test_logging, BZTestCase, __dir__
from bzt.modules.gatling import GatlingExecutor
import shutil
import os
import json
from tests.mocks import EngineEmul
import bzt.utils
from bzt.utils import BetterDict

setup_test_logging()

class TestGatlingExecutor(BZTestCase):
    def test_install_Gatling(self):
        bzt.utils.TEST_RUNNING = True
        
        path = os.path.abspath(__dir__() + "/../../build/tmp/gatling-taurus/bin/gatling.sh")
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)
        
        #backup download link and version
        gatling_link = GatlingExecutor.DOWNLOAD_LINK
        gatling_ver = GatlingExecutor.VERSION
        
        GatlingExecutor.DOWNLOAD_LINK = "file://" + __dir__() + "/../data/gatling-dist-{version}_{version}.zip"
        GatlingExecutor.VERSION = '2.1.4'
        
        self.assertFalse(os.path.exists(path))
        obj = GatlingExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})
        
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": "tests/gatling/BasicSimulation.scala",\
                                          "simulation": "mytest.BasicSimulation"}})
        obj.prepare()
        self.assertTrue(os.path.exists(path))
        obj.prepare()
        GatlingExecutor.DOWNLOAD_LINK = gatling_link
        GatlingExecutor.VERSION = gatling_ver
        
        bzt.utils.TEST_RUNNING = False

    def test_gatling_widget(self):
        obj = GatlingExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario":{"script":"tests/gatling/BasicSimulation.scala"}})
        obj.prepare()
        obj.get_widget()
        self.assertEqual(obj.widget.script_name.text, "Script: BasicSimulation.scala")

    def test_resource_files_collection(self):
        obj = GatlingExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario":{"script":"tests/gatling/LocalBasicSimulation.scala"}})
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 13)
        self.assertEqual(len(artifacts), 13)

    def test_resource_files_from_requests(self):
        obj = GatlingExecutor()
        obj.engine = EngineEmul()
        obj.engine.config = json.loads(open("tests/json/get-post.json").read())
        obj.execution = obj.engine.config['execution']
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 1)
        self.assertEqual(len(artifacts), 1)