'''
Created on Mar 20, 2015

@author: Coeurl
'''

from tests import setup_test_logging, BZTestCase, __dir__
from bzt.modules.gatling import GatlingExecutor
import shutil
import os
from tests.mocks import EngineEmul
from bzt.utils import BetterDict

setup_test_logging()

class TestGatlingExecutor(BZTestCase):
    
    def test_install_Gatling(self):
        """
        Test Gatling installation
        """
        
        #=======================================================================
        # _progress_hook = download_progress_hook
        # download_progress_hook = download_progress_mock
        #=======================================================================
        
        path = os.path.abspath(__dir__() + "/../../build/tmp/gatling-taurus/bin/gatling.sh")
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)
        
        #backup download link and version
        gatling_link = GatlingExecutor.DOWNLOAD_LINK
        gatling_ver = GatlingExecutor.VERSION
        
        # NOTE: Gatling download link format: %s/blah/%s
        GatlingExecutor.DOWNLOAD_LINK = "file://" + __dir__() + "/../data/gatling-dist-%s_%s.zip"
        GatlingExecutor.VERSION = '2.1.4'
        
        self.assertFalse(os.path.exists(path))
        
        obj = GatlingExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})
        
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {"script": "tests/gatling/LocalBasicSimulation.scala",\
                                          "simulation": "mytest.LocalBasicSimulation"}})
        
        obj.prepare()
        
        self.assertTrue(os.path.exists(path))
        
        obj.prepare()

        GatlingExecutor.DOWNLOAD_LINK = gatling_link
        GatlingExecutor.VERSION = gatling_ver