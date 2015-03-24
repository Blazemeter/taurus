'''
Created on Mar 23, 2015

@author: Coeurl
'''

from tests import setup_test_logging, BZTestCase, __dir__
from bzt.modules.grinder import GrinderExecutor
import shutil
import os
from tests.mocks import EngineEmul
from bzt.utils import BetterDict
import bzt.utils

setup_test_logging()

class TestGrinderExecutor(BZTestCase):
    
    def test_install_Grinder(self):
        bzt.utils.TEST_RUNNING = True
        path = os.path.abspath(__dir__() + "/../../build/tmp/grinder-taurus/lib/grinder.jar")
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)
        
        grinder_link = GrinderExecutor.DOWNLOAD_LINK
        grinder_version = GrinderExecutor.VERSION
        GrinderExecutor.DOWNLOAD_LINK = "file://" + __dir__() + "/../data/grinder-{version}_{version}-binary.zip"
        GrinderExecutor.VERSION = "3.11"
        
        self.assertFalse(os.path.exists(path))
        
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {
                                         "script": "tests/grinder/local_helloworld.py",
                                         "properties_file": "tests/grinder/grinder.properties",
                                         "properties": {"grinder.useConsole": "false"}}})
        obj.prepare()
        
        self.assertTrue(os.path.exists(path))
        
        obj.prepare()
        GrinderExecutor.DOWNLOAD_LINK = grinder_link
        GrinderExecutor.VERSION = grinder_version
        bzt.utils.TEST_RUNNING = False
        