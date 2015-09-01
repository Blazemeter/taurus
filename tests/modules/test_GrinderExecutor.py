import shutil
import os

from tests import setup_test_logging, BZTestCase, __dir__
from bzt.modules.grinder import GrinderExecutor
from tests.mocks import EngineEmul
from bzt.utils import BetterDict

setup_test_logging()


class TestGrinderExecutor(BZTestCase):
    def test_install_Grinder(self):
        path = os.path.abspath(__dir__() + "/../../build/tmp/grinder-taurus/lib/grinder.jar")
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)

        grinder_link = GrinderExecutor.DOWNLOAD_LINK
        grinder_version = GrinderExecutor.VERSION
        GrinderExecutor.DOWNLOAD_LINK = "file:///" + __dir__() + "/../data/grinder-{version}_{version}-binary.zip"
        GrinderExecutor.VERSION = "3.11"

        self.assertFalse(os.path.exists(path))

        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})
        obj.execution = BetterDict()
        obj.execution.merge({"scenario": {
            "script": __dir__() + "/../grinder/helloworld.py",
            "properties-file": __dir__() + "/..//grinder/grinder.properties",
            "properties": {"grinder.useConsole": "false"}}})
        obj.prepare()

        self.assertTrue(os.path.exists(path))

        obj.prepare()
        GrinderExecutor.DOWNLOAD_LINK = grinder_link
        GrinderExecutor.VERSION = grinder_version

    def test_grinder_widget(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../grinder/helloworld.py"}})
        obj.prepare()
        obj.get_widget()
        self.assertEqual(obj.widget.widgets[0].text, "Script: helloworld.py")

    def test_resource_files_collection_remote(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../grinder/helloworld.py",
                                          "properties-file": __dir__() + "/../grinder/grinder.properties"}})
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 2)
        self.assertEqual(len(artifacts), 2)

    def test_resource_files_collection_local(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../grinder/helloworld.py",
                                          "properties-file": __dir__() + "/../grinder/grinder.properties"}})
        obj.prepare()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(artifacts), 2)

    def test_resource_files_collection_invalid(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../grinder/helloworld.py",
                                          "properties-file": __dir__() + "/../grinder/grinder_invalid.properties"}})
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 2)
        self.assertEqual(len(artifacts), 2)

        self.assertIn("helloworld.py", open(os.path.join(obj.engine.artifacts_dir,
                                                         "grinder_invalid.properties")).read())

    def test_resource_files_collection_noscript(self):
        obj = GrinderExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"properties-file": __dir__() + "/../grinder/grinder.properties"}})
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 2)
        self.assertEqual(len(artifacts), 2)
