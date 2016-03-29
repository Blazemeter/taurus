""" test """
import os

from bzt.cli import CLI
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul, ModuleMock


class TestCLI(BZTestCase):
    def setUp(self):
        super(TestCLI, self).setUp()
        self.log = os.path.join(os.path.dirname(__file__), "..", "build", "bzt.log")
        self.verbose = True
        self.no_system_configs = True
        self.option = []
        self.datadir = os.path.join(os.path.dirname(__file__), "..", "build", "acli")
        self.obj = CLI(self)
        self.aliases = []
        self.obj.engine = EngineEmul()

    def test_perform_normal(self):
        ret = self.obj.perform([__dir__() + "/json/mock_normal.json"])
        self.assertEquals(0, ret)

    def test_perform_overrides(self):
        self.option.append("test.subkey5.-1=value")
        self.option.append("modules.mock=" + ModuleMock.__module__ + "." + ModuleMock.__name__)
        self.option.append("provisioning=mock")
        self.option.append("settings.artifacts-dir=build/test/%Y-%m-%d_%H-%M-%S.%f")
        self.option.append("test.subkey2.0.sskey=value")
        self.option.append("test.subkey.0=value")
        self.option.append("execution.-1.option=value")
        ret = self.obj.perform([])
        self.assertEquals(0, ret)

    def test_perform_overrides_fail(self):
        self.option.append("test.subkey2.0.sskey=value")
        self.option.append("test.subkey.0=value")
        ret = self.obj.perform([__dir__() + "/json/mock_normal.json"])
        self.assertEquals(1, ret)

    def test_perform_prepare_err(self):
        ret = self.obj.perform([__dir__() + "/json/mock_prepare_err.json"])
        self.assertEquals(1, ret)

        prov = self.obj.engine.provisioning

        self.assertTrue(prov.was_prepare)
        self.assertFalse(prov.was_startup)
        self.assertFalse(prov.was_check)
        self.assertFalse(prov.was_shutdown)
        self.assertTrue(prov.was_postproc)

    def test_aperform_start_err(self):
        conf = __dir__() + "/json/mock_start_err.json"
        self.assertEquals(1, self.obj.perform([conf]))

        prov = self.obj.engine.provisioning
        self.assertTrue(prov.was_prepare)
        self.assertTrue(prov.was_startup)
        self.assertFalse(prov.was_check)
        self.assertTrue(prov.was_shutdown)
        self.assertTrue(prov.was_postproc)

    def test_perform_wait_err(self):
        conf = __dir__() + "/json/mock_wait_err.json"
        self.assertEquals(1, self.obj.perform([conf]))

        prov = self.obj.engine.provisioning
        self.assertTrue(prov.was_prepare)
        self.assertTrue(prov.was_startup)
        self.assertTrue(prov.was_check)
        self.assertTrue(prov.was_shutdown)
        self.assertTrue(prov.was_postproc)

    def test_perform_end_err(self):
        conf = __dir__() + "/json/mock_end_err.json"
        self.assertEquals(1, self.obj.perform([conf]))

        prov = self.obj.engine.provisioning
        self.assertTrue(prov.was_prepare)
        self.assertTrue(prov.was_startup)
        self.assertTrue(prov.was_check)
        self.assertTrue(prov.was_shutdown)
        self.assertTrue(prov.was_postproc)

    def test_perform_postproc_err(self):
        conf = __dir__() + "/json/mock_postproc_err.json"
        self.assertEquals(3, self.obj.perform([conf]))

        prov = self.obj.engine.provisioning
        self.assertTrue(prov.was_prepare)
        self.assertTrue(prov.was_startup)
        self.assertTrue(prov.was_check)
        self.assertTrue(prov.was_shutdown)
        self.assertTrue(prov.was_postproc)

    def test_jmx_shorthand(self):
        ret = self.obj.perform([
            __dir__() + "/json/mock_normal.json",
            __dir__() + "/jmx/dummy.jmx",
            __dir__() + "/jmx/dummy.jmx",
        ])
        self.assertEquals(0, ret)
