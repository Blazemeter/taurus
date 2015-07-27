from tests import setup_test_logging, BZTestCase, local_paths_config
from tests.mocks import EngineEmul
import tempfile

class TestShellExec(BZTestCase):
    def setUp(self):
        super(TestShellExec, self).setUp()
        self.obj = EngineEmul()
        self.obj.artifacts_base_dir = tempfile.gettempdir() + "/bzt"
        self.paths = local_paths_config()
    def test_create_shell(self):
        self.obj = EngineEmul()
