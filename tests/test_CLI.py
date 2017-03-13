import logging
import os
import shutil

from bzt.cli import CLI, ConfigOverrider
from bzt.engine import Configuration
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

    def test_perform_start_err(self):
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

    def test_override_artifacts_dir(self):
        # because EngineEmul sets up its own artifacts_dir
        self.obj.engine.artifacts_dir = None
        artifacts_dir = "build/tmp-test-artifacts"

        self.option.append("modules.mock=" + ModuleMock.__module__ + "." + ModuleMock.__name__)
        self.option.append("provisioning=mock")
        self.option.append("settings.artifacts-dir=%s" % artifacts_dir)
        try:
            ret = self.obj.perform([])
            self.assertEquals(0, ret)
            self.assertTrue(os.path.exists(artifacts_dir))
        finally:
            # cleanup artifacts dir
            if os.path.exists(artifacts_dir):
                shutil.rmtree(artifacts_dir)

    def test_logging_verbosity_adjustment(self):
        was_verbose = self.verbose
        try:
            self.verbose = False
            ret = self.obj.perform([
                __dir__() + "/json/mock_normal.json",
            ])
            self.assertEquals(0, ret)
            log_lines = open(os.path.join(self.obj.engine.artifacts_dir, "bzt.log")).readlines()
            checking = False
            found_line = False
            for line in log_lines:
                if "Leveling down" in line:
                    found_line = True
                    checking = True
                elif "Leveled up" in line:
                    checking = False
                else:
                    if checking:
                        self.assertNotIn("DEBUG", line)
            self.assertTrue(found_line)
        finally:
            self.verbose = was_verbose


class TestConfigOverrider(BZTestCase):
    def setUp(self):
        self.obj = ConfigOverrider(logging.getLogger())
        self.config = Configuration()

    def test_numbers(self):
        self.obj.apply_overrides(["int=11", "float=3.14"], self.config)
        self.assertEqual(self.config.get("int"), int(11))
        self.assertEqual(self.config.get("float"), float(3.14))

    def test_booleans(self):
        self.obj.apply_overrides(["yes=true", "no=false"], self.config)
        self.assertEqual(self.config.get("yes"), bool(True))
        self.assertEqual(self.config.get("no"), bool(False))

    def test_strings(self):
        self.obj.apply_overrides(["plain=ima plain string",
                                  """quoted='"ima quoted string"'""",
                                  """empty-quoted='""'""",
                                  '''escaped="a "b" 'c' d"''',
                                  '''escaped-quoted="a "b" 'c' d"'''], self.config)
        self.assertEqual(self.config.get("plain"), str("ima plain string"))
        self.assertEqual(self.config.get("quoted"), str('''"ima quoted string"'''))
        self.assertEqual(self.config.get("empty-quoted"), str('""'))
        self.assertEqual(self.config.get("escaped"), str('''"a "b" 'c' d"'''))
        self.assertEqual(self.config.get("escaped-quoted"), str('''"a "b" 'c' d"'''))

    def test_strings_literals_clash(self):
        # we want to pass literal string 'true' (and not have it converted to bool(True))
        self.obj.apply_overrides(['yes="true"',
                                  'list="[1,2,3]"'], self.config)
        self.assertEqual(self.config.get("yes"), str("true"))
        self.assertEqual(self.config.get("list"), str("[1,2,3]"))

    def test_null(self):
        self.obj.apply_overrides(['nothing=null'], self.config)
        self.assertEqual(self.config.get("nothing"), None)

    def test_objects(self):
        self.config.merge({
            "obj": {
                "key": "142857",
            },
        })
        self.obj.apply_overrides(['obj={"key": "value"}'], self.config)
        self.assertEqual(self.config.get("obj").get("key"), str("value"))

    def test_lists(self):
        self.config.merge({
            "list": ["stuff"],
        })
        self.obj.apply_overrides(['list=[1, 2.0, "str", []]'], self.config)
        self.assertEqual(self.config.get("list"), list([1, 2.0, "str", []]))

    def test_nested_quotation(self):
        # bzt -o man='{"name": "Robert \"Destroyer of Worlds\" Oppenheimer"}'
        self.obj.apply_overrides(['''man={"name": "Robert \\"Destroyer of Worlds\\" Oppenheimer"}'''], self.config)
        self.assertEqual(self.config.get("man").get("name"), str('Robert "Destroyer of Worlds" Oppenheimer'))

    def test_no_override(self):
        self.obj.apply_overrides(['nothing='], self.config)
        self.assertEqual(self.config.get("nothing"), None)

    def test_unquoted_keys(self):
        self.obj.apply_overrides(['obj={abc: def}'], self.config)
        self.assertEqual(self.config.get("obj").get("abc"), str("def"))

    def test_override_delete_from_list(self):
        self.config["items"] = [1, 2, 3]
        self.config["dict"] = {"1": 1, "2": 2, "3": 3}
        self.obj.apply_overrides(['items.^-4=null'], self.config)
        self.obj.apply_overrides(['items.^1=null'], self.config)
        self.obj.apply_overrides(['dict.^2=null'], self.config)
        self.assertEqual(self.config.get("items"), [1, 3])
        self.assertEqual(self.config.get("dict"), {"1": 1, "3": 3})
