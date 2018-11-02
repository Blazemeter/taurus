import codecs
import logging
import os
import re
import shutil

from bzt import TaurusException
from tests import BZTestCase, RESOURCES_DIR

from bzt.cli import CLI, ConfigOverrider, get_option_parser
from bzt.engine import Configuration
from tests.mocks import EngineEmul, ModuleMock


class TestCLI(BZTestCase):
    def setUp(self):
        super(TestCLI, self).setUp()
        self.logger = self.log
        self.log = os.path.join(os.path.dirname(__file__), "..", "build", "bzt.log")
        self.verbose = False
        self.quiet = False
        self.no_system_configs = True
        self.option = []
        self.datadir = os.path.join(os.path.dirname(__file__), "..", "build", "acli")
        self.obj = CLI(self)
        self.assertTrue(os.path.exists(self.log))

        self.aliases = []
        self.obj.engine = EngineEmul()

    def get_ret_code(self, configs):
        self.obj.engine.config.get("settings", force_set=True).get("default-executor", "mock", force_set=True)
        return self.obj.perform(configs)

    def tearDown(self):
        self.obj.close_log()
        self.log = self.logger
        super(BZTestCase, self).tearDown()

    def test_perform_normal(self):
        ret = self.get_ret_code([RESOURCES_DIR + "json/mock_normal.json"])
        self.assertEquals(0, ret)

    def test_call_proc_error(self):
        ret = self.get_ret_code([RESOURCES_DIR + "yaml/wrong_cmd.yml"])
        self.assertEquals(1, ret)
        bad_err = "__init__() missing 1 required positional argument: 'cmd'"
        good_err = "DEBUG EngineEmul] Command 'wrong_cmd' returned non-zero exit status 127"
        log_file = os.path.join(self.obj.engine.artifacts_dir, "bzt.log")
        log_content = codecs.open(log_file, encoding="utf-8").read()
        self.assertNotIn(bad_err, log_content)
        self.assertIn(good_err, log_content)

    def test_unicode_logging(self):
        """ check whether unicode symbols are logged correctly into file """
        self.verbose = False
        u_symbol = b'\xe3\x81\xbc'.decode(encoding='utf-8')  # U+307C, uniform for py2/3
        self.obj.options.option = ['bo=%s' % u_symbol]

        ret = self.get_ret_code([RESOURCES_DIR + "json/mock_normal.json"])
        self.assertEqual(0, ret)
        log_file = os.path.join(self.obj.engine.artifacts_dir, "bzt.log")
        log_content = codecs.open(log_file, encoding="utf-8").read()
        self.assertIn(u_symbol, log_content)

    def test_perform_aliases(self):
        self.aliases = ['test4']
        ret = self.get_ret_code([RESOURCES_DIR + "json/mock_normal.json"])
        self.assertEquals(0, ret)
        self.assertEqual("mock4", self.obj.engine.config.get("services")[0]["module"])

    def test_perform_prepare_exc(self):
        self.obj.engine.prepare_exc = TaurusException()
        ret = self.get_ret_code([RESOURCES_DIR + "json/mock_normal.json"])
        self.assertEquals(1, ret)

    def test_perform_overrides(self):
        self.option.append("test.subkey5.-1=value")
        self.option.append("modules.mock=" + ModuleMock.__module__ + "." + ModuleMock.__name__)
        self.option.append("provisioning=mock")
        self.option.append("settings.artifacts-dir=build/test/%Y-%m-%d_%H-%M-%S.%f")
        self.option.append("test.subkey2.0.sskey=value")
        self.option.append("test.subkey.0=value")
        self.option.append("execution.-1.option=value")
        ret = self.get_ret_code([])
        self.assertEquals(0, ret)

    def test_perform_overrides_fail(self):
        self.option.append("test.subkey2.0.sskey=value")
        self.option.append("test.subkey.0=value")
        ret = self.get_ret_code([RESOURCES_DIR + "json/mock_normal.json"])
        self.assertEquals(1, ret)

    def test_perform_prepare_err(self):
        ret = self.get_ret_code([RESOURCES_DIR + "json/mock_prepare_err.json"])
        self.assertEquals(1, ret)

        prov = self.obj.engine.provisioning

        self.assertTrue(prov.was_prepare)
        self.assertFalse(prov.was_startup)
        self.assertFalse(prov.was_check)
        self.assertFalse(prov.was_shutdown)
        self.assertTrue(prov.was_postproc)

    def test_perform_start_err(self):
        conf = RESOURCES_DIR + "json/mock_start_err.json"
        self.assertEquals(1, self.get_ret_code([conf]))

        prov = self.obj.engine.provisioning
        self.assertTrue(prov.was_prepare)
        self.assertTrue(prov.was_startup)
        self.assertFalse(prov.was_check)
        self.assertTrue(prov.was_shutdown)
        self.assertTrue(prov.was_postproc)

    def test_perform_wait_err(self):
        conf = RESOURCES_DIR + "json/mock_wait_err.json"
        self.assertEquals(1, self.get_ret_code([conf]))

        prov = self.obj.engine.provisioning
        self.assertTrue(prov.was_prepare)
        self.assertTrue(prov.was_startup)
        self.assertTrue(prov.was_check)
        self.assertTrue(prov.was_shutdown)
        self.assertTrue(prov.was_postproc)

    def test_perform_end_err(self):
        conf = RESOURCES_DIR + "json/mock_end_err.json"
        self.assertEquals(1, self.get_ret_code([conf]))

        prov = self.obj.engine.provisioning
        self.assertTrue(prov.was_prepare)
        self.assertTrue(prov.was_startup)
        self.assertTrue(prov.was_check)
        self.assertTrue(prov.was_shutdown)
        self.assertTrue(prov.was_postproc)

    def test_perform_postproc_err(self):
        conf = RESOURCES_DIR + "json/mock_postproc_err.json"
        self.assertEquals(3, self.get_ret_code([conf]))

        prov = self.obj.engine.provisioning
        self.assertTrue(prov.was_prepare)
        self.assertTrue(prov.was_startup)
        self.assertTrue(prov.was_check)
        self.assertTrue(prov.was_shutdown)
        self.assertTrue(prov.was_postproc)

    def test_jmx_shorthand(self):
        json_config = RESOURCES_DIR + "json/mock_normal.json"
        jmx1 = RESOURCES_DIR + "jmeter/jmx/dummy.jmx"
        jmx2 = RESOURCES_DIR + "jmeter/jmx/http.jmx"

        ret = self.get_ret_code([json_config, jmx1, jmx2])

        executions = self.obj.engine.config.get('execution', [])
        scenarios = [execution.get('scenario', {}) for execution in executions]
        scripts = set([scenario.get('script', None) for scenario in scenarios])

        self.assertEquals(0, ret)
        self.assertIn(jmx1, scripts)
        self.assertIn(jmx2, scripts)

    def test_override_artifacts_dir(self):
        # because EngineEmul sets up its own artifacts_dir
        self.obj.engine.artifacts_dir = None
        artifacts_dir = "build/tmp-test-artifacts"

        self.option.append("modules.mock=" + ModuleMock.__module__ + "." + ModuleMock.__name__)
        self.option.append("provisioning=mock")
        self.option.append("settings.artifacts-dir=%s" % artifacts_dir)
        try:
            ret = self.get_ret_code([])
            self.assertEquals(0, ret)
            self.assertTrue(os.path.exists(artifacts_dir))
        finally:
            # cleanup artifacts dir
            if os.path.exists(artifacts_dir):
                shutil.rmtree(artifacts_dir)

    def test_logging_verbosity_adjustment(self):
        self.verbose = False
        ret = self.get_ret_code([
            RESOURCES_DIR + "json/mock_normal.json",
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

    def test_cover_option_parser(self):
        parser = get_option_parser()
        parser.print_usage()

    def test_http_shorthand(self):
        self.option.append("modules.mock=" + ModuleMock.__module__ + "." + ModuleMock.__name__)
        self.option.append("provisioning=mock")
        self.option.append("settings.default-executor=mock")
        code = self.get_ret_code(["http://blazedemo.com/"])
        self.assertEqual(code, 0)
        log_content = open(os.path.join(self.obj.engine.artifacts_dir, "bzt.log")).read()
        configs = re.findall(r'[^\s\']*http_.*\.yml', log_content)
        self.assertGreater(len(configs), 0)

    def test_normal(self):
        self.option.append("cli.linter.lint-and-exit=true")
        self.obj.engine.config.merge({"execution": [{"concurrency": 10, "scenario": {"script": "foo.jmx"}}]})
        ret = self.get_ret_code([])
        self.assertEquals(0, ret)

    def test_normal_error(self):
        self.option.append("cli.linter.lint-and-exit=true")
        self.obj.engine.config.merge({"execution": {"concurrency": 10, "scenarion": {"script": "foo.jmx"}}})
        ret = self.get_ret_code([])
        self.assertEquals(1, ret)

    def test_ignore(self):
        self.option.append("cli.linter.lint-and-exit=true")
        self.option.append("cli.linter.ignored-warnings.0=single-execution")
        self.obj.engine.config.merge({"execution": {"concurrency": 10, "scenario": {"script": "foo.jmx"}}})
        ret = self.get_ret_code([])
        self.assertEquals(0, ret)


class TestConfigOverrider(BZTestCase):
    def setUp(self):
        super(TestConfigOverrider, self).setUp()
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
