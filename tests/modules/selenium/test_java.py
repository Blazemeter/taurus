import os
import shutil
import time
from os import listdir
from os.path import exists, join, dirname

import yaml

from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.functional import FunctionalAggregator, FuncSamplesReader
from bzt.modules.java import JUnitTester, TestNGTester
from bzt.modules.java.executors import JavaTestRunner
from bzt.modules.java.tools import JavaC, JarTool, Hamcrest, SeleniumServer
from bzt.modules.jmeter import JTLReader
from bzt.modules.selenium import SeleniumExecutor
from bzt.utils import ToolError
from tests import BZTestCase, local_paths_config, RESOURCES_DIR, BUILD_DIR, ExecutorTestCase
from tests.mocks import EngineEmul
from tests.modules.selenium import SeleniumTestCase


class TestTestNGTester(ExecutorTestCase):
    EXECUTOR = TestNGTester
    CMD_LINE = None

    def start_subprocess(self, args, env, cwd=None, **kwargs):
        self.CMD_LINE = args

    def setUp(self):
        super(TestTestNGTester, self).setUp()
        self.obj.engine.configure([local_paths_config()])
        self.obj.settings = self.obj.engine.config.get("modules").get("testng")

    def test_simple(self):
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "selenium/testng/TestNGSuite.java"}})
        self.obj.settings['autodetect-xml'] = False
        self.obj._compile_scripts = lambda: None
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.post_process()

        self.assertTrue('-cp' in self.CMD_LINE)
        cp_val = self.CMD_LINE[self.CMD_LINE.index('-cp')+1]
        for class_val in self.obj.class_path:
            self.assertIn(class_val, cp_val)

    def test_install_tools(self):
        installation_path = BUILD_DIR + "selenium-taurus"
        source_url = "file:///" + RESOURCES_DIR + "selenium/selenium-server.jar"

        shutil.rmtree(dirname(installation_path), ignore_errors=True)
        self.assertFalse(exists(installation_path))

        saved_url = JarTool.URL
        saved_local_path = JarTool.LOCAL_PATH

        JarTool.URL = source_url
        JarTool.LOCAL_PATH = join(installation_path, "{tool_file}")

        try:

            self.obj.settings.merge({
                "selenium-server": {
                    "path": join(installation_path, "selenium-server.jar"),
                    "download-link": source_url,
                    "version": "9.9"
                },
                "hamcrest-core": join(installation_path, "tools", "testng", "hamcrest-core.jar"),
                "path": JarTool.LOCAL_PATH})

            self.obj.execution.merge({
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/testng/jars/testng-suite.jar"}})

            self.obj.prepare()

        finally:
            JarTool.URL = saved_url
            JarTool.LOCAL_PATH = saved_local_path

        self.assertTrue(isinstance(self.obj, TestNGTester))

        jar_tools = [tool for tool in self.obj._tools if isinstance(tool, JarTool)]
        self.assertTrue(15, len(jar_tools))

        for tool in jar_tools:
            msg = "Wrong path to {tool}: {path}".format(tool=str(tool), path=str(tool.tool_path))
            self.assertTrue(os.path.isfile(tool.tool_path), msg)
            if isinstance(tool, SeleniumServer):
                self.assertEqual(tool.version, "9.9.0")


class TestJavaC(BZTestCase):
    def test_missed_tool(self):
        self.obj = JavaC()
        self.obj.tool_path = "javac-not-found"
        self.assertEqual(False, self.obj.check_if_installed())
        self.assertRaises(ToolError, self.obj.install)

    def test_get_version(self):
        self.obj = JavaC()
        out1 = "javac 10.0.1"
        out2 = "javac 1.8.0_151"

        self.assertEqual("10", self.obj._get_version(out1))
        self.assertEqual("8", self.obj._get_version(out2))

    def test_simple_compilation(self):
        super(TestJavaC, self).setUp()
        engine_obj = EngineEmul()

        self.obj = JUnitTester()
        self.obj.settings = engine_obj.config.get("modules").get("junit")
        self.obj.engine = engine_obj

        self.obj.execution.merge({
            "scenario": {"script": RESOURCES_DIR + "selenium/invalid/selenium1.java"}
        })

        self.obj.prepare()
        self.obj.post_process()


class TestJUnitTester(BZTestCase):
    def setUp(self):
        super(TestJUnitTester, self).setUp()
        engine_obj = EngineEmul()
        paths = [local_paths_config()]
        engine_obj.configure(paths)

        # just download geckodriver & chromedriver with selenium
        selenium = SeleniumExecutor()
        selenium.engine = engine_obj
        selenium.install_required_tools()
        for driver in selenium.webdrivers:
            selenium.env.add_path({"PATH": driver.get_driver_dir()})

        self.obj = JUnitTester()
        self.obj.env = selenium.env
        self.obj.settings = engine_obj.config.get("modules").get("junit")
        self.obj.engine = engine_obj

    def tearDown(self):
        if self.obj.stdout:
            self.obj.stdout.close()
        if self.obj.stderr:
            self.obj.stderr.close()
        super(TestJUnitTester, self).tearDown()

    def test_install_tools(self):
        """
        check installation of selenium-server, junit
        :return:
        """
        installation_path = BUILD_DIR + "selenium-taurus"
        source_url = "file:///" + RESOURCES_DIR + "selenium/selenium-server.jar"

        shutil.rmtree(dirname(installation_path), ignore_errors=True)
        self.assertFalse(exists(installation_path))

        saved_url = JarTool.URL
        saved_local_path = JarTool.LOCAL_PATH

        JarTool.URL = source_url
        JarTool.LOCAL_PATH = join(installation_path, "{tool_file}")

        try:
            self.obj.settings.merge({
                "selenium-server": join(installation_path, "selenium-server.jar"),
                "hamcrest-core": {
                    "path": join(installation_path, "tools", "junit", "hamcrest-core.jar"),
                    "version": "0.1",
                },
                "path": installation_path
            })

            self.obj.execution.merge({
                "scenario": {
                    "script": RESOURCES_DIR + "selenium/junit/jar/"},
                "runner": "junit"})
            self.obj.prepare()
        finally:
            JarTool.URL = saved_url
            JarTool.LOCAL_PATH = saved_local_path

        self.assertTrue(isinstance(self.obj, JUnitTester))

        jar_tools = [tool for tool in self.obj._tools if isinstance(tool, JarTool)]
        self.assertTrue(15, len(jar_tools))

        for tool in jar_tools:
            msg = "Wrong path to {tool}: {path}".format(tool=str(tool), path=str(tool.tool_path))
            if isinstance(tool, Hamcrest):
                self.assertEqual(tool.version, "0.1")
            self.assertTrue(os.path.isfile(tool.tool_path), msg)

    def test_simple(self):
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.execution.merge({
            "scenario": {"script": RESOURCES_DIR + "BlazeDemo.java", "properties": {"scenprop": 3}},
            "properties": {"execprop": 2}
        })
        self.obj.settings.merge({"properties": {"settprop": 1}, "junit-version": 5})
        self.obj._compile_scripts = lambda: None
        self.obj.prepare()
        self.obj.engine.aggregator.prepare()
        self.obj.engine.start_subprocess = lambda **kwargs: None
        self.obj.startup()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()

        orig_prop_file = RESOURCES_DIR + "selenium/junit/runner.properties"
        start1 = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '/')
        start2 = "ARTIFACTS+"
        self.assertFilesEqual(orig_prop_file, self.obj.props_file, replace_str=start1, replace_with=start2)

    def test_load_mode(self):
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.execution.merge({
            "iterations": 10,
            "scenario": {"script": RESOURCES_DIR + "selenium/invalid/SimpleTest.java"},
        })
        self.obj.settings.merge({"junit-version": 5})
        self.obj._compile_scripts = lambda: None
        self.obj.prepare()
        self.obj.engine.aggregator.prepare()
        self.obj.engine.start_subprocess = lambda **kwargs: None
        self.obj.startup()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()
        self.assertTrue(self.obj.report_file.endswith(".csv"))
        self.assertIsInstance(self.obj.reader, JTLReader)

    def test_func_mode(self):
        self.obj.engine.aggregator = FunctionalAggregator()
        self.obj.execution.merge({
            "iterations": 10,
            "scenario": {"script": RESOURCES_DIR + "selenium/invalid/SimpleTest.java"},
        })
        self.obj.settings.merge({"junit-version": 5})
        self.obj._compile_scripts = lambda: None
        self.obj.prepare()
        self.obj.engine.aggregator.prepare()
        self.obj.engine.start_subprocess = lambda **kwargs: None
        self.obj.startup()
        self.obj.post_process()
        self.obj.engine.aggregator.post_process()

        self.obj.reader.report_reader.json_reader.file.close()

        self.assertTrue(self.obj.report_file.endswith(".ldjson"))
        self.assertIsInstance(self.obj.reader, FuncSamplesReader)


class TestSeleniumJUnitTester(SeleniumTestCase):
    """
    :type obj: bzt.modules.selenium.SeleniumExecutor
    """

    def __init__(self, methodName='runTest'):
        super(TestSeleniumJUnitTester, self).__init__(methodName)
        self.obj = None

    def obj_prepare(self):
        super(SeleniumExecutor, self.obj).prepare()
        self.obj.install_required_tools()
        for driver in self.obj.webdrivers:
            self.obj.env.add_path({"PATH": driver.get_driver_dir()})
        self.obj.create_runner()
        self.obj.runner.install_required_tools = lambda: None
        self.obj.runner._compile_scripts = lambda: None
        self.obj.runner.prepare()
        self.obj.script = self.obj.runner.script

    def test_prepare_java_single(self):
        """
        Check if script exists in working dir
        :return:
        """
        self.obj.execution.merge({
            "scenario": {"script": RESOURCES_DIR + "selenium/junit/java/TestBlazemeterFail.java"}
        })
        self.obj_prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        self.assertFalse(exists(join(self.obj.runner.working_dir, "TestBlazemeterFail.java")))

    def test_prepare_java_folder(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "selenium/junit/java/"}})
        self.obj_prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        prepared_files = listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 0)

    def test_prepare_java_package(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "selenium/junit/java_package/"}})
        self.obj_prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)

    def test_selenium_startup_shutdown_java_package(self):
        """
        Run tests from package
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/java_package/src'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj_prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)

    def test_prepare_jar_single(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "selenium/junit/jar/dummy.jar"}})
        self.obj.prepare()

    def test_prepare_jar_folder(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "selenium/junit/jar/"}})
        self.obj.prepare()

    def test_selenium_startup_shutdown_jar_single(self):
        """
        runt tests from single jar
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/jar/'},
                'runner': 'junit',
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj_prepare()
        self.assertIsInstance(self.obj.runner, JUnitTester)
        self.obj.shutdown()

        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        prepared_files = listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 0)
        self.assertEqual(len(jars), 0)

    def test_selenium_startup_shutdown_jar_folder(self):
        """
        run tests from jars
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/jar/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj_prepare()

        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        prepared_files = listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 0)
        self.assertEqual(len(jars), 0)

    def test_selenium_startup_shutdown_java_single(self):
        """
        run tests from single .java file
        :return:
        """
        self.obj.engine.config.merge({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.obj.execution.merge({
            "scenario": {"script": RESOURCES_DIR + "selenium/junit/java/TestBlazemeterFail.java"}
        })
        self.obj_prepare()
        self.obj.shutdown()

        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        prepared_files = listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(0, len(java_files))

    def test_selenium_startup_shutdown_java_folder(self):
        """
        run tests from .java files
        :return:
        """
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })

        self.obj_prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        prepared_files = listdir(self.obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(0, len(java_files))

    def test_resource_files_collection_remote_java(self):
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.assertEqual(len(self.obj.resource_files()), 1)

    def test_additional_classpath(self):
        scenario_cp = 'class_path_from_scenario'
        settings_cp = 'class_path_from_settings'
        self.configure({
            'execution': {
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/junit/java/',
                    'additional-classpath': [scenario_cp]},
                'executor': 'selenium', },
            'modules': {
                'junit': {
                    'additional-classpath': [settings_cp]}}})
        self.obj_prepare()
        self.assertIsInstance(self.obj.runner, JavaTestRunner)
        class_path = ':'.join(self.obj.runner.class_path)
        self.assertIn(scenario_cp, class_path)
        self.assertIn(settings_cp, class_path)

    def test_resource_files_collection_remote_jar(self):
        self.configure({
            'execution': {
                'scenario': {'script': RESOURCES_DIR + 'selenium/junit/jar/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        self.assertEqual(len(self.obj.resource_files()), 1)


class TestSeleniumTestNGRunner(SeleniumTestCase):
    CMD_LINE = None

    def start_subprocess(self, args, env, cwd=None, **kwargs):
        self.CMD_LINE = args

    def obj_prepare(self):
        super(SeleniumExecutor, self.obj).prepare()
        self.obj.install_required_tools()
        for driver in self.obj.webdrivers:
            self.obj.env.add_path({"PATH": driver.get_driver_dir()})
        self.obj.create_runner()
        self.obj.runner.install_required_tools = lambda: None
        self.obj.runner._compile_scripts = lambda: None
        self.obj.runner.prepare()
        self.obj.script = self.obj.runner.script

    def test_prepare_java_package(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'},
                'runner': 'testng'},
            'modules': {
                'testng': {
                    'autodetect-xml': False}}})
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.post_process()

        self.assertTrue('-cp' in self.CMD_LINE)
        cp_val = self.CMD_LINE[self.CMD_LINE.index('-cp')+1]
        for class_val in self.obj.runner.class_path:
            self.assertIn(class_val, cp_val)

    def test_prepare_java_file(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/TestNGSuite.java'},
                'runner': 'testng'},
            'modules': {
                'testng': {
                    'autodetect-xml': False
                }}})
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.post_process()

        self.assertTrue('-cp' in self.CMD_LINE)
        cp_val = self.CMD_LINE[self.CMD_LINE.index('-cp')+1]
        for class_val in self.obj.runner.class_path:
            self.assertIn(class_val, cp_val)

    def test_resource_files(self):
        script_jar = RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'
        self.configure({
            'execution': {
                'testng-xml': 'testng.xml',
                'scenario': {
                    'script': script_jar,
                },
                'runner': 'testng',
            },
        })
        resources = self.obj.get_resource_files()
        self.assertPathsEqual(resources, [script_jar, 'testng.xml'])

    def test_resource_files_detect_config(self):
        script_jar = RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'
        self.configure({
            'execution': {
                'scenario': {
                    'script': script_jar,
                },
                'runner': 'testng',
            },
        })
        resources = self.obj.get_resource_files()
        self.assertPathsEqual(resources, [script_jar, (RESOURCES_DIR + 'selenium/testng/jars/testng.xml')])

    def test_hold(self):
        self.configure({
            'execution': {
                'hold-for': '1s',
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'},
                'runner': 'testng'},
            'modules': {
                'testng': {
                    'autodetect-xml': False}}})
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.post_process()

        self.assertIsNotNone(self.CMD_LINE)
        props = self.CMD_LINE[-1]
        self.assertTrue(props.endswith("runner.properties"))
        properties = open(props).readlines()
        self.assertIn("hold_for=1.0\n", properties)

    def test_iterations(self):
        self.configure({
            'execution': {
                'iterations': 3,
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'},
                'runner': 'testng'},
            'modules': {
                'testng': {
                    'autodetect-xml': False}}})
        self.obj.prepare()
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.post_process()

        self.assertIsNotNone(self.CMD_LINE)
        props = self.CMD_LINE[-1]
        self.assertTrue(props.endswith("runner.properties"))
        properties = open(props).readlines()
        self.assertIn("iterations=3\n", properties)

    def test_with_testng_config(self):
        self.configure({
            'execution': {
                'testng-xml': RESOURCES_DIR + 'selenium/testng/jars/testng.xml',
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'}}})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.assertTrue(exists(self.obj.runner.report_file))
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 6)

    def test_testng_config_autodetect(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar'}}})
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.assertTrue(exists(self.obj.runner.report_file))
        lines = open(self.obj.runner.report_file).readlines()
        self.assertEqual(len(lines), 6)

    def test_autodetect_script_type(self):
        self.configure({
            'execution': {
                'scenario': {
                    'script': RESOURCES_DIR + 'selenium/testng/jars/testng-suite.jar',
                },
            },
        })
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, TestNGTester)

    def test_detect_testng_xml_with_config(self):
        test_yml = RESOURCES_DIR + "selenium/testng/test.yml"
        self.obj.engine.config.merge(yaml.load(open(test_yml)))
        self.obj.execution = self.obj.engine.config.get('execution')
        self.obj.engine.file_search_paths.append(dirname(test_yml))
        self.obj.prepare()
        self.assertIsInstance(self.obj.runner, TestNGTester)
