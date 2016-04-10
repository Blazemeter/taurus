import os
import shutil
import time

import yaml

from bzt.engine import ScenarioExecutor, Provisioning
from bzt.modules.selenium import SeleniumExecutor, JUnitJar
from tests import BZTestCase, local_paths_config, __dir__
from tests.mocks import EngineEmul


class SeleniumTestCase(BZTestCase):
    def setUp(self):
        super(SeleniumTestCase, self).setUp()
        self.engine_obj = EngineEmul()
        self.paths = [__dir__() + "/../../bzt/10-base.json", local_paths_config()]
        self.engine_obj.configure(self.paths)  # FIXME: avoid using whole engine in particular module test!
        self.engine_obj.config.get("modules").get("selenium").merge({"virtual-display": {"width": 1024, "height": 768}})
        self.selenium_config = self.engine_obj.config["modules"]["selenium"]
        self.engine_obj.create_artifacts_dir(self.paths)


class TestSeleniumJUnitRunner(SeleniumTestCase):
    """
    java:one/folder/project/list
    jar:one/folder/list
    python:one/folder/list
    """

    def setUp(self):
        super(TestSeleniumJUnitRunner, self).setUp()

    def test_install_tools(self):
        """
        check installation of selenium-server, junit
        :return:
        """
        dummy_installation_path = __dir__() + "/../../build/tmp/selenium-taurus"
        base_link = "file:///" + __dir__() + "/../data/"

        shutil.rmtree(os.path.dirname(dummy_installation_path), ignore_errors=True)

        selenium_server_link = SeleniumExecutor.SELENIUM_DOWNLOAD_LINK
        SeleniumExecutor.SELENIUM_DOWNLOAD_LINK = base_link + "/selenium-server-standalone-2.46.0.jar"

        junit_link = SeleniumExecutor.JUNIT_DOWNLOAD_LINK
        junit_mirrors = SeleniumExecutor.JUNIT_MIRRORS_SOURCE
        SeleniumExecutor.JUNIT_DOWNLOAD_LINK = base_link + "/junit-4.12.jar"
        SeleniumExecutor.JUNIT_MIRRORS_SOURCE = base_link + "unicode_file"

        hamcrest_link = SeleniumExecutor.HAMCREST_DOWNLOAD_LINK
        SeleniumExecutor.HAMCREST_DOWNLOAD_LINK = base_link + "/hamcrest-core-1.3.jar"

        self.assertFalse(os.path.exists(dummy_installation_path))

        obj = self.get_selenium_executor()
        obj.settings.merge({"selenium-tools": {
            "junit": {"selenium-server": os.path.join(dummy_installation_path, "selenium-server.jar")}
        }})
        obj.settings.merge({"selenium-tools": {
            "junit": {"hamcrest-core": os.path.join(dummy_installation_path, "tools", "junit", "hamcrest-core.jar")}
        }})
        obj.settings.merge({"selenium-tools": {
            "junit": {"path": os.path.join(dummy_installation_path, "tools", "junit", "junit.jar")}
        }})

        obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/jar/"}})
        obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "selenium-server.jar")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "tools", "junit", "junit.jar")))
        self.assertTrue(os.path.exists(os.path.join(dummy_installation_path, "tools", "junit", "hamcrest-core.jar")))
        SeleniumExecutor.SELENIUM_DOWNLOAD_LINK = selenium_server_link
        SeleniumExecutor.JUNIT_DOWNLOAD_LINK = junit_link
        SeleniumExecutor.HAMCREST_DOWNLOAD_LINK = hamcrest_link
        SeleniumExecutor.JUNIT_MIRRORS_SOURCE = junit_mirrors

    def get_selenium_executor(self):
        obj = SeleniumExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.selenium_config
        return obj

    def test_prepare_java_single(self):
        """
        Check if script exists in working dir
        :return:
        """
        obj = self.get_selenium_executor()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/java/TestBlazemeterFail.java"}})
        obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "TestBlazemeterFail.java")))
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "TestBlazemeterFail.class")))
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "compiled.jar")))

    def test_prepare_java_folder(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        obj = self.get_selenium_executor()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/java/"}})
        obj.prepare()
        prepared_files = os.listdir(obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 2)
        self.assertEqual(len(class_files), 2)
        self.assertEqual(len(jars), 1)

    def test_prepare_java_package(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        obj = self.get_selenium_executor()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/java_package/"}})
        obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "compiled.jar")))

    def test_selenium_startup_shutdown_java_package(self):
        """
        Run tests from package
        :return:
        """
        obj = self.get_selenium_executor()
        obj.engine.config.merge({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/java_package/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "compiled.jar")))

    def test_prepare_jar_single(self):
        """
        Check if jar exists in working dir
        :return:
        """
        obj = self.get_selenium_executor()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/jar/dummy.jar"}})
        obj.prepare()
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "dummy.jar")))

    def test_prepare_jar_folder(self):
        """
        Check if jars exist in working dir
        :return:
        """
        obj = self.get_selenium_executor()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/jar/"}})
        obj.prepare()
        java_scripts = os.listdir(obj.runner.working_dir)
        self.assertEqual(len(java_scripts), 2)

    def test_selenium_startup_shutdown_jar_single(self):
        """
        runt tests from single jar
        :return:
        """
        obj = self.get_selenium_executor()
        obj.engine.config.merge({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/jar/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/jar/dummy.jar"}})
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        prepared_files = os.listdir(obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 0)
        self.assertEqual(len(jars), 1)
        self.assertTrue(os.path.exists(obj.runner.settings.get("report-file")))

    def test_selenium_startup_shutdown_jar_folder(self):
        """
        run tests from jars
        :return:
        """
        obj = self.get_selenium_executor()
        obj.engine.config.merge({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/jar/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        prepared_files = os.listdir(obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(len(java_files), 0)
        self.assertEqual(len(class_files), 0)
        self.assertEqual(len(jars), 2)
        self.assertTrue(os.path.exists(obj.runner.settings.get("report-file")))

    def test_selenium_startup_shutdown_java_single(self):
        """
        run tests from single .java file
        :return:
        """
        obj = self.get_selenium_executor()
        obj.engine.config.merge({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/java/TestBlazemeterFail.java"}})
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        prepared_files = os.listdir(obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(1, len(java_files))
        self.assertEqual(1, len(class_files))
        self.assertEqual(1, len(jars))
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "compiled.jar")))
        self.assertTrue(os.path.exists(obj.runner.settings.get("report-file")))

    def test_selenium_startup_shutdown_java_folder(self):
        """
        run tests from .java files
        :return:
        """
        obj = self.get_selenium_executor()
        obj.engine.config.merge({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })

        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

        prepared_files = os.listdir(obj.runner.working_dir)
        java_files = [fname for fname in prepared_files if fname.endswith(".java")]
        class_files = [fname for fname in prepared_files if fname.endswith(".class")]
        jars = [fname for fname in prepared_files if fname.endswith(".jar")]
        self.assertEqual(2, len(java_files))
        self.assertEqual(2, len(class_files))
        self.assertEqual(1, len(jars))
        self.assertTrue(os.path.exists(os.path.join(obj.runner.working_dir, "compiled.jar")))
        self.assertTrue(os.path.exists(obj.runner.settings.get("report-file")))

    def test_not_junit(self):
        """
        Check that JUnit runner fails if no tests were found
        :return:
        """
        obj = self.get_selenium_executor()
        obj.engine.config.merge({
            Provisioning.PROV: "local",
            ScenarioExecutor.EXEC: {
                "executor": "selenium",
                "scenario": {"script": __dir__() + "/../selenium/invalid/NotJUnittest.java"}
            }
        })
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        obj.startup()
        try:
            while not obj.check():
                time.sleep(1)
            self.fail()
        except BaseException as exc:
            self.assertIn("Nothing to test", exc.args[0])
        obj.shutdown()

    def test_resource_files_collection_remote_java(self):
        obj = self.get_selenium_executor()
        obj.engine.config.merge({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/java/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))

        self.assertEqual(len(obj.resource_files()), 2)

    def test_resource_files_collection_remote_jar(self):
        obj = self.get_selenium_executor()
        obj.engine.config.merge({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/jar/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))

        self.assertEqual(len(obj.resource_files()), 2)


class TestSeleniumNoseRunner(BZTestCase):
    def test_selenium_prepare_python_single(self):
        """
        Check if script exists in working dir
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {
            "script": __dir__() + "/../selenium/python/test_blazemeter_fail.py"
        }})
        obj.prepare()
        python_scripts = os.listdir(obj.runner.working_dir)
        self.assertEqual(len(python_scripts), 1)

    def test_selenium_prepare_python_folder(self):
        """
        Check if scripts exist in working dir
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/python/"}})
        obj.prepare()
        python_scripts = os.listdir(obj.runner.working_dir)
        self.assertEqual(len(python_scripts), 2)

    def test_selenium_startup_shutdown_python_single(self):
        """
        run tests from .py file
        :return:
        """

        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/python/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']

        obj.execution.merge({"scenario": {
            "script": __dir__() + "/../selenium/python/test_blazemeter_fail.py"
        }})

        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        prepared_files = os.listdir(obj.runner.working_dir)
        python_files = [fname for fname in prepared_files if fname.endswith(".py")]
        self.assertEqual(1, len(python_files))
        self.assertTrue(os.path.exists(obj.runner.settings.get("report-file")))

    def test_selenium_startup_shutdown_python_folder(self):
        """
        run tests from .py files
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            'execution': {
                'scenario': {'script': __dir__() + '/../selenium/python/'},
                'executor': 'selenium'
            },
            'reporting': [{'module': 'junit-xml'}]
        })
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        prepared_files = os.listdir(obj.runner.working_dir)
        python_files = [fname for fname in prepared_files if fname.endswith(".py")]
        self.assertEqual(2, len(python_files))
        self.assertTrue(os.path.exists(obj.runner.settings.get("report-file")))

    def runner_fail_no_test_found(self):
        """
        Check that Python Nose runner fails if no tests were found
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "selenium",
                "scenario": {"script": __dir__() + "/../selenium/invalid/dummy.py"}
            }
        })
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        obj.startup()
        try:
            while not obj.check():
                time.sleep(1)
            self.fail()
        except RuntimeError as exc:
            self.assertIn("Nothing to test.", exc.args[0])
        obj.shutdown()

    def test_resource_files_collection_remote_nose(self):
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../selenium/python/"}})
        obj.settings.merge(obj.engine.config.get("modules").get("selenium"))

        self.assertEqual(len(obj.resource_files()), 2)


class TestSeleniumStuff(SeleniumTestCase):
    def test_empty_scenario(self):
        """
        Raise runtime error when no scenario provided
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge({ScenarioExecutor.EXEC: {"executor": "selenium"}})
        obj.execution = obj.engine.config['execution']
        self.assertRaises(ValueError, obj.prepare)

    def test_javac_fail(self):
        """
        Test RuntimeError when compilation fails
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.selenium_config
        obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "selenium",
                "scenario": {"script": __dir__() + "/../selenium/invalid/invalid.java"}
            }
        })
        obj.execution = obj.engine.config['execution']
        self.assertRaises(RuntimeError, obj.prepare)

    def test_no_supported_files_to_test(self):
        """
        Test RuntimeError raised when no files of known types were found.
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.config.merge({ScenarioExecutor.EXEC: {
            "executor": "selenium",
            "scenario": {"script": __dir__() + "/../selenium/invalid/not_found"}
        }})
        obj.execution = obj.engine.config['execution']
        self.assertRaises(ValueError, obj.prepare)

    def test_samples_count_annotations(self):
        """
        Test exact number of tests when java annotations used
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.selenium_config
        obj.engine.config.merge({ScenarioExecutor.EXEC: {
            "executor": "selenium",
            "scenario": {"script": __dir__() + "/../selenium/invalid/SeleniumTest.java"}
        }})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

    def test_samples_count_testcase(self):
        """
        Test exact number of tests when test class extends JUnit TestCase
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.selenium_config
        obj.engine.config.merge({ScenarioExecutor.EXEC: {
            "executor": "selenium",
            "scenario": {"script": __dir__() + "/../selenium/invalid/SimpleTest.java"}
        }})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

    def test_no_test_in_name(self):
        """
        Test exact number of tests when annotations used and no "test" in class name
        :return:
        """
        obj = SeleniumExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.selenium_config
        obj.engine.config.merge({ScenarioExecutor.EXEC: {
            "executor": "selenium",
            "scenario": {"script": __dir__() + "/../selenium/invalid/selenium1.java"}
        }})
        obj.execution = obj.engine.config['execution']
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()

    def test_requests(self):
        obj = SeleniumExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.selenium_config
        obj.engine.config.merge(yaml.load(open(__dir__() + "/../yaml/selenium_executor_requests.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']

        obj.prepare()
        obj.get_widget()
        obj.startup()
        while not obj.check():
            time.sleep(1)
        obj.shutdown()
        with open(os.path.join(obj.engine.artifacts_dir, "junit.err")) as fds:
            contents = fds.read()
            self.assertEqual(1, contents.count("ok"), "file: '%s', size: %s, content: '%s'" % (fds, fds.__sizeof__(),
                             contents))
            self.assertEqual(1, contents.count("OK"))

    def test_fail_on_zero_results(self):
        obj = SeleniumExecutor()
        obj.engine = self.engine_obj
        obj.settings = self.selenium_config
        obj.engine.config.merge(yaml.load(open(__dir__() + "/../yaml/selenium_executor_requests.yml").read()))
        obj.engine.config.merge({"provisioning": "local"})
        obj.execution = obj.engine.config['execution']

        obj.prepare()
        self.assertRaises(RuntimeWarning, obj.post_process)

    def test_junit_mirrors(self):
        dummy_installation_path = __dir__() + "/../../../build/tmp/selenium-taurus"
        shutil.rmtree(os.path.dirname(dummy_installation_path), ignore_errors=True)
        obj = SeleniumExecutor()
        objjm = JUnitJar(os.path.join(dummy_installation_path, "tools", "junit", "junit.jar"), obj.log,
                         SeleniumExecutor.JUNIT_VERSION)
        objjm.install()

    def test_remote_prov_requests(self):
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "scenario": {
                "requests": [
                    "http://blazedemo.com"
                ]
            }
        })
        obj.resource_files()

    def test_dont_copy_local_script_to_artifacts(self):
        "ensures that .java file is not copied into artifacts-dir"
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        filename = "BlazeDemo.java"
        script_path = __dir__() + "/../data/" + filename
        obj.execution.merge({
            "scenario": {
                "script": script_path,
            }
        })
        obj.prepare()
        files = obj.resource_files()
        self.assertIn(script_path, files)
        artifacts_script = os.path.join(obj.engine.artifacts_dir, filename)
        self.assertFalse(os.path.exists(artifacts_script))

    def test_take_script_from_artifacts(self):
        "ensures that executor looks for script in artifacts-dir (for cloud/remote cases)"
        obj = SeleniumExecutor()
        obj.engine = EngineEmul()
        obj.engine.file_search_paths = [obj.engine.artifacts_dir]

        script_name = "BlazeDemo.java"
        test_script = __dir__() + "/../data/" + script_name
        artifacts_script = os.path.join(obj.engine.artifacts_dir, script_name)
        shutil.copy2(test_script, artifacts_script)

        obj.execution.merge({
            "scenario": {
                "script": script_name,
            }
        })
        obj.prepare()
