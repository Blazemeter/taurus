# coding=utf-8
import os
import shutil
import sys
import time

from bzt import ToolError, TaurusConfigError
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.gatling import GatlingExecutor, DataLogReader, is_gatling2
from bzt.modules.provisioning import Local
from bzt.utils import EXE_SUFFIX, get_full_path, is_windows
from tests import ExecutorTestCase, BZTestCase, __dir__, RESOURCES_DIR, BUILD_DIR, close_reader_file, ROOT_LOGGER


class TestGatlingExecutor(ExecutorTestCase):
    EXECUTOR = GatlingExecutor

    def setUp(self):
        super(TestGatlingExecutor, self).setUp()
        path = os.path.abspath(RESOURCES_DIR + "gatling/gatling2" + EXE_SUFFIX)
        self.obj.settings.merge({"path": path, "version": "2.3.0"})
        self.obj.env.add_path({"PATH": os.path.dirname(sys.executable)})

    def tearDown(self):
        close_reader_file(self.obj.reader)
        super(TestGatlingExecutor, self).tearDown()

    def test_gatling3(self):
        self.obj.settings.merge({
            "path": os.path.abspath(RESOURCES_DIR + "gatling/gatling3" + EXE_SUFFIX),
            "version": "3.0.1"})

        self.configure({
            "execution": {
                "executor": "gatling",
                "ramp-up": "1m",
                "scenario": "gs"},
            "scenarios": {
                "gs": {
                    "requests": [
                        "http://blazedemo.com"]}}})
        self.obj.prepare()
        target_script = os.path.join(RESOURCES_DIR, "gatling", "gatling3.scala")
        target_str = "class TaurusSimulation extends Simulation {"
        substitute = "class TaurusSimulation_%s extends Simulation {" % id(self.obj)
        self.assertFilesEqual(target_script, self.obj.script, replace_str=target_str, replace_with=substitute)

    def test_embedded_res(self):
        self.configure({
            "execution": {
                "executor": "gatling",
                "ramp-up": "1m",
                "scenario": "gs"},
            "scenarios": {
                "gs": {
                    "retrieve-resources": True,
                    "retrieve-resources-regex": "(.*)boo(. *)",
                    "requests": [
                        "http://blazedemo.com"]}}})
        self.obj.prepare()
        target_script = os.path.join(RESOURCES_DIR, "gatling", "embedded_res.scala")
        target_str = "class TaurusSimulation extends Simulation {"
        substitute = "class TaurusSimulation_%s extends Simulation {" % id(self.obj)
        self.assertFilesEqual(target_script, self.obj.script, replace_str=target_str, replace_with=substitute)

    def test_external_jar_wrong_launcher(self):
        modified_launcher = self.obj.engine.create_artifact('wrong-gatling', EXE_SUFFIX)
        origin_launcher = get_full_path(self.obj.settings['path'])
        with open(origin_launcher) as orig_file:
            with open(modified_launcher, 'w') as mod_file:
                for line in orig_file.readlines():
                    if 'COMPILATION_CLASSPATH' not in line:
                        mod_file.writelines([line])
        os.chmod(modified_launcher, 0o755)

        self.obj.settings.merge({"path": modified_launcher})

        self.obj.execution.merge({
            "scenario": {
                "script": "tests/resources/gatling/bs",
                "additional-classpath": ["tests/resources/grinder/fake_grinder.jar"]}})
        self.assertRaises(ToolError, self.obj.prepare)

    def test_additional_classpath(self):
        jars = ("gatling", "simulations.jar"), ("gatling", "deps.jar")
        jars = list(os.path.join(RESOURCES_DIR, *jar) for jar in jars)

        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "gatling/BasicSimulation.scala",
                "additional-classpath": [jars[0]]}})
        self.obj.settings.merge({"additional-classpath": [jars[1]]})
        self.obj.prepare()

        for jar in jars:
            for var in ("JAVA_CLASSPATH", "COMPILATION_CLASSPATH"):
                self.assertIn(jar, self.obj.env.get(var))

    def test_external_jar_built_launcher_v2(self):
        jars = ['tests/resources/grinder/fake_grinder.jar', 'tests/resources/selenium/junit/another_dummy.jar']
        self.obj.execution.merge({
            'files': [
                jars[0]],
            'scenario': {
                "script": RESOURCES_DIR + "gatling/BasicSimulation.scala",
                "additional-classpath": [jars[1]],
                "simulation": "mytest.BasicSimulation"}})
        self.obj.prepare()
        self.obj.startup()
        self.obj.shutdown()

        modified_launcher = self.obj.tool.tool_path
        with open(modified_launcher) as modified:
            modified_lines = modified.readlines()

        for var in ("JAVA_CLASSPATH", "COMPILATION_CLASSPATH"):
            self.assertNotIn(jars[0], self.obj.env.get(var))
            self.assertIn(jars[1], self.obj.env.get(var))

        for line in modified_lines:
            if not is_windows() and '"$JAVA"' in line and not line.startswith("bash"):
                self.assertTrue(line.startswith('eval'))
            self.assertFalse(line.startswith('set COMPILATION_CLASSPATH=""'))  # win
            if line.startswith('COMPILATION_CLASSPATH='):  # linux
                self.assertTrue(line.endswith(':"${COMPILATION_CLASSPATH}"\n'))

    def test_external_jar_built_launcher_v3(self):
        jars = ['tests/resources/grinder/fake_grinder.jar', 'tests/resources/selenium/junit/another_dummy.jar']
        self.obj.execution.merge({
            'files': [
                jars[0]],
            'scenario': {
                "script": RESOURCES_DIR + "gatling/BasicSimulation.scala",
                "additional-classpath": [jars[1]],
                "simulation": "mytest.BasicSimulation"}})

        path = os.path.abspath(RESOURCES_DIR + "gatling/gatling3" + EXE_SUFFIX)
        self.obj.settings.merge({"path": path, "version": "3.0.0"})

        self.obj.prepare()
        self.obj.startup()
        self.obj.shutdown()

        modified_launcher = self.obj.tool.tool_path
        with open(modified_launcher) as modified:
            modified_lines = modified.readlines()

        for var in ("JAVA_CLASSPATH", "COMPILATION_CLASSPATH"):
            self.assertNotIn(jars[0], self.obj.env.get(var))
            self.assertIn(jars[1], self.obj.env.get(var))

        for line in modified_lines:
            if not is_windows() and '"$JAVA"' in line and not line.startswith("bash"):
                self.assertTrue(line.startswith('eval'))
            if line.startswith('set COMPILER_CLASSPATH='):  # win
                self.assertTrue(line.endswith(';%COMPILATION_CLASSPATH%\n'))
            if line.startswith('set GATLING_CLASSPATH='):  # win
                self.assertTrue(line.endswith(';%JAVA_CLASSPATH%\n'))
            if line.startswith('COMPILER_CLASSPATH'):  # linux
                self.assertTrue(line.endswith('${COMPILATION_CLASSPATH}"\n'))
            if line.startswith('GATLING_CLASSPATH'):  # linux
                self.assertTrue(line.endswith('${JAVA_CLASSPATH}"\n'))

    def test_install_Gatling(self):
        path = os.path.abspath(BUILD_DIR + "gatling-taurus/bin/gatling" + EXE_SUFFIX)
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)

        download_link = "file:///" + RESOURCES_DIR + "gatling/gatling-dist-{version}.zip"
        gatling_version = '2.3.0'

        self.assertFalse(os.path.exists(path))
        self.obj.settings.merge({
            "path": path,
            "download-link": download_link,
            "version": gatling_version})

        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "gatling/BasicSimulation.scala",
                "simulation": "mytest.BasicSimulation"}})

        self.obj.prepare()
        self.assertTrue(os.path.exists(path))

    def test_gatling_widget(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "gatling/BasicSimulation.scala"}})
        self.obj.prepare()
        self.obj.get_widget()
        self.assertEqual(self.obj.widget.widgets[0].text, "Gatling: BasicSimulation.scala")

    def test_resource_files_collection_remote2(self):  # script = <dir>
        script_path = RESOURCES_DIR + "gatling/bs"
        self.obj.execution.merge({"scenario": {"script": script_path}})
        res_files = self.obj.resource_files()
        self.assertPathsEqual(res_files, [script_path])

    def test_resource_files_collection_local(self):
        script = "LocalBasicSimulation.scala"
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "gatling/" + script}})
        self.obj.prepare()
        artifacts = os.listdir(self.obj.engine.artifacts_dir)
        self.assertNotIn(script, artifacts)

    def test_warning_for_throughput_without_duration(self):
        script = "LocalBasicSimulation.scala"
        self.configure({"execution": {
            "concurrency": 2,
            "throughput": 100,
            "scenario": {"script": RESOURCES_DIR + "gatling/" + script}}})
        self.obj.prepare()
        self.obj.engine.artifacts_dir = self.obj.engine.artifacts_dir
        self.obj.startup()
        self.obj.shutdown()
        with open(self.obj.stdout.name) as fds:
            lines = fds.readlines()
        self.assertNotIn('throughput', lines[-1])

    def test_requests_1(self):
        self.configure({"execution": {
            "concurrency": 10,
            "iterations": 5,
            "scenario": {
                "think-time": 1,
                "follow-redirects": False,
                "default-address": "blazedemo.com",
                "headers": {"H1": "V1"},
                "requests": [
                    {
                        "url": "/reserve.php",  # complicate body without content header -> json
                        "headers": {"H2": "V2"},
                        "method": "POST",
                        "body": {"com": {"pli": {"cat": ["ed", "dict"]}}},
                        "assert": [{
                            "contains": ["bootstrap.min"],
                            "not": True}]
                    }, {
                        "url": "/",
                        "think-time": 2,
                        "follow-redirects": True
                    }, {
                        "url": "/reserve.php",
                        "method": "POST",
                        "body": u"Body Content 日本語",
                    }, {
                        "url": "/something.php",
                        "method": "POST",
                        "body": {
                            "param_name1": "param_value1",
                            "param_name2": "param_value2"}  # simple body without content header -> params
                    }, {
                        "url": "/something_else.php",
                        "headers": {"Content-Type": "application/json"},
                        "method": "POST",
                        "body": {
                            "param_name3": "param_value4"}}  # simple body with content header -> json
                ]}}})
        self.obj.prepare()
        scala_file = self.obj.engine.artifacts_dir + '/' + self.obj.get_scenario().get('simulation') + '.scala'
        self.assertFilesEqual(RESOURCES_DIR + "gatling/generated1.scala", scala_file,
                              self.obj.get_scenario().get('simulation'), "SIMNAME")

    def test_requests_def_addr_is_none(self):
        self.configure({"execution": {
            "concurrency": 10,
            "hold-for": 110,
            "throughput": 33,
            "ramp-up": 30,
            "scenario": {
                'keepalive': False,
                'timeout': '100ms',
                'requests': ['http://blazedemo.com', 'google.com']
            }
        }})
        self.obj.prepare()

    def test_requests_def_addr_is_empty(self):
        self.configure({"execution": {
            "concurrency": 10,
            "hold-for": 110,
            "throughput": 33,
            "ramp-up": 30,
            "scenario": {
                'default-address': '',
                'keepalive': False,
                'timeout': '100ms',
                'requests': ['http://blazedemo.com', 'google.com']
            }
        }})
        self.obj.prepare()

    def test_requests_3(self):
        self.obj.execution.merge({
            "iterations": 55,
            "scenario": {
                "requests": [{'url': 'http://site.com/reserve.php',
                              'assert': [{
                                  'contains': [200],
                                  'subject': 'http-code',
                                  'not': False
                              }]}]
            }
        })
        self.obj.prepare()
        scala_file = self.obj.engine.artifacts_dir + '/' + self.obj.get_scenario().get('simulation') + '.scala'
        self.assertFilesEqual(RESOURCES_DIR + "gatling/generated3.scala", scala_file,
                              self.obj.get_scenario().get('simulation'), "SIMNAME")

    def test_requests_4(self):
        self.obj.execution.merge({
            "iterations": 55,
            "scenario": {
                "default-address": "",
                "requests": [{'url': 'site.com/reserve.php',
                              'assert': [{
                                  'subject': 'body',
                                  'contains': 'boot(.*)strap.min',
                                  'regexp': True,
                                  'not': False
                              }]}]
            }
        })
        self.obj.prepare()
        scala_file = self.obj.engine.artifacts_dir + '/' + self.obj.get_scenario().get('simulation') + '.scala'
        self.assertFilesEqual(RESOURCES_DIR + "gatling/generated4.scala", scala_file,
                              self.obj.get_scenario().get('simulation'), "SIMNAME")

    def test_requests_5(self):
        self.obj.execution.merge({
            "iterations": 55,
            "scenario": {
                "default-address": "blazedemo.com",
                "requests": [{'url': '/reserve.php',
                              'assert': [{
                                  'subject': 'body',
                                  'regexp': True,
                                  'not': False
                              }]}]
            }
        })
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_requests_6(self):
        self.obj.execution.merge({
            "iterations": 5,
            "scenario": {
                "store-cache": False,
                "default-address": "example.com",
                "requests": ['/'],
            }
        })
        self.obj.prepare()
        scala_file = self.obj.engine.artifacts_dir + '/' + self.obj.get_scenario().get('simulation') + '.scala'
        self.assertFilesEqual(RESOURCES_DIR + "gatling/generated6.scala", scala_file,
                              self.obj.get_scenario().get('simulation'), "SIMNAME")

    def test_fail_on_zero_results(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "gatling/bs/BasicSimulation.scala"}})
        self.obj.prepare()
        self.obj.engine.prepared = [self.obj]
        self.obj.engine.started = [self.obj]
        prov = Local()
        prov.engine = self.obj.engine
        prov.executors = [self.obj]
        self.obj.engine.provisioning = prov
        self.obj.reader.buffer = ['some info']
        self.obj.engine.provisioning.post_process()

    def test_no_simulation(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "gatling/bs/BasicSimulation.scala"}})
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()

    def test_full_Gatling(self):
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "gatling/bs/BasicSimulation.scala",
                "simulation": "fake"
            }
        })
        self.obj.prepare()

        self.obj.settings.merge({"path": RESOURCES_DIR + "gatling/gatling" + EXE_SUFFIX})

        try:
            self.obj.startup()

            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()

    def test_interactive_request(self):
        self.obj.engine.existing_artifact(RESOURCES_DIR + "gatling/SimpleSimulation.scala")
        self.obj.execution.merge({
            "scenario": {
                "script": self.obj.engine.artifacts_dir + "/SimpleSimulation.scala",
                "simulation": "SimpleSimulation"}})
        self.obj.prepare()
        self.obj.settings.merge({"path": RESOURCES_DIR + "gatling/gatling" + EXE_SUFFIX})
        counter1 = 0
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
            counter1 += 1
        self.obj.shutdown()
        self.obj.post_process()

        self.tearDown()  # Carthage must be destroyed...
        self.setUp()

        self.obj.engine.existing_artifact(RESOURCES_DIR + "gatling/SimpleSimulation.scala")
        self.obj.engine.existing_artifact(RESOURCES_DIR + "gatling/generated1.scala")
        self.obj.execution.merge({
            "scenario": {
                "script": self.obj.engine.artifacts_dir + "/SimpleSimulation.scala",
                "simulation": "fake"}})
        self.obj.prepare()
        self.obj.settings.merge({"path": RESOURCES_DIR + "gatling/gatling" + EXE_SUFFIX})
        counter2 = 0
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
                counter2 += 1
                if counter2 > counter1 * 5:
                    self.fail('It seems gatling made interactive request')
            self.obj.shutdown()
            self.obj.post_process()
        except TaurusConfigError:
            return
        self.fail('ValueError not found')

    def test_script_jar(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "gatling/simulations.jar",
                                               "simulation": "tests.gatling.BasicSimulation"}})
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()

        for var in ("JAVA_CLASSPATH", "COMPILATION_CLASSPATH"):
            self.assertIn("simulations.jar", self.obj.env.get(var))

    def test_files_find_file(self):
        curdir = get_full_path(os.curdir)
        try:
            os.chdir(__dir__() + "/../")
            self.obj.engine.file_search_paths.append(RESOURCES_DIR + "gatling/")
            self.obj.engine.config.merge({
                "execution": {
                    "scenario": {
                        "script": "simulations.jar",
                        "simulation": "tests.gatling.BasicSimulation"
                    },
                    "files": ["deps.jar"]
                }
            })
            self.obj.execution.merge(self.obj.engine.config["execution"])
            self.obj.prepare()
            try:
                self.obj.startup()
                while not self.obj.check():
                    time.sleep(self.obj.engine.check_interval)
            finally:
                self.obj.shutdown()

            for var in ("JAVA_CLASSPATH", "COMPILATION_CLASSPATH"):
                self.assertIn("simulations.jar", self.obj.env.get(var))
                self.assertNotIn("deps.jar", self.obj.env.get(var))  # new logic: don't add 'files' to cp

        finally:
            os.chdir(curdir)

    def test_data_sources(self):
        csv1 = RESOURCES_DIR + "test1.csv"
        csv2 = RESOURCES_DIR + "test2.csv"
        csv3 = RESOURCES_DIR + "files/test2.csv"

        self.obj.execution.merge({
            "scenario": {
                "data-sources": [{
                    "path": csv1,
                    "loop": False,
                    "delimiter": ","
                }, csv2, csv3],
                "requests": ["http://blazedemo.com/?tag=${col1}"],
            }
        })

        path = os.path.abspath(RESOURCES_DIR + "gatling/gatling3" + EXE_SUFFIX)
        self.obj.settings.merge({"path": path, "version": "3.1"})

        self.obj.prepare()

        original_scala = RESOURCES_DIR + "gatling/generated_data_sources.scala"
        generated_scala = self.obj.engine.artifacts_dir + '/' + self.obj.get_scenario().get('simulation') + '.scala'

        replace_str = self.obj.get_scenario().get('simulation'), csv1, csv2, csv3

        replace_with = "SIMNAME", os.path.basename(csv1), os.path.basename(csv2), os.path.basename(csv3)

        self.assertFilesEqual(original_scala, generated_scala, replace_str, replace_with)

        # don't copy csv to artifacts dir
        self.assertFalse(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, 'test1.csv')))

    def test_resource_files_data_sources(self):
        csv_path = RESOURCES_DIR + "test1.csv"
        jar_file = "path_to_my_jar"
        self.obj.execution.merge({
            "scenario": {
                "data-sources": [csv_path],
                "requests": ["http://blazedemo.com/"],
            }
        })
        self.obj.settings.merge({'additional-classpath': [jar_file]})
        res_files = self.obj.resource_files()
        self.assertEqual(res_files, [csv_path, jar_file])

    def test_diagnostics(self):
        self.obj.execution.merge({
            "scenario": {
                "script": RESOURCES_DIR + "gatling/simulations.jar",
                "simulation": "tests.gatling.BasicSimulation"}})
        self.obj.prepare()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertIsNotNone(self.obj.get_error_diagnostics())

    def test_properties_migration(self):
        self.obj.execution.merge({
            "scenario": {
                "keepalive": True,
                "requests": ["http://blazedemo.com/"]}
        })

        self.obj._execute = lambda *args, **kwargs: None
        self.obj.prepare()
        self.obj.startup()

        if is_windows():
            form = '%s'
        else:
            form = '%r'

        self.assertIn("gatling.http.ahc.allowPoolingConnections=" + form % 'true', self.obj.env.get("JAVA_OPTS"))
        self.assertIn("gatling.http.ahc.keepAlive=" + form % 'true', self.obj.env.get("JAVA_OPTS"))

    def test_properties_2levels(self):
        self.obj.settings.merge({
            "properties": {
                "settlevel": u"settval",
                "unc": u"Ä",
                "override": 1,
            },
        })
        self.obj.execution.merge({
            "scenario": {
                "properties": {
                    "scenlevel": "scenval",
                    "override": 2,
                },
                "requests": ["http://blazedemo.com/"]}
        })

        self.obj._execute = lambda *args, **kwargs: None
        self.obj.prepare()
        self.obj.startup()

        if is_windows():
            form = '%s'
        else:
            form = '%r'

            self.assertIn("-Dscenlevel=" + form % 'scenval', self.obj.env.get("JAVA_OPTS"))
            self.assertIn("-Dsettlevel=" + form % 'settval', self.obj.env.get("JAVA_OPTS"))
        self.assertIn("-Doverride=2", self.obj.env.get("JAVA_OPTS"))


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = RESOURCES_DIR + "gatling/"
        obj = DataLogReader(log_path, ROOT_LOGGER, 'gatling-0')
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 23)
        self.assertEqual(obj.guessed_gatling_version, "2.1")
        self.assertIn('request_1', list_of_values[-1][DataPoint.CUMULATIVE].keys())

    def test_read_asserts(self):
        log_path = RESOURCES_DIR + "gatling/"
        obj = DataLogReader(log_path, ROOT_LOGGER, 'gatling-1')
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 3)
        self.assertEqual(obj.guessed_gatling_version, "2.2+")
        self.assertIn('ping request', list_of_values[-1][DataPoint.CUMULATIVE].keys())

    def test_read_220_format(self):
        log_path = RESOURCES_DIR + "gatling/"
        obj = DataLogReader(log_path, ROOT_LOGGER, 'gatling-220')
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 4)
        self.assertEqual(obj.guessed_gatling_version, "2.2+")
        self.assertIn('/', list_of_values[-1][DataPoint.CUMULATIVE].keys())

    def test_read_labels_problematic(self):
        log_path = RESOURCES_DIR + "gatling/"
        obj = DataLogReader(log_path, ROOT_LOGGER, 'gatling-2')  # problematic one
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 1)
        self.assertEqual(obj.guessed_gatling_version, "2.2+")
        last_cumul = list_of_values[-1][DataPoint.CUMULATIVE]
        self.assertEqual(1, last_cumul['User-Login'][KPISet.SAMPLE_COUNT])

    def test_read_labels_regular(self):
        log_path = RESOURCES_DIR + "gatling/"
        obj = DataLogReader(log_path, ROOT_LOGGER, 'gatling-3')  # regular one
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 10)
        self.assertEqual(obj.guessed_gatling_version, "2.2+")
        self.assertIn('http://blazedemo.com/', list_of_values[-1][DataPoint.CUMULATIVE].keys())

    def test_read_group(self):
        log_path = RESOURCES_DIR + "gatling/"
        obj = DataLogReader(log_path, ROOT_LOGGER, 'gatling-4')  # regular one
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 179)
        self.assertEqual(obj.guessed_gatling_version, "2.2+")
        last_cumul = list_of_values[-1][DataPoint.CUMULATIVE]
        self.assertEqual(2, len(last_cumul['[empty]'][KPISet.ERRORS]))

    def test_read_rc_asserts(self):
        log_path = RESOURCES_DIR + "gatling/"
        obj = DataLogReader(log_path, ROOT_LOGGER, 'gatling-5')  # regular one
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 1)
        self.assertEqual(obj.guessed_gatling_version, "2.2+")
        last_cumul = list_of_values[-1][DataPoint.CUMULATIVE]
        self.assertEqual(1, last_cumul[''][KPISet.RESP_CODES]['400'])
        self.assertEqual(1, last_cumul[''][KPISet.RESP_CODES]['401'])

    def test_read_gatling302(self):
        log_path = RESOURCES_DIR + "gatling/"
        obj = DataLogReader(log_path, ROOT_LOGGER, 'gatling-302')  # regular one
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 1)
        self.assertEqual(obj.guessed_gatling_version, "3.X")
        self.assertIn('group2', list_of_values[-1][DataPoint.CUMULATIVE].keys())
