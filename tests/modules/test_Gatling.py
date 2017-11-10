import logging
import os
import shutil
import time

from bzt.modules.gatling import GatlingExecutor, DataLogReader
from bzt.six import u
from bzt.utils import EXE_SUFFIX, get_full_path, BetterDict
from tests import BZTestCase, __dir__, RESOURCES_DIR, BUILD_DIR
from tests.mocks import EngineEmul
from bzt.modules.provisioning import Local
from bzt import ToolError, TaurusConfigError


def get_gatling():
    path = os.path.abspath(RESOURCES_DIR + "gatling/gatling" + EXE_SUFFIX)
    obj = GatlingExecutor()
    obj.engine = EngineEmul()
    obj.settings.merge({"path": path})
    return obj


class TestGatlingExecutor(BZTestCase):
    def setUp(self):
        super(TestGatlingExecutor, self).setUp()
        self.obj = get_gatling()

    def tearDown(self):
        if self.obj.stdout_file:
            self.obj.stdout_file.close()
        if self.obj.stderr_file:
            self.obj.stderr_file.close()
        if self.obj.reader and self.obj.reader.fds:
            self.fail("Reader file descriptor not closed")
        super(TestGatlingExecutor, self).tearDown()

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
            'files': [
                'tests/resources/grinder/fake_grinder.jar',
                'tests/resources/selenium/junit/jar'],
            'scenario': 'tests/resources/gatling/bs'})
        self.assertRaises(ToolError, self.obj.prepare)

    def test_external_jar_right_launcher(self):
        self.obj.execution.merge({
            'files': [
                'tests/resources/grinder/fake_grinder.jar',
                'tests/resources/selenium/junit/jar'],
            'scenario': {
                "script": RESOURCES_DIR + "gatling/BasicSimulation.scala",
                "simulation": "mytest.BasicSimulation"}})
        self.obj.prepare()
        self.obj.startup()
        self.obj.shutdown()

        jar_files = self.obj.jar_list
        modified_launcher = self.obj.launcher
        with open(modified_launcher) as modified:
            modified_lines = modified.readlines()

        self.assertIn('fake_grinder.jar', jar_files)
        self.assertIn('another_dummy.jar', jar_files)

        for line in modified_lines:
            self.assertFalse(line.startswith('set COMPILATION_CLASSPATH=""'))
            self.assertTrue(not line.startswith('COMPILATION_CLASSPATH=') or
                            line.endswith('":${COMPILATION_CLASSPATH}"\n'))

        with open(self.obj.stdout_file.name) as stdout:
            out_lines = stdout.readlines()

        out_lines = [out_line.rstrip() for out_line in out_lines]
        self.assertEqual(out_lines[-4], get_full_path(self.obj.settings['path'], step_up=2))  # $GATLING_HOME
        self.assertIn('fake_grinder.jar', out_lines[-3])  # $COMPILATION_CLASSPATH
        self.assertIn('another_dummy.jar', out_lines[-3])  # $COMPILATION_CLASSPATH
        self.assertEqual(out_lines[-2], 'TRUE')  # $NO_PAUSE

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
        self.assertEqual(res_files, [script_path])

    def test_resource_files_collection_local(self):
        script = "LocalBasicSimulation.scala"
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "gatling/" + script}})
        self.obj.prepare()
        artifacts = os.listdir(self.obj.engine.artifacts_dir)
        self.assertNotIn(script, artifacts)

    def test_env_type(self):
        script = "LocalBasicSimulation.scala"
        self.obj.execution.merge({
            "concurrency": 2,
            "hold-for": 1000,
            "throughput": 100,
            "scenario": {"script": RESOURCES_DIR + "gatling/" + script}})
        self.obj.prepare()
        self.obj.engine.artifacts_dir = u(self.obj.engine.artifacts_dir)
        self.obj.startup()
        self.obj.shutdown()
        with open(self.obj.stdout_file.name) as fds:
            lines = fds.readlines()
        self.assertIn('throughput', lines[-1])

    def test_warning_for_throughput_without_duration(self):
        script = "LocalBasicSimulation.scala"
        self.obj.execution.merge({
            "concurrency": 2,
            "throughput": 100,
            "scenario": {"script": RESOURCES_DIR + "gatling/" + script}})
        self.obj.prepare()
        self.obj.engine.artifacts_dir = u(self.obj.engine.artifacts_dir)
        self.obj.startup()
        self.obj.shutdown()
        with open(self.obj.stdout_file.name) as fds:
            lines = fds.readlines()
        self.assertNotIn('throughput', lines[-1])

    def test_requests_1(self):
        self.obj.execution.merge({
            "concurrency": 10,
            "iterations": 5,
            "scenario": {
                "think-time": 1,
                "follow-redirects": False,
                "default-address": "blazedemo.com",
                "headers": {"H1": "V1"},
                "requests": [{"url": "/reserve.php",
                              "headers": {"H2": "V2"},
                              "method": "POST",
                              "body": "Body Content",
                              "assert": [{
                                  "contains": ["bootstrap.min"],
                                  "not": True
                              }]},
                             {"url": "/",
                              "think-time": 2,
                              "follow-redirects": True}]
            }
        })
        self.obj.prepare()
        scala_file = self.obj.engine.artifacts_dir + '/' + self.obj.get_scenario().get('simulation') + '.scala'
        self.assertEqualFiles(RESOURCES_DIR + "gatling/generated1.scala", scala_file)

    def test_requests_def_addr_is_none(self):
        self.obj.execution.merge({
            "concurrency": 10,
            "hold-for": 110,
            "throughput": 33,
            "ramp-up": 30,
            "scenario": {
                'default-address': None,
                'keepalive': False,
                'timeout': '100ms',
                'requests': ['http://blazedemo.com', 'google.com']
            }
        })
        self.obj.prepare()

    def test_requests_def_addr_is_empty(self):
        self.obj.execution.merge({
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
        })
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
        self.assertEqualFiles(RESOURCES_DIR + "gatling/generated3.scala", scala_file)

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
        self.assertEqualFiles(RESOURCES_DIR + "gatling/generated4.scala", scala_file)

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

    def assertEqualFiles(self, name1, name2):
        def without_id(lines):
            id_mark = 'TaurusSimulation'
            id_pos = lines.find(id_mark)
            space_pos = lines.find(' ', id_pos)
            return lines[:id_pos + len(id_mark)] + lines[space_pos:]

        with open(name1, 'rt') as file1:
            with open(name2, 'rt') as file2:
                lines1 = without_id(file1.read())
                lines2 = without_id(file2.read())
        self.assertEqual(lines1, lines2)

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
        self.assertIn('simulations.jar', self.obj.jar_list)

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
            self.assertIn('simulations.jar', self.obj.jar_list)
            self.assertIn('deps.jar', self.obj.jar_list)
        finally:
            os.chdir(curdir)

    def test_data_sources(self):
        self.obj.execution.merge({
            "scenario": {
                "data-sources": [
                    RESOURCES_DIR + "test1.csv",
                ],
                "requests": ["http://blazedemo.com/?tag=${col1}"],
            }
        })
        self.obj.prepare()
        scala_file = self.obj.engine.artifacts_dir + '/' + self.obj.get_scenario().get('simulation') + '.scala'
        self.assertEqualFiles(RESOURCES_DIR + "gatling/generated_data_sources.scala", scala_file)
        self.assertTrue(os.path.exists(os.path.join(self.obj.engine.artifacts_dir, 'test1.csv')))

    def test_resource_files_data_sources(self):
        csv_path = RESOURCES_DIR + "test1.csv"
        self.obj.execution.merge({
            "scenario": {
                "data-sources": [csv_path],
                "requests": ["http://blazedemo.com/"],
            }
        })
        res_files = self.obj.resource_files()
        self.assertEqual(res_files, [csv_path])

    def test_diagnostics(self):
        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "gatling/simulations.jar",
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
                "requests": ["http://blazedemo.com/"],
            }
        })
        saved_env = BetterDict()
        self.obj.execute = lambda self, *args, **kwargs: saved_env.merge(kwargs['env'])
        self.obj.prepare()
        self.obj.startup()
        self.assertIn("gatling.http.ahc.allowPoolingConnections=true", saved_env["JAVA_OPTS"])
        self.assertIn("gatling.http.ahc.keepAlive=true", saved_env["JAVA_OPTS"])


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = RESOURCES_DIR + "gatling/"
        obj = DataLogReader(log_path, logging.getLogger(''), 'gatling-0')
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 23)
        self.assertEqual(obj.guessed_gatling_version, "2.1")

    def test_read_asserts(self):
        log_path = RESOURCES_DIR + "gatling/"
        obj = DataLogReader(log_path, logging.getLogger(''), 'gatling-1')
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 3)
        self.assertEqual(obj.guessed_gatling_version, "2.2+")

    def test_read_220_format(self):
        log_path = RESOURCES_DIR + "gatling/"
        obj = DataLogReader(log_path, logging.getLogger(''), 'gatling-220')
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 4)
        self.assertEqual(obj.guessed_gatling_version, "2.2+")
