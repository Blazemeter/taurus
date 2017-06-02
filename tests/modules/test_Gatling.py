import logging
import os
import shutil
import time

from bzt.modules.gatling import GatlingExecutor, DataLogReader
from bzt.six import u
from bzt.utils import EXE_SUFFIX, get_full_path
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul
from bzt.modules.provisioning import Local
from bzt import ToolError, TaurusConfigError


class TestGatlingExecutor(BZTestCase):
    @staticmethod
    def getGatling():
        path = os.path.abspath(__dir__() + "/../resources/gatling/gatling" + EXE_SUFFIX)
        obj = GatlingExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})
        return obj

    def test_external_jar_wrong_launcher(self):
        obj = self.getGatling()

        modified_launcher = obj.engine.create_artifact('wrong-gatling', EXE_SUFFIX)
        origin_launcher = get_full_path(obj.settings['path'])
        with open(origin_launcher) as orig_file:
            with open(modified_launcher, 'w') as mod_file:
                for line in orig_file.readlines():
                    if 'COMPILATION_CLASSPATH' not in line:
                        mod_file.writelines([line])
        os.chmod(modified_launcher, 0o755)

        obj.settings.merge({"path": modified_launcher})

        obj.execution.merge({
            'files': [
                'tests/resources/grinder/fake_grinder.jar',
                'tests/resources/selenium/junit/jar'],
            'scenario': 'tests/resources/gatling/bs'})
        self.assertRaises(ToolError, obj.prepare)

    def test_external_jar_right_launcher(self):
        obj = self.getGatling()

        obj.execution.merge({
            'files': [
                'tests/resources/grinder/fake_grinder.jar',
                'tests/resources/selenium/junit/jar'],
            'scenario': {
                "script": __dir__() + "/../resources/gatling/BasicSimulation.scala",
                "simulation": "mytest.BasicSimulation"}})
        obj.prepare()
        obj.startup()
        obj.shutdown()

        jar_files = obj.jar_list
        modified_launcher = obj.launcher
        with open(modified_launcher) as modified:
            modified_lines = modified.readlines()

        self.assertIn('fake_grinder.jar', jar_files)
        self.assertIn('another_dummy.jar', jar_files)

        for line in modified_lines:
            self.assertFalse(line.startswith('set COMPILATION_CLASSPATH=""'))
            self.assertTrue(not line.startswith('COMPILATION_CLASSPATH=') or
                            line.endswith('":${COMPILATION_CLASSPATH}"\n'))

        with open(obj.stdout_file.name) as stdout:
            out_lines = stdout.readlines()

        out_lines = [out_line.rstrip() for out_line in out_lines]
        self.assertEqual(out_lines[-4], get_full_path(obj.settings['path'], step_up=2))  # $GATLING_HOME
        self.assertIn('fake_grinder.jar', out_lines[-3])  # $COMPILATION_CLASSPATH
        self.assertIn('another_dummy.jar', out_lines[-3])  # $COMPILATION_CLASSPATH
        self.assertEqual(out_lines[-2], 'TRUE')  # $NO_PAUSE

    def test_install_Gatling(self):
        path = os.path.abspath(__dir__() + "/../../build/tmp/gatling-taurus/bin/gatling" + EXE_SUFFIX)
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)

        download_link = "file:///" + __dir__() + "/../resources/gatling/gatling-dist-{version}_{version}.zip"
        gatling_version = '2.1.4'

        self.assertFalse(os.path.exists(path))
        obj = self.getGatling()
        obj.settings.merge({
            "path": path,
            "download-link": download_link,
            "version": gatling_version})

        obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../resources/gatling/BasicSimulation.scala",
                "simulation": "mytest.BasicSimulation"}})

        obj.prepare()
        self.assertTrue(os.path.exists(path))

    def test_gatling_widget(self):
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../resources/gatling/BasicSimulation.scala"}})
        obj.prepare()
        obj.get_widget()
        self.assertEqual(obj.widget.widgets[0].text, "Gatling: BasicSimulation.scala")

    def test_resource_files_collection_remote2(self):  # script = <dir>
        obj = self.getGatling()
        script_path = __dir__() + "/../resources/gatling/bs"
        obj.execution.merge({"scenario": {"script": script_path}})
        res_files = obj.resource_files()
        self.assertEqual(res_files, [script_path])

    def test_resource_files_collection_local(self):
        obj = self.getGatling()
        script = "LocalBasicSimulation.scala"
        obj.execution.merge({"scenario": {"script": __dir__() + "/../resources/gatling/" + script}})
        obj.prepare()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertNotIn(script, artifacts)

    def test_env_type(self):
        obj = self.getGatling()
        script = "LocalBasicSimulation.scala"
        obj.execution.merge({
            "concurrency": 2,
            "hold-for": 1000,
            "throughput": 100,
            "scenario": {"script": __dir__() + "/../resources/gatling/" + script}})
        obj.prepare()
        obj.engine.artifacts_dir = u(obj.engine.artifacts_dir)
        obj.startup()
        obj.shutdown()
        with open(obj.stdout_file.name) as fds:
            lines = fds.readlines()
        self.assertIn('throughput', lines[-1])

    def test_warning_for_throughput_without_duration(self):
        obj = self.getGatling()
        script = "LocalBasicSimulation.scala"
        obj.execution.merge({
            "concurrency": 2,
            "throughput": 100,
            "scenario": {"script": __dir__() + "/../resources/gatling/" + script}})
        obj.prepare()
        obj.engine.artifacts_dir = u(obj.engine.artifacts_dir)
        obj.startup()
        obj.shutdown()
        with open(obj.stdout_file.name) as fds:
            lines = fds.readlines()
        self.assertNotIn('throughput', lines[-1])

    def test_requests_1(self):
        obj = self.getGatling()
        obj.execution.merge({
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
        obj.prepare()
        scala_file = obj.engine.artifacts_dir + '/' + obj.get_scenario().get('simulation') + '.scala'
        self.assertEqualFiles(__dir__() + "/../resources/gatling/generated1.scala", scala_file)

    def test_requests_def_addr_is_none(self):
        obj = self.getGatling()
        obj.execution.merge({
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
        obj.prepare()

    def test_requests_def_addr_is_empty(self):
        obj = self.getGatling()
        obj.execution.merge({
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
        obj.prepare()

    def test_requests_3(self):
        obj = self.getGatling()
        obj.execution.merge({
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
        obj.prepare()
        scala_file = obj.engine.artifacts_dir + '/' + obj.get_scenario().get('simulation') + '.scala'
        self.assertEqualFiles(__dir__() + "/../resources/gatling/generated3.scala", scala_file)

    def test_requests_4(self):
        obj = self.getGatling()
        obj.execution.merge({
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
        obj.prepare()
        scala_file = obj.engine.artifacts_dir + '/' + obj.get_scenario().get('simulation') + '.scala'
        self.assertEqualFiles(__dir__() + "/../resources/gatling/generated4.scala", scala_file)

    def test_requests_5(self):
        obj = self.getGatling()
        obj.execution.merge({
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
        self.assertRaises(TaurusConfigError, obj.prepare)

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
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../resources/gatling/bs/BasicSimulation.scala"}})
        obj.prepare()
        obj.engine.prepared = [obj]
        obj.engine.started = [obj]
        prov = Local()
        prov.engine = obj.engine
        prov.executors = [obj]
        obj.engine.provisioning = prov
        obj.reader.buffer = ['some info']
        obj.engine.provisioning.post_process()

    def test_no_simulation(self):
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../resources/gatling/bs/BasicSimulation.scala"}})
        obj.prepare()
        try:
            obj.startup()
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()

    def test_full_Gatling(self):
        obj = self.getGatling()
        obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../resources/gatling/bs/BasicSimulation.scala",
                "simulation": "fake"
            }
        })
        obj.prepare()

        obj.settings.merge({"path": __dir__() + "/../resources/gatling/gatling" + EXE_SUFFIX})

        try:
            obj.startup()

            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()

    def test_interactive_request(self):
        obj = self.getGatling()
        obj.engine.existing_artifact(__dir__() + "/../resources/gatling/SimpleSimulation.scala")
        obj.execution.merge({
            "scenario": {
                "script": obj.engine.artifacts_dir + "/SimpleSimulation.scala",
                "simulation": "SimpleSimulation"}})
        obj.prepare()
        obj.settings.merge({"path": __dir__() + "/../resources/gatling/gatling" + EXE_SUFFIX})
        counter1 = 0
        obj.startup()
        while not obj.check():
            time.sleep(obj.engine.check_interval)
            counter1 += 1
        obj.shutdown()
        obj.post_process()

        obj = self.getGatling()
        obj.engine.existing_artifact(__dir__() + "/../resources/gatling/SimpleSimulation.scala")
        obj.engine.existing_artifact(__dir__() + "/../resources/gatling/generated1.scala")
        obj.execution.merge({
            "scenario": {
                "script": obj.engine.artifacts_dir + "/SimpleSimulation.scala",
                "simulation": "fake"}})
        obj.prepare()
        obj.settings.merge({"path": __dir__() + "/../resources/gatling/gatling" + EXE_SUFFIX})
        counter2 = 0
        try:
            obj.startup()
            while not obj.check():
                time.sleep(obj.engine.check_interval)
                counter2 += 1
                if counter2 > counter1 * 5:
                    self.fail('It seems gatling made interactive request')
            obj.shutdown()
            obj.post_process()
        except TaurusConfigError:
            return
        self.fail('ValueError not found')

    def test_script_jar(self):
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../resources/gatling/simulations.jar",
                                          "simulation": "tests.gatling.BasicSimulation"}})
        obj.prepare()
        try:
            obj.startup()
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()
        self.assertIn('simulations.jar', obj.jar_list)

    def test_files_find_file(self):
        curdir = get_full_path(os.curdir)
        try:
            os.chdir(__dir__() + "/../")
            obj = self.getGatling()
            obj.engine.file_search_paths.append(__dir__() + "/../resources/gatling/")
            obj.engine.config.merge({
                "execution":{
                    "scenario": {
                        "script": "simulations.jar",
                        "simulation": "tests.gatling.BasicSimulation"
                    },
                    "files": ["deps.jar"]
                }
            })
            obj.execution.merge(obj.engine.config["execution"])
            obj.prepare()
            try:
                obj.startup()
                while not obj.check():
                    time.sleep(obj.engine.check_interval)
            finally:
                obj.shutdown()
            self.assertIn('simulations.jar', obj.jar_list)
            self.assertIn('deps.jar', obj.jar_list)
        finally:
            os.chdir(curdir)

    def test_data_sources(self):
        obj = self.getGatling()
        obj.execution.merge({
            "scenario": {
                "data-sources": [
                  __dir__() + "/../resources/test1.csv",
                ],
                "requests": ["http://blazedemo.com/"],
            }
        })
        obj.prepare()
        scala_file = obj.engine.artifacts_dir + '/' + obj.get_scenario().get('simulation') + '.scala'
        self.assertEqualFiles(__dir__() + "/../resources/gatling/generated_data_sources.scala", scala_file)
        self.assertTrue(os.path.exists(os.path.join(obj.engine.artifacts_dir, 'test1.csv')))

    def test_resource_files_data_sources(self):
        obj = self.getGatling()
        csv_path = __dir__() + "/../resources/test1.csv"
        obj.execution.merge({
            "scenario": {
                "data-sources": [csv_path],
                "requests": ["http://blazedemo.com/"],
            }
        })
        res_files = obj.resource_files()
        self.assertEqual(res_files, [csv_path])


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = __dir__() + "/../resources/gatling/"
        obj = DataLogReader(log_path, logging.getLogger(''), 'gatling-0')
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 23)
        self.assertEqual(obj.guessed_gatling_version, "2.1")

    def test_read_220_format(self):
        log_path = __dir__() + "/../resources/gatling/"
        obj = DataLogReader(log_path, logging.getLogger(''), 'gatling-220')
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 4)
        self.assertEqual(obj.guessed_gatling_version, "2.2")
