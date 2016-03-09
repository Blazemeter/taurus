import logging
import os
import re
import shutil
import time

from bzt.modules.gatling import GatlingExecutor, DataLogReader, Gatling
from bzt.utils import EXE_SUFFIX
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestGatlingExecutor(BZTestCase):
    def getGatling(self):
        path = os.path.abspath(__dir__() + "/../../build/gatling-taurus/bin/gatling" + EXE_SUFFIX)
        obj = GatlingExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({"path": path})
        return obj

    def test_gatling_mirrors(self):
        path = os.path.abspath(__dir__() + "/../../build/tmp/gatling-taurus/bin/gatling" + EXE_SUFFIX)
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)
        obj = GatlingExecutor()
        gatling_tool = Gatling(path, obj.log, GatlingExecutor.VERSION)
        gatling_tool.install()

    def test_install_Gatling(self):
        path = os.path.abspath(__dir__() + "/../../build/tmp/gatling-taurus/bin/gatling" + EXE_SUFFIX)
        shutil.rmtree(os.path.dirname(os.path.dirname(path)), ignore_errors=True)

        GatlingExecutor.DOWNLOAD_LINK = "file:///" + __dir__() + "/../data/gatling-dist-{version}_{version}.zip"
        GatlingExecutor.VERSION = '2.1.4'
        GatlingExecutor.MIRRORS_SOURCE = "file:///" + __dir__() + "/../data/unicode_file"

        self.assertFalse(os.path.exists(path))
        obj = self.getGatling()
        obj.settings.merge({"path": path})

        obj.execution.merge({"scenario": {"script": __dir__() + "/../gatling/BasicSimulation.scala",
                                          "simulation": "mytest.BasicSimulation"}})
        obj.prepare()
        self.assertTrue(os.path.exists(path))

    def test_gatling_widget(self):
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../gatling/BasicSimulation.scala"}})
        obj.prepare()
        obj.get_widget()
        self.assertEqual(obj.widget.widgets[0].text, "Script: BasicSimulation.scala")

    def test_resource_files_collection_remote(self):
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../gatling/LocalBasicSimulation.scala"}})
        res_files = obj.resource_files()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(res_files), 14)  # file "gatling_" will be not found
        self.assertEqual(len(artifacts), 0)

    def test_resource_files_collection_local(self):
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../gatling/LocalBasicSimulation.scala"}})
        obj.prepare()
        artifacts = os.listdir(obj.engine.artifacts_dir)
        self.assertEqual(len(artifacts), 0)

    def test_requests_1(self):
        obj = self.getGatling()
        obj.execution.merge({
            "concurrency": 10,
            "iterations": 5,
            "scenario": {
                "think-time": 1,
                "default-address": "blazedemo.com",
                "headers": {'H1': 'V1'},
                "requests": [{'url': '/reserve.php',
                              'headers': {'H2': 'V2'},
                              'method': 'POST',
                              'body': 'Body Content',
                              'assert': [{
                                  'contains': ['bootstrap.min'],
                                  'not': True
                              }]},
                             {'url': '/'}]
            }
        })
        obj.prepare()
        scala_file = obj.engine.artifacts_dir + '/' + obj.get_scenario().get('simulation') + '.scala'
        self.assertEqualFiles(__dir__() + "/../gatling/generated1.scala", scala_file)

    def test_requests_2(self):
        obj = self.getGatling()
        obj.execution.merge({
            "concurrency": 10,
            "hold-for": 110,
            "ramp-up": 30,
            "scenario": {
                'keepalive': False,
                'timeout': '100ms',
                'requests': ['http://blazedemo.com', 'google.com']
            }
        })
        obj.prepare()

        scala_file = obj.engine.artifacts_dir + '/' + obj.get_scenario().get('simulation') + '.scala'
        self.assertEqualFiles(__dir__() + "/../gatling/generated2.scala", scala_file)

    def test_requests_3(self):
        obj = self.getGatling()
        obj.execution.merge({
            "iterations": 55,
            "scenario": {
                "default-address": "blazedemo.com",
                "requests": [{'url': '/reserve.php',
                              'assert': [{
                                  'contains': [200],
                                  'subject': 'http-code',
                                  'not': False
                              }]}]
            }
        })
        obj.prepare()
        scala_file = obj.engine.artifacts_dir + '/' + obj.get_scenario().get('simulation') + '.scala'
        self.assertEqualFiles(__dir__() + "/../gatling/generated3.scala", scala_file)

    def test_requests_4(self):
        obj = self.getGatling()
        obj.execution.merge({
            "iterations": 55,
            "scenario": {
                "default-address": "blazedemo.com",
                "requests": [{'url': '/reserve.php',
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
        self.assertEqualFiles(__dir__() + "/../gatling/generated4.scala", scala_file)

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
        self.assertRaises(ValueError, obj.prepare)

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
        obj.execution.merge({"scenario": {"script": __dir__() + "/../gatling/bs/BasicSimulation.scala"}})
        obj.prepare()
        self.assertRaises(RuntimeWarning, obj.post_process)

    def test_no_simulation(self):
        obj = self.getGatling()
        obj.execution.merge({"scenario": {"script": __dir__() + "/../gatling/bs/BasicSimulation.scala"}})
        obj.prepare()
        self.assertRaises(ValueError, obj.startup)

    def test_full_Gatling(self):
        obj = self.getGatling()
        obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../gatling/bs/BasicSimulation.scala",
                "simulation": "fake"
            }
        })
        obj.prepare()

        obj.settings.merge({"path": __dir__() + "/../gatling/gatling" + EXE_SUFFIX})

        try:
            obj.startup()

            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()

    def test_interactive_request(self):
        obj = self.getGatling()
        obj.engine.existing_artifact(__dir__() + "/../gatling/SimpleSimulation.scala")
        obj.execution.merge({
            "scenario": {
                "script": obj.engine.artifacts_dir + "/SimpleSimulation.scala",
                "simulation": "SimpleSimulation"}})
        obj.prepare()
        obj.settings.merge({"path": __dir__() + "/../gatling/gatling" + EXE_SUFFIX})
        counter1 = 0
        try:
            obj.startup()
            while not obj.check():
                time.sleep(obj.engine.check_interval)
                counter1 += 1
            obj.shutdown()
            obj.post_process()
        except RuntimeWarning:
            pass

        obj = self.getGatling()
        obj.engine.existing_artifact(__dir__() + "/../gatling/SimpleSimulation.scala")
        obj.engine.existing_artifact(__dir__() + "/../gatling/generated1.scala")
        obj.execution.merge({
            "scenario": {
                "script": obj.engine.artifacts_dir + "/SimpleSimulation.scala",
                "simulation": "fake"}})
        obj.prepare()
        obj.settings.merge({"path": __dir__() + "/../gatling/gatling" + EXE_SUFFIX})
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
        except ValueError:
            return
        self.fail('ValueError not found')


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = os.path.join(os.path.dirname(__file__), '..', 'gatling')
        obj = DataLogReader(log_path, logging.getLogger(''))
        list_of_values = list(obj.datapoints(True))
        self.assertEqual(len(list_of_values), 23)
