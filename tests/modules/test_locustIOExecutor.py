import logging
import sys
import time

import os
from bzt import six, ToolError
from tests import BZTestCase, __dir__

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.locustio import LocustIOExecutor, SlavesReader
from bzt.modules.provisioning import Local
from tests.mocks import EngineEmul


class TestLocustIOExecutor(BZTestCase):
    def setUp(self):
        super(TestLocustIOExecutor, self).setUp()
        sys.path.append(__dir__() + "/../resources/locust/")
        self.obj = LocustIOExecutor()
        self.obj.engine = EngineEmul()
        self.obj.engine.config['provisioning'] = 'local'

    def test_simple(self):
        if six.PY3:
            logging.warning("No locust available for python 3")
        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": __dir__() + "/../resources/locust/simple.py"
            }
        })
        self.obj.prepare()
        self.obj.startup()
        try:
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        except RuntimeError:  # FIXME: not good, but what to do?
            pass
        self.obj.shutdown()
        self.assertFalse(self.obj.has_results())

    def test_locust_widget(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": __dir__() + "/../resources/locust/simple.py"
            }
        })

        self.obj.prepare()
        self.obj.startup()
        self.obj.get_widget()
        self.obj.check()
        self.assertEqual(self.obj.widget.duration, 30)
        self.assertTrue(self.obj.widget.widgets[0].text.endswith("simple.py"))
        self.obj.shutdown()

    def test_locust_master(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "master": True,
            "slaves": 1,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": __dir__() + "/../resources/locust/simple.py"
            }
        })

        self.obj.prepare()
        self.obj.startup()
        self.obj.get_widget()
        try:
            self.obj.check()
            time.sleep(2)
            self.obj.check()
        except RuntimeError:
            logging.warning("Do you use patched locust for non-GUI master?")
        self.obj.shutdown()
        self.obj.post_process()
        self.assertFalse(self.obj.has_results())

    def test_locust_slave_results(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        obj = SlavesReader(__dir__() + "/../resources/locust/locust-slaves.ldjson", 2, logging.getLogger(""))
        points = [x for x in obj.datapoints(True)]
        self.assertEquals(107, len(points))
        for point in points:
            self.assertGreater(point[DataPoint.CURRENT][''][KPISet.AVG_RESP_TIME], 0)
            self.assertGreater(point[DataPoint.CURRENT][''][KPISet.BYTE_COUNT], 0)

    def test_locust_slave_results_errors(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        obj = SlavesReader(__dir__() + "/../resources/locust/locust-slaves2.ldjson", 2, logging.getLogger(""))
        points = [x for x in obj.datapoints(True)]
        self.assertEquals(60, len(points))
        for point in points:
            self.assertEquals(len(point[DataPoint.CURRENT][''][KPISet.ERRORS]), 1)
            self.assertGreaterEqual(point[DataPoint.CURRENT][''][KPISet.FAILURES], 70)

    def test_locust_resource_files(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": __dir__() + "/../resources/locust/simple.py"
            }
        })
        resource_files = self.obj.resource_files()
        self.assertEqual(1, len(resource_files))

    def test_resource_files_requests(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "requests": [
                    "/",
                ]
            }
        })
        resource_files = self.obj.resource_files()
        self.assertEqual(0, len(resource_files))

    def test_fail_on_zero_results(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": __dir__() + "/../resources/locust/simple.py"
            }
        })
        self.obj.prepare()
        self.obj.engine.prepared = [self.obj]
        self.obj.engine.started = [self.obj]
        prov = Local()
        prov.engine = self.obj.engine
        prov.executors = [self.obj]
        self.obj.engine.provisioning = prov
        self.assertRaises(ToolError, self.obj.engine.provisioning.post_process)

    def test_requests_minimal(self):
        self.obj.engine.config.merge({
            "execution": [{
                "executor": "locust",
                "scenario": {
                    "requests": ["http://blazedemo.com/"]}}]})

        self.obj.execution = self.obj.engine.config.get('execution')[0]
        self.obj.prepare()
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

    def test_build_script(self):
        self.obj.engine.config.merge({
            "execution": [{
                "executor": "locust",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "keepalive": False,
                    "think-time": "5s",
                    "default-address": "http://blazedemo.com",
                    "headers": {
                        "Keep-Alive": "timeout=15, max=100",
                    },
                    "requests": [{
                        "url": "/",
                        "method": "GET",
                        "headers": {'var2': 'val2'},
                        "assert": [
                            {
                                'subject': 'body',
                                'contains': ['text1', 'text2'],
                                'regexp': False
                            },
                            'enigma for body',
                            {
                                'subject': 'http-code',
                                'contains': 200,
                                'not': True}]
                    }, {
                        "url": "/page",
                        "timeout": "1s500ms",
                        "think-time": '1s',
                        "method": "POST",
                        "body": {'var1': 'val1'},
                        "assert": [{
                            'subject': 'body',
                            'contains': '\w+l1e'}]}]}}})

        self.obj.execution = self.obj.engine.config.get('execution')[0]
        self.obj.prepare()
        self.assertFilesEqual(__dir__() + "/../resources/locust/generated_from_requests_1.py", self.obj.script)

    def test_build_script_none_def_addr(self):
        self.obj.engine.config.merge({
            "execution": [{
                "executor": "locust",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "url": "http://blazedemo.com"}]}}})

        self.obj.execution = self.obj.engine.config.get('execution')[0]
        self.obj.prepare()
        self.assertFilesEqual(__dir__() + "/../resources/locust/generated_from_requests_2.py", self.obj.script)

    def test_jtl_key_order(self):
        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 1,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "requests": [
                    "/"
                ]
            }
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.shutdown()
        self.obj.post_process()

        kpi_path = os.path.join(self.obj.engine.artifacts_dir, "kpi.jtl")
        if os.path.exists(kpi_path):
            with open(kpi_path) as fds:
                jtl = fds.readlines()

            header_line = jtl[0].strip()
            expected = "timeStamp,label,method,elapsed,bytes,responseCode,responseMessage,success,allThreads,Latency"
            self.assertEqual(header_line, expected)
