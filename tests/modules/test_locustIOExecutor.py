import logging
import sys
import time

import os
import unittest

try:
    import unittest.mock as mock
except ImportError:
    import mock

from bzt import six, ToolError
from bzt.modules.jmeter import JTLReader
from bzt.six import PY2
from bzt.utils import dehumanize_time
from tests import BZTestCase, __dir__, RESOURCES_DIR

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.locustio import LocustIOExecutor, SlavesReader
from bzt.modules.provisioning import Local
from tests.mocks import EngineEmul


class TestLocustIOExecutor(BZTestCase):
    def setUp(self):
        super(TestLocustIOExecutor, self).setUp()
        sys.path.append(RESOURCES_DIR + "locust/")
        self.obj = LocustIOExecutor()
        self.obj.engine = EngineEmul()
        self.obj.env = self.obj.engine.env
        self.obj.engine.config['provisioning'] = 'local'

    def test_simple(self):
        if six.PY3:
            logging.warning("No locust available for python 3")
        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": RESOURCES_DIR + "locust/simple.py"
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
                "script": RESOURCES_DIR + "locust/simple.py"
            }
        })

        self.obj.prepare()
        self.obj.startup()
        self.obj.get_widget()
        self.obj.check()
        self.assertEqual(self.obj.widget.duration, 30)
        self.assertTrue(self.obj.widget.widgets[0].text.endswith("simple.py"))
        self.obj.shutdown()

    def test_locust_fractional_hatch_rate(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        test_concurrency, test_ramp_up = 4, "60s"
        expected_hatch_rate = test_concurrency / dehumanize_time(test_ramp_up)

        self.obj.execution.merge({
            "concurrency": test_concurrency,
            "ramp-up": test_ramp_up,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": RESOURCES_DIR + "locust/simple.py"
            }
        })

        self.obj.prepare()
        with mock.patch('bzt.modules.locustio.LocustIOExecutor.execute') as m:
            self.obj.startup()
            # Extract the hatch-rate cmdline arg that bzt passed to locust.
            hatch = [
                x.split('=')[1] for x in m.call_args[0][0]
                if x.startswith("--hatch-rate")
            ]
            self.assertEqual(hatch[0], "%f" % expected_hatch_rate)

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
                "script": RESOURCES_DIR + "locust/simple.py"
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

        obj = SlavesReader(RESOURCES_DIR + "locust/locust-slaves.ldjson", 2, logging.getLogger(""))
        points = [x for x in obj.datapoints(True)]
        self.assertEquals(107, len(points))
        for point in points:
            self.assertGreater(point[DataPoint.CURRENT][''][KPISet.AVG_RESP_TIME], 0)
            self.assertGreater(point[DataPoint.CURRENT][''][KPISet.BYTE_COUNT], 0)

    def test_locust_slave_results_errors(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        obj = SlavesReader(RESOURCES_DIR + "locust/locust-slaves2.ldjson", 2, logging.getLogger(""))
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
                "script": RESOURCES_DIR + "locust/simple.py"
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
                "script": RESOURCES_DIR + "locust/simple.py"
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
        self.assertFilesEqual(RESOURCES_DIR + "locust/generated_from_requests_1.py", self.obj.script)

    def test_build_script_none_def_addr(self):
        self.sniff_log(self.obj.log)
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
        self.obj.startup()
        self.obj.shutdown()
        debug_buff = self.log_recorder.debug_buff.getvalue()
        self.assertNotIn("'--host='", debug_buff)

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

    @unittest.skipUnless(PY2, "Locust is having issues with py3")
    def test_jtl_quoting_issue(self):
        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 1,
            "scenario": {
                "default-address": "http://httpbin.org/status/503",
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
        self.assertTrue(os.path.exists(kpi_path))

        reader = JTLReader(kpi_path, self.obj.log, None)
        for point in reader.datapoints():
            pass

    def test_diagnostics(self):
        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 1,
            "scenario": {
                "default-address": "http://httpbin.org/status/503",
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
        diagnostics = self.obj.get_error_diagnostics()
        self.assertIsNotNone(diagnostics)
