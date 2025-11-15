import os
import sys
import time
from unittest import mock

from bzt import ToolError
from bzt.utils import dehumanize_time, EXE_SUFFIX
from bzt.modules.jmeter import JTLReader
from bzt.modules.aggregator import DataPoint, KPISet, ConsolidatingAggregator
from bzt.modules._locustio import LocustIOExecutor, WorkersReader
from bzt.modules.provisioning import Local

from tests.unit import ExecutorTestCase, RESOURCES_DIR, ROOT_LOGGER, EngineEmul


class TestLocustIOExecutor(ExecutorTestCase):
    EXECUTOR = LocustIOExecutor
    CMD_LINE = None

    @classmethod
    def setUpClass(cls):
        sys.path.append(RESOURCES_DIR + "locust/")

    def setUp(self):
        super(TestLocustIOExecutor, self).setUp()
        self.obj.engine.config['provisioning'] = 'local'

    def start_subprocess(self, args, **kwargs):
        self.CMD_LINE = args

    def obj_prepare(self):
        tmp_exec = sys.executable
        try:
            sys.executable = os.path.join(RESOURCES_DIR, "python-pip", 'python-pip' + EXE_SUFFIX)
            self.obj.prepare()
        finally:
            sys.executable = tmp_exec

    def test_simple(self):
        self.configure({"execution": {
            "concurrency": 1,
            "iterations": 10,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": RESOURCES_DIR + "locust/simple.py"
            }
        }})
        self.obj_prepare()
        tmp_exec = sys.executable
        try:
            sys.executable = RESOURCES_DIR + "locust/locust-mock" + EXE_SUFFIX
            self.obj.startup()
        finally:
            sys.executable = tmp_exec

        self.obj.post_process()
        self.assertFalse(self.obj.has_results())

    def test_locust_widget(self):
        self.configure({"execution": {
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": RESOURCES_DIR + "locust/simple.py"
            }
        }})
        self.obj_prepare()
        self.obj.engine.start_subprocess = lambda **kwargs: None
        self.obj.startup()
        self.obj.get_widget()
        self.assertEqual(self.obj.widget.duration, 30)
        self.assertTrue(self.obj.widget.widgets[0].text.endswith("simple.py"))
        self.obj.post_process()

    def test_locust_fractional_hatch_rate(self):
        test_concurrency, test_ramp_up = 4, "60s"
        expected_hatch_rate = test_concurrency / dehumanize_time(test_ramp_up)

        self.configure({"execution": {
            "concurrency": test_concurrency,
            "ramp-up": test_ramp_up,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": RESOURCES_DIR + "locust/simple.py"
            }
        }})
        self.obj_prepare()
        with mock.patch('bzt.modules._locustio.LocustIOExecutor._execute') as m:
            self.obj.startup()
            # Extract the hatch-rate cmdline arg that bzt passed to locust.
            hatch = [
                x.split('=')[1] for x in m.call_args[0][0]
                if x.startswith("--spawn-rate")
            ]
            self.assertEqual(hatch[0], "%f" % expected_hatch_rate)
        self.obj.post_process()

    def test_locust_master(self):
        self.configure({"execution": {
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "master": True,
            "workers": 1,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": RESOURCES_DIR + "locust/simple.py"
            }
        }})
        self.obj_prepare()
        self.obj.engine.start_subprocess = lambda **kwargs: None
        self.obj.startup()
        self.obj.get_widget()
        self.obj.post_process()
        self.assertFalse(self.obj.has_results())

    def test_locust_worker_results(self):
        obj = WorkersReader(RESOURCES_DIR + "locust/locust-workers.ldjson", 2, ROOT_LOGGER)
        points = [x for x in obj.datapoints(True)]
        self.assertEquals(107, len(points))
        for point in points:
            self.assertGreater(point[DataPoint.CURRENT][''][KPISet.AVG_RESP_TIME], 0)
            self.assertGreater(point[DataPoint.CURRENT][''][KPISet.BYTE_COUNT], 0)

    def test_locust_worker_results_errors(self):
        obj = WorkersReader(RESOURCES_DIR + "locust/locust-workers2.ldjson", 2, ROOT_LOGGER)
        points = [x for x in obj.datapoints(True)]
        self.assertEquals(60, len(points))
        for point in points:
            self.assertEquals(len(point[DataPoint.CURRENT][''][KPISet.ERRORS]), 1)
            self.assertGreaterEqual(point[DataPoint.CURRENT][''][KPISet.FAILURES], 70)

    def test_locust_delayed_worker(self):
        obj = WorkersReader(RESOURCES_DIR + "locust/locust-workers-none.ldjson", 2, ROOT_LOGGER)
        points = [x for x in obj.datapoints(True)]
        self.assertEquals(0, len(points))

    def test_locust_resource_files(self):
        self.configure({"execution": {
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": RESOURCES_DIR + "locust/simple.py"
            }
        }})
        resource_files = self.obj.resource_files()
        self.assertEqual(1, len(resource_files))

    def test_worker_aggregation(self):
        self.configure({"execution": {
            "scenario": {"script": RESOURCES_DIR + "locust/simple.py"}}})
        self.obj_prepare()

        self.obj.reader = WorkersReader(RESOURCES_DIR + "locust/locust-workers.ldjson", 2, ROOT_LOGGER)
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.engine.aggregator.engine = EngineEmul()
        self.obj.engine.aggregator.add_underling(self.obj.reader)
        self.assertEqual(107, len(list(self.obj.engine.aggregator.datapoints(final_pass=True))))
        self.obj.post_process()

    def test_resource_files_requests(self):
        self.configure({"execution": {
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "requests": [
                    "/",
                ]
            }
        }})
        resource_files = self.obj.resource_files()
        self.assertEqual(0, len(resource_files))

    def test_fail_on_zero_results(self):
        self.configure({"execution": {
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": RESOURCES_DIR + "locust/simple.py"
            }
        }})
        self.obj_prepare()
        self.obj.engine.prepared = [self.obj]
        self.obj.engine.started = [self.obj]
        prov = Local()
        prov.engine = self.obj.engine
        prov.executors = [self.obj]
        prov.started_modules = [self.obj]
        self.obj.engine.provisioning = prov
        self.assertRaises(ToolError, self.obj.engine.provisioning.post_process)
        self.obj.post_process()

    def test_requests_minimal(self):
        self.configure({"execution": {
            "executor": "locust",
            "scenario": {
                "requests": ["http://blazedemo.com/"]}}})
        self.obj_prepare()
        self.obj.post_process()

    def test_build_script(self):
        self.configure({
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
                            'contains': r'\w+l1e'}]}]}}})
        self.obj_prepare()
        self.assertFilesEqual(RESOURCES_DIR + "locust/generated_from_requests.py", self.obj.script)
        self.obj.post_process()

    def test_build_script_none_def_addr(self):
        self.sniff_log(self.obj.log)
        self.configure({
            "execution": [{
                "executor": "locust",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "url": "http://blazedemo.com"}]}}})
        self.obj_prepare()
        debug_buff = self.log_recorder.debug_buff.getvalue()
        self.assertNotIn("'--host='", debug_buff)
        self.obj.post_process()

    def test_jtl_key_order(self):
        self.configure({"execution": {
            "concurrency": 1,
            "iterations": 1,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "requests": [
                    "/"
                ]
            }
        }})
        self.obj_prepare()
        self.obj.post_process()

        kpi_path = os.path.join(self.obj.engine.artifacts_dir, "kpi.jtl")
        if os.path.exists(kpi_path):
            with open(kpi_path) as fds:
                jtl = fds.readlines()

            header_line = jtl[0].strip()
            expected = "timeStamp,label,method,elapsed,bytes,responseCode,responseMessage,success,allThreads,Latency"
            self.assertEqual(header_line, expected)

    def test_jtl_quoting_issue(self):
        def exec_and_communicate(*args, **kwargs):
            return "", ""

        self.configure({"execution": {
            "concurrency": 1,
            "iterations": 1,
            "scenario": {
                "default-address": "http://httpbin.org/status/503",
                "requests": [
                    "/"
                ]
            }
        }})
        self.obj_prepare()
        tmp_exec = sys.executable
        try:
            sys.executable = RESOURCES_DIR + "locust/locust-mock" + EXE_SUFFIX
            self.obj.startup()
        finally:
            sys.executable = tmp_exec

        while not self.obj.check():
            time.sleep(self.obj.engine.check_interval)
        self.obj.post_process()

        kpi_path = RESOURCES_DIR + "locust/locust-kpi.jtl"

        reader = JTLReader(kpi_path, self.obj.log)
        list(reader.datapoints())

    def test_diagnostics(self):
        self.configure({"execution": {
            "concurrency": 1,
            "iterations": 1,
            "scenario": {
                "default-address": "http://httpbin.org/status/503",
                "requests": [
                    "/"
                ]
            }
        }})
        self.obj_prepare()
        self.obj.post_process()
        diagnostics = self.obj.get_error_diagnostics()
        self.assertIsNotNone(diagnostics)
