import io
import logging
import math
import pprint
import time
import unittest

import urwid
import yaml


from os.path import join
from bzt import TaurusConfigError, ToolError
from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet, AggregatorListener
from bzt.modules.pbench import PBenchExecutor, Scheduler, TaurusPBenchTool
from bzt.six import parse, b
from bzt.utils import is_windows
from tests import BZTestCase, RESOURCES_DIR, close_reader_file
from tests.mocks import EngineEmul


def get_pbench():
    obj = PBenchExecutor()
    obj.engine = EngineEmul()
    obj.env = obj.engine.env
    obj.settings.merge({"path": join(RESOURCES_DIR, "pbench", "phantom.sh")})
    return obj


class DataPointLogger(AggregatorListener):
    def aggregated_second(self, data):
        current = data[DataPoint.CURRENT]['']
        logging.info("DataPoint %s: VU:%s RPS:%s/%s RT:%s", data[DataPoint.TIMESTAMP],
                     current[KPISet.CONCURRENCY], current[KPISet.SAMPLE_COUNT], current[KPISet.FAILURES],
                     current[KPISet.AVG_RESP_TIME])


@unittest.skipIf(is_windows(), "Disabled on Windows")
class TestPBench(BZTestCase):
    def setUp(self):
        super(TestPBench, self).setUp()
        self.obj = get_pbench()

    def tearDown(self):
        if self.obj.pbench:
            if self.obj.pbench.stdout_file:
                self.obj.pbench.stdout_file.close()
            if self.obj.pbench.stderr_file:
                self.obj.pbench.stderr_file.close()

        if self.obj.reader:
            close_reader_file(self.obj.reader)
            close_reader_file(self.obj.reader.stats_reader)
        super(TestPBench, self).tearDown()


class TestPBenchExecutor(TestPBench):
    def test_simple(self):
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.engine.aggregator.add_listener(DataPointLogger())
        self.obj.engine.config.merge({"provisioning": "test"})

        self.obj.execution.merge({
            "log-responses": "proto_error",
            # "iterations": 5000000,
            "concurrency": 10,
            "throughput": 1000,
            "ramp-up": "1m",
            # "steps": 5,
            "hold-for": "15",
            "scenario": {
                "timeout": 1,
                "default-address": "http://localhost:33",
                "headers": {
                    "Connection": "close"
                },
                "requests": [
                    # "/",
                    {
                        "url": "/api",
                        "method": "POST",
                        "headers": {
                            "Content-Length": 0
                        },
                        "body": {
                            "param": "value"}}]}})
        self.obj.engine.aggregator.prepare()
        self.obj.prepare()

        self.obj.engine.aggregator.startup()
        self.obj.startup()

        while not self.obj.check():
            logging.debug("Running...")
            self.obj.engine.aggregator.check()
            time.sleep(0.1)

        self.obj.shutdown()
        self.obj.engine.aggregator.shutdown()

        self.obj.post_process()
        self.obj.engine.aggregator.post_process()

    def test_widget(self):
        self.obj.engine.config.merge({
            "provisioning": "test",
            ScenarioExecutor.EXEC: [
                {
                    "throughput": 10,
                    "hold-for": 30,
                    "scenario": {
                        "default-address": "http://blazedemo.com/",
                        "requests": ["/"]}}]})
        self.obj.execution = self.obj.engine.config['execution'][0]
        self.obj.prepare()
        self.obj.startup()
        self.obj.get_widget()
        self.assertTrue(isinstance(self.obj.widget.progress, urwid.ProgressBar))
        self.assertEqual(self.obj.widget.duration, 30)
        self.assertEqual(self.obj.widget.widgets[0].text, "Pbench: http://blazedemo.com:80")
        self.obj.check()
        self.obj.shutdown()

    def test_improved_request_building(self):
        self.obj.engine.config.merge(yaml.load(
            open(RESOURCES_DIR + "yaml/phantom_improved_request.yml").read()))
        self.obj.execution = self.obj.engine.config['execution'][0]
        self.obj.prepare()
        with open(self.obj.pbench.schedule_file) as fds:
            config = fds.readlines()

        get_requests = [req_str.split(" ")[1] for req_str in config if req_str.startswith("GET")]
        self.assertEqual(len(get_requests), 2)

        for get_req in get_requests:
            self.assertEqual(dict(parse.parse_qsl(parse.urlsplit(get_req).query)),
                             {"get_param1": "value1", "get_param2": "value2"})

    def test_same_address_port(self):
        self.obj.engine.config.merge(
            yaml.load(open(RESOURCES_DIR + "yaml/phantom_request_same_address.yml").read()))
        self.obj.execution = self.obj.engine.config['execution'][0]
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_install_pbench(self):
        self.obj.settings.merge({"path": "/notexistent"})
        # self.obj.execution = self.obj.engine.config['execution'][0]
        try:
            self.obj.prepare()
            self.fail()
        except ToolError as exc:
            self.assertEquals("Please install PBench tool manually", str(exc))

    def test_pbench_file_lister(self):
        self.obj.engine.config.merge(
            {'execution': {"executor": "pbench", "scenario": {"script": "script.src"}}})
        self.obj.execution = self.obj.engine.config['execution']
        resource_files = self.obj.resource_files()
        self.assertEqual(1, len(resource_files))
        self.assertEqual(resource_files[0], 'script.src')

    def test_pbench_script(self):
        self.obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "pbench",
                "scenario": {"script": join(RESOURCES_DIR, "pbench", "pbench.src")}
            },
            "provisioning": "test"
        })
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()

    def test_pbench_payload_relpath(self):
        """Verify that enhanced pbench preserves relative script path"""
        script_path = join(RESOURCES_DIR, "pbench", "pbench.src")
        self.obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "pbench",
                "scenario": {"script": script_path}
            },
            "provisioning": "test",
        })
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.settings.merge({"enhanced": True})
        self.obj.prepare()

        pbench_conf = join(self.obj.engine.artifacts_dir, "pbench.conf")
        with open(pbench_conf) as conf_fds:
            config = conf_fds.read()
            self.assertIn(script_path, config)

    def test_pbench_payload_py3_crash(self):
        self.obj.engine.config.merge({
            ScenarioExecutor.EXEC: {
                "executor": "pbench",
                "scenario": {"requests": ["test%d" % i for i in range(20)]}
            },
            "provisioning": "test",
        })
        self.obj.execution = self.obj.engine.config['execution']
        self.obj.prepare()

    def test_diagnostics(self):
        self.obj.engine.config.merge({
            "provisioning": "test",
            ScenarioExecutor.EXEC: [
                {
                    "throughput": 10,
                    "hold-for": 30,
                    "scenario": {
                        "default-address": "http://blazedemo.com/",
                        "requests": ["/"]}}]})
        self.obj.execution = self.obj.engine.config['execution'][0]
        self.obj.prepare()
        self.obj.startup()
        for _ in range(3):
            self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()
        self.assertIsNotNone(self.obj.get_error_diagnostics())


class MockByteFile(io.BytesIO):
    def __init__(self, buf=None):
        super(MockByteFile, self).__init__(buf)
        self.name = "taurus_test"


class TestScheduler(TestPBench):
    def get_scheduler(self, buf=None):
        filename = ''
        scheduler = Scheduler(self.obj.get_load(), filename, logging.getLogger(""))
        scheduler.payload_file.fds = MockByteFile(buf)
        return scheduler

    def test_schedule_rps(self):
        self.obj.engine.config.merge({"provisioning": "test"})
        rps = 9
        rampup = 12
        self.obj.execution.merge({"throughput": rps, "ramp-up": rampup, "steps": 3, "hold-for": 0})
        scheduler = self.get_scheduler(b("4 test\ntest\n"))

        cnt = 0
        cur = 0
        currps = 0
        for item in scheduler.generate():
            # logging.debug("Item: %s", item)
            if int(math.ceil(item[0])) != cur:
                # self.assertLessEqual(currps, rps)
                cur = int(math.ceil(item[0]))
                logging.debug("RPS: %s", currps)
                currps = 0

            cnt += 1
            currps += 1

        logging.debug("RPS: %s", currps)

    def test_schedule_with_no_rampup(self):
        self.obj.execution.merge({"concurrency": 10, "ramp-up": None, "steps": 3, "hold-for": 10})
        # this line shouln't throw an exception
        self.get_scheduler(b("4 test\ntest\n"))

    def test_schedule_empty(self):
        # concurrency: 1, iterations: 1
        scheduler = self.get_scheduler(b("4 test\ntest\n"))
        items = list(scheduler.generate())
        for item in items:
            logging.debug("Item: %s", item)
        self.assertEqual(1, len(items))

    def test_schedule_concurrency(self):
        self.obj.execution.merge({"concurrency": 5, "ramp-up": 10, "hold-for": 5})
        scheduler = self.get_scheduler(b("5 test1\ntest1\n5 test2\ntest2\n"))
        items = list(scheduler.generate())
        self.assertEqual(8, len(items))
        self.assertEqual(-1, items[5][0])  # instance became unlimited
        self.assertEqual(Scheduler.REC_TYPE_LOOP_START, items[6][5])  # looped payload

    def test_schedule_throughput_only(self):
        self.obj.execution.merge({"throughput": 5})
        scheduler = self.get_scheduler(b("5 test1\ntest1\n5 test2\ntest2\n"))
        items = list(scheduler.generate())
        self.assertTrue(len(items) > 0)

    def test_schedule_concurrency_steps(self):
        self.obj.execution.merge({"concurrency": 5, "ramp-up": 10, "steps": 3})
        scheduler = self.get_scheduler(b("5 test1\ntest1\n5 test2\ntest2\n"))
        items = list(scheduler.generate())
        self.assertEqual(8, len(items))
        self.assertEqual(-1, items[5][0])  # instance became unlimited
        self.assertEqual(Scheduler.REC_TYPE_LOOP_START, items[6][5])  # looped payload


class TestSchedulerSize(TestPBench):
    def check_schedule_size_estimate(self, execution):
        self.obj.engine.config.merge({
            ScenarioExecutor.EXEC: execution,
            "provisioning": "local",
        })
        self.obj.execution = self.obj.engine.config['execution']
        load = self.obj.get_load()
        self.obj.pbench = TaurusPBenchTool(self.obj, logging.getLogger(''))
        self.obj.pbench.generate_payload(self.obj.get_scenario())
        payload_count = len(self.obj.get_scenario().get('requests', []))
        sch = Scheduler(load, self.obj.pbench.payload_file, logging.getLogger(''))
        estimated_schedule_size = self.obj.pbench._estimate_schedule_size(load, payload_count)
        logging.debug("Estimated schedule size: %s", estimated_schedule_size)
        items = list(sch.generate())
        actual_schedule_size = len(items)
        logging.debug("Actual schedule size: %s", actual_schedule_size)
        if actual_schedule_size != 0:
            error = abs(estimated_schedule_size - actual_schedule_size)
            error_rel = error / float(actual_schedule_size)
            logging.debug("Estimation error: %s", error)
            if error_rel >= 0.1:
                self.fail("Estimation failed (error=%s) on config %s" % (error_rel, pprint.pformat(execution)))

    def test_est_conc_requests(self):
        self.check_schedule_size_estimate({
            "concurrency": 10,
            "scenario": {
                "requests": ["test1", "test2", "test3"]}})

    def test_est_conc_iterations(self):
        self.check_schedule_size_estimate({
            "concurrency": 10,
            "iterations": 3,
            "scenario": {
                "requests": ["test1", "test2", "test3"]}})

    def test_est_conc_holdfor(self):
        self.check_schedule_size_estimate({
            "concurrency": 10,
            "hold-for": "10s",
            "scenario": {
                "requests": ["test1", "test2", "test3"]}})

    def test_est_conc_rampup(self):
        self.check_schedule_size_estimate({
            "concurrency": 5,
            "ramp-up": "20s",
            "scenario": {
                "requests": ["test1", "test2", "test3"]}})

    def test_est_tput_requests(self):
        self.check_schedule_size_estimate({
            "throughput": 100,
            "scenario": {
                "requests": ["test1", "test2", "test3"]}})

    def test_est_tput_iterations(self):
        self.check_schedule_size_estimate({
            "throughput": 100,
            "iterations": 10,
            "scenario": {
                "requests": ["test1", "test2", "test3"]}})

    def test_est_tput_hold(self):
        self.check_schedule_size_estimate({
            "throughput": 100,
            "hold-for": "20s",
            "scenario": {
                "requests": ["test1", "test2"]}})

    def test_est_tput_hold_iterations(self):
        self.check_schedule_size_estimate({
            "throughput": 100,
            "iterations": 10,
            "hold-for": "10s",
            "scenario": {
                "requests": ["test1", "test2", "test3"]}})

    def test_est_tput_rampup(self):
        self.check_schedule_size_estimate({
            "throughput": 100,
            "ramp-up": "10s",
            "scenario": {
                "requests": ["test1", "test2"]}})

    def test_est_tput_rampup_iterations(self):
        self.check_schedule_size_estimate({
            "throughput": 100,
            "ramp-up": "10s",
            "iterations": 20,
            "scenario": {
                "requests": ["test1", "test2", "test3"]}})
