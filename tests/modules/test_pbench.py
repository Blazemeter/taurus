import io
import math
import pprint
import time
import unittest

import urwid
import yaml

from os.path import join
from bzt import TaurusConfigError, ToolError
from bzt.engine import EXEC
from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet, AggregatorListener
from bzt.modules.pbench import PBenchExecutor, Scheduler, TaurusPBenchGenerator
from bzt.six import parse, b
from bzt.utils import is_windows
from tests import RESOURCES_DIR, close_reader_file, ROOT_LOGGER
from tests.cases import ExecutorTestCase


class DataPointLogger(AggregatorListener):
    def aggregated_second(self, data):
        current = data[DataPoint.CURRENT]['']
        ROOT_LOGGER.info(
            "DataPoint %s: VU:%s RPS:%s/%s RT:%s",
            data[DataPoint.TIMESTAMP],
            current[KPISet.CONCURRENCY],
            current[KPISet.SAMPLE_COUNT],
            current[KPISet.FAILURES],
            current[KPISet.AVG_RESP_TIME])


@unittest.skipIf(is_windows(), "Disabled on Windows")
class TestPBench(ExecutorTestCase):
    EXECUTOR = PBenchExecutor

    def configure(self, config):
        self.obj.settings.merge({"path": join(RESOURCES_DIR, "pbench", "phantom.sh")})
        super(TestPBench, self).configure(config)

    def tearDown(self):
        if self.obj.reader:
            close_reader_file(self.obj.reader)
            close_reader_file(self.obj.reader.stats_reader)
        super(TestPBench, self).tearDown()


class TestPBenchExecutor(TestPBench):
    def test_simple(self):
        self.configure({
            "provisioning": "test",
            EXEC: {
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
                        {
                            "url": "/api",
                            "method": "POST",
                            "headers": {
                                "Content-Length": 0
                            },
                            "body": {
                                "param": "value"}}]}}})

        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj.engine.aggregator.engine = self.obj.engine
        self.obj.engine.aggregator.add_listener(DataPointLogger())

        self.obj.engine.aggregator.prepare()
        self.obj.prepare()

        self.obj.engine.aggregator.startup()
        self.obj.startup()

        while not self.obj.check():
            ROOT_LOGGER.debug("Running...")
            self.obj.engine.aggregator.check()
            time.sleep(0.1)

        self.obj.shutdown()
        self.obj.engine.aggregator.shutdown()

        self.obj.post_process()
        self.obj.engine.aggregator.post_process()

    def test_widget(self):
        self.configure({
            "provisioning": "test",
            EXEC: [
                {
                    "throughput": 10,
                    "hold-for": 30,
                    "scenario": {
                        "default-address": "http://blazedemo.com/",
                        "requests": ["/"]}}]})
        self.obj.prepare()
        self.obj.startup()
        self.obj.get_widget()
        self.assertTrue(isinstance(self.obj.widget.progress, urwid.ProgressBar))
        self.assertEqual(self.obj.widget.duration, 30)
        self.assertEqual(self.obj.widget.widgets[0].text, "Pbench: http://blazedemo.com:80")
        self.obj.check()
        self.obj.shutdown()

    def test_improved_request_building(self):
        config = RESOURCES_DIR + "yaml/phantom_improved_request.yml"
        self.configure(yaml.full_load(open(config).read()))
        self.obj.prepare()
        with open(self.obj.generator.schedule_file) as fds:
            config = fds.readlines()

        get_requests = [req_str.split(" ")[1] for req_str in config if req_str.startswith("GET")]
        self.assertEqual(len(get_requests), 2)

        for get_req in get_requests:
            self.assertEqual(dict(parse.parse_qsl(parse.urlsplit(get_req).query)),
                             {"get_param1": "value1", "get_param2": "value2"})

    def test_same_address_port(self):
        config = RESOURCES_DIR + "yaml/phantom_request_same_address.yml"
        self.configure(yaml.full_load(open(config).read()))
        self.assertRaises(TaurusConfigError, self.obj.prepare)

    def test_install_pbench(self):
        self.obj.settings.merge({"path": "/notexistent"})
        self.assertRaises(ToolError, self.obj.prepare)

    def test_pbench_file_lister(self):
        self.configure({'execution': {"executor": "pbench", "scenario": {"script": "script.src"}}})
        resource_files = self.obj.resource_files()
        self.assertEqual(1, len(resource_files))
        self.assertEqual(resource_files[0], 'script.src')

    def test_pbench_script(self):
        self.configure({
            EXEC: {
                "executor": "pbench",
                "scenario": {"script": join(RESOURCES_DIR, "pbench", "pbench.src")}
            },
            "provisioning": "test"
        })
        self.obj.prepare()
        self.assertEquals("/usr/lib/phantom", self.obj.generator.modules_path)

    def test_pbench_payload_relpath(self):
        """Verify that enhanced pbench preserves relative script path"""
        script_path = join(RESOURCES_DIR, "pbench", "pbench.src")
        self.configure({
            EXEC: {
                "executor": "pbench",
                "scenario": {"script": script_path}
            },
            "provisioning": "test",
        })
        self.obj.settings.merge({"enhanced": True})
        self.obj.prepare()

        pbench_conf = join(self.obj.engine.artifacts_dir, "pbench.conf")
        with open(pbench_conf) as conf_fds:
            config = conf_fds.read()
            self.assertIn(script_path, config)

    def test_pbench_payload_py3_crash(self):
        self.configure({
            EXEC: {
                "executor": "pbench",
                "scenario": {"requests": ["test%d" % i for i in range(20)]}
            },
            "provisioning": "test",
        })
        self.obj.prepare()

    def test_diagnostics(self):
        self.configure({
            "provisioning": "test",
            EXEC: [
                {
                    "throughput": 10,
                    "hold-for": 30,
                    "scenario": {
                        "default-address": "http://blazedemo.com/",
                        "requests": ["/"]}}]})
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
        scheduler = Scheduler(self.obj.get_load(), filename, ROOT_LOGGER)
        scheduler.payload_file.fds = MockByteFile(buf)
        return scheduler

    def test_schedule_rps(self):
        rps = 9
        rampup = 12
        self.configure({
            "provisioning": "test",
            EXEC: {"throughput": rps, "ramp-up": rampup, "steps": 3, "hold-for": 0}})
        scheduler = self.get_scheduler(b("4 test\ntest\n"))

        cnt = 0
        cur = 0
        currps = 0
        for item in scheduler.generate():
            if int(math.ceil(item[0])) != cur:
                # self.assertLessEqual(currps, rps)
                cur = int(math.ceil(item[0]))
                ROOT_LOGGER.debug("RPS: %s", currps)
                currps = 0

            cnt += 1
            currps += 1

        ROOT_LOGGER.debug("RPS: %s", currps)

    def test_schedule_with_no_rampup(self):
        self.configure({EXEC: {"concurrency": 10, "ramp-up": None, "steps": 3, "hold-for": 10}})
        # this line shouln't throw an exception
        self.get_scheduler(b("4 test\ntest\n"))

    def test_schedule_empty(self):
        # concurrency: 1, iterations: 1
        scheduler = self.get_scheduler(b("4 test\ntest\n"))
        items = list(scheduler.generate())
        for item in items:
            ROOT_LOGGER.debug("Item: %s", item)
        self.assertEqual(1, len(items))

    def test_schedule_concurrency(self):
        self.configure({EXEC: {"concurrency": 5, "ramp-up": 10, "hold-for": 5}})
        scheduler = self.get_scheduler(b("5 test1\ntest1\n5 test2\ntest2\n"))
        items = list(scheduler.generate())
        self.assertEqual(8, len(items))
        self.assertEqual(-1, items[5][0])  # instance became unlimited
        self.assertEqual(Scheduler.REC_TYPE_LOOP_START, items[6][5])  # looped payload

    def test_schedule_throughput_only(self):
        self.configure({EXEC: {"throughput": 5}})
        scheduler = self.get_scheduler(b("5 test1\ntest1\n5 test2\ntest2\n"))
        items = list(scheduler.generate())
        self.assertTrue(len(items) > 0)

    def test_schedule_concurrency_steps(self):
        self.configure({EXEC: {"concurrency": 5, "ramp-up": 10, "steps": 3}})
        scheduler = self.get_scheduler(b("5 test1\ntest1\n5 test2\ntest2\n"))
        items = list(scheduler.generate())
        self.assertEqual(8, len(items))
        self.assertEqual(-1, items[5][0])  # instance became unlimited
        self.assertEqual(Scheduler.REC_TYPE_LOOP_START, items[6][5])  # looped payload


class TestSchedulerSize(TestPBench):
    def check_schedule_size_estimate(self, execution):
        self.configure({
            EXEC: execution,
            "provisioning": "local",
        })
        load = self.obj.get_load()
        self.obj.generator = TaurusPBenchGenerator(self.obj, ROOT_LOGGER)
        self.obj.generator.generate_payload(self.obj.get_scenario())
        payload_count = len(self.obj.get_scenario().get('requests', []))
        sch = Scheduler(load, self.obj.generator.payload_file, ROOT_LOGGER)
        estimated_schedule_size = self.obj.generator._estimate_schedule_size(load, payload_count)
        ROOT_LOGGER.debug("Estimated schedule size: %s", estimated_schedule_size)
        items = list(sch.generate())
        actual_schedule_size = len(items)
        ROOT_LOGGER.debug("Actual schedule size: %s", actual_schedule_size)
        if actual_schedule_size != 0:
            error = abs(estimated_schedule_size - actual_schedule_size)
            error_rel = error / float(actual_schedule_size)
            ROOT_LOGGER.debug("Estimation error: %s", error)
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
