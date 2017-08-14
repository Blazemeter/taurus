import io
import logging
import math
import os
import pprint
import time

import urwid
import yaml

from bzt import TaurusConfigError, ToolError
from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet, AggregatorListener
from bzt.modules.pbench import PBenchExecutor, Scheduler, TaurusPBenchTool
from bzt.six import parse, b
from bzt.utils import BetterDict, is_windows
from tests import BZTestCase, RESOURCES_DIR
from tests.mocks import EngineEmul

if not is_windows():
    class TestPBench(BZTestCase):
        def test_simple(self):
            obj = PBenchExecutor()
            obj.engine = EngineEmul()
            obj.engine.aggregator = ConsolidatingAggregator()
            obj.engine.aggregator.add_listener(DataPointLogger())
            obj.engine.config.merge({"provisioning": "test"})

            if os.path.exists("/home/undera/Sources/phantom"):  # FIXME: not good, get rid of it
                obj.settings.merge({
                    "path": "/home/undera/Sources/phantom/bin/phantom",
                    "modules-path": "/home/undera/Sources/phantom/lib/phantom"})
            else:
                obj.settings.merge({
                    "path": RESOURCES_DIR + "pbench/phantom.sh"})

            obj.execution.merge({
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
            obj.engine.aggregator.prepare()
            obj.prepare()

            obj.engine.aggregator.startup()
            obj.startup()

            while not obj.check():
                logging.debug("Running...")
                obj.engine.aggregator.check()
                time.sleep(1)

            obj.shutdown()
            obj.engine.aggregator.shutdown()

            obj.post_process()
            obj.engine.aggregator.post_process()

        def test_schedule_rps(self):
            executor = PBenchExecutor()
            executor.engine = EngineEmul()
            executor.engine.config.merge({"provisioning": "test"})
            rps = 9
            rampup = 12
            executor.execution.merge({"throughput": rps, "ramp-up": rampup, "steps": 3, "hold-for": 0})
            obj = Scheduler(executor.get_load(), io.BytesIO(b("4 test\ntest\n")), logging.getLogger(""))

            cnt = 0
            cur = 0
            currps = 0
            for item in obj.generate():
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
            executor = PBenchExecutor()
            executor.engine = EngineEmul()
            executor.execution.merge({"concurrency": 10, "ramp-up": None, "steps": 3, "hold-for": 10})
            # this line shouln't throw an exception
            obj = Scheduler(executor.get_load(), io.BytesIO(b("4 test\ntest\n")), logging.getLogger(""))

        def test_schedule_empty(self):
            executor = PBenchExecutor()
            executor.engine = EngineEmul()
            # concurrency: 1, iterations: 1
            obj = Scheduler(executor.get_load(), io.BytesIO(b("4 test\ntest\n")), logging.getLogger(""))
            items = list(obj.generate())
            for item in items:
                logging.debug("Item: %s", item)
            self.assertEqual(1, len(items))

        def test_schedule_concurrency(self):
            executor = PBenchExecutor()
            executor.engine = EngineEmul()
            executor.execution.merge({"concurrency": 5, "ramp-up": 10, "hold-for": 5})
            obj = Scheduler(executor.get_load(),
                            io.BytesIO(b("5 test1\ntest1\n5 test2\ntest2\n")), logging.getLogger(""))
            items = list(obj.generate())
            self.assertEqual(8, len(items))
            self.assertEqual(-1, items[5][0])  # instance became unlimited
            self.assertEqual(Scheduler.REC_TYPE_LOOP_START, items[6][5])  # looped payload

        def test_schedule_throughput_only(self):
            executor = PBenchExecutor()
            executor.engine = EngineEmul()
            executor.execution.merge({"throughput": 5})
            obj = Scheduler(executor.get_load(),
                            io.BytesIO(b("5 test1\ntest1\n5 test2\ntest2\n")), logging.getLogger(""))
            items = list(obj.generate())
            self.assertTrue(len(items) > 0)

        def test_schedule_concurrency_steps(self):
            executor = PBenchExecutor()
            executor.engine = EngineEmul()
            executor.execution.merge({"concurrency": 5, "ramp-up": 10, "steps": 3})
            obj = Scheduler(executor.get_load(),
                            io.BytesIO(b("5 test1\ntest1\n5 test2\ntest2\n")), logging.getLogger(""))
            items = list(obj.generate())
            self.assertEqual(8, len(items))
            self.assertEqual(-1, items[5][0])  # instance became unlimited
            self.assertEqual(Scheduler.REC_TYPE_LOOP_START, items[6][5])  # looped payload

        def test_widget(self):
            obj = PBenchExecutor()
            obj.engine = EngineEmul()
            obj.settings = BetterDict()
            obj.engine.config.merge({
                "provisioning": "test",
                ScenarioExecutor.EXEC: [
                    {
                        "throughput": 10,
                        "hold-for": 30,
                        "scenario": {
                            "default-address": "http://blazedemo.com/",
                            "requests": ["/"]}}]})
            obj.execution = obj.engine.config['execution'][0]
            obj.settings.merge({
                "path": RESOURCES_DIR + "pbench/phantom.sh",
            })
            obj.prepare()
            obj.startup()
            obj.get_widget()
            self.assertTrue(isinstance(obj.widget.progress, urwid.ProgressBar))
            self.assertEqual(obj.widget.duration, 30)
            self.assertEqual(obj.widget.widgets[0].text, "Pbench: http://blazedemo.com:80")
            obj.check()
            obj.shutdown()

        def test_improved_request_building(self):
            obj = PBenchExecutor()
            obj.engine = EngineEmul()
            obj.settings = BetterDict()
            obj.engine.config = BetterDict()
            obj.engine.config.merge(yaml.load(
                open(RESOURCES_DIR + "yaml/phantom_improved_request.yml").read()))
            obj.execution = obj.engine.config['execution'][0]
            obj.settings.merge({
                "path": RESOURCES_DIR + "pbench/phantom.sh",
            })
            obj.prepare()
            with open(obj.pbench.schedule_file) as fds:
                config = fds.readlines()

            get_requests = [req_str.split(" ")[1] for req_str in config if req_str.startswith("GET")]
            self.assertEqual(len(get_requests), 2)

            for get_req in get_requests:
                self.assertEqual(dict(parse.parse_qsl(parse.urlsplit(get_req).query)),
                                 {"get_param1": "value1", "get_param2": "value2"})

        def test_same_address_port(self):
            obj = PBenchExecutor()
            obj.engine = EngineEmul()
            obj.settings = BetterDict()
            obj.engine.config = BetterDict()
            obj.engine.config.merge(yaml.load(open(RESOURCES_DIR + "yaml/phantom_request_same_address.yml").read()))
            obj.execution = obj.engine.config['execution'][0]
            obj.settings.merge({
                "path": RESOURCES_DIR + "pbench/phantom.sh",
            })
            self.assertRaises(TaurusConfigError, obj.prepare)

        def test_install_pbench(self):
            obj = PBenchExecutor()
            obj.engine = EngineEmul()
            obj.settings = BetterDict()
            obj.engine.config = BetterDict()
            obj.settings.merge({"path": "/notexistent"})
            # obj.execution = obj.engine.config['execution'][0]
            try:
                obj.prepare()
                self.fail()
            except ToolError as exc:
                self.assertEquals("Please install PBench tool manually", str(exc))

        def test_pbench_file_lister(self):
            obj = PBenchExecutor()
            obj.engine = EngineEmul()
            obj.settings = BetterDict()
            obj.engine.config = BetterDict()
            obj.engine.config.merge(
                    {'execution': {"executor": "pbench", "scenario": {"script": "script.src"}}})
            obj.execution = obj.engine.config['execution']
            obj.settings.merge({
                "path": RESOURCES_DIR + "pbench/phantom.sh",
            })
            resource_files = obj.resource_files()
            self.assertEqual(1, len(resource_files))
            self.assertEqual(resource_files[0], 'script.src')

        def test_pbench_script(self):
            obj = PBenchExecutor()
            obj.engine = EngineEmul()
            obj.settings = BetterDict()
            obj.engine.config = BetterDict()
            obj.engine.config.merge({
                ScenarioExecutor.EXEC: {
                    "executor": "pbench",
                    "scenario": {"script": RESOURCES_DIR + "pbench/pbench.src"}
                },
                "provisioning": "test"
            })
            obj.execution = obj.engine.config['execution']
            obj.settings.merge({
                "path": RESOURCES_DIR + "pbench/phantom.sh"
            })
            obj.prepare()

        def test_pbench_payload_relpath(self):
            "Verify that enhanced pbench preserves relative script path"
            script_path = "tests/resources/pbench/pbench.src"

            obj = PBenchExecutor()
            obj.engine = EngineEmul()
            obj.settings = BetterDict()
            obj.engine.config = BetterDict()
            obj.engine.config.merge({
                ScenarioExecutor.EXEC: {
                    "executor": "pbench",
                    "scenario": {"script": "tests/resources/pbench/pbench.src"}
                },
                "provisioning": "test",
            })
            obj.execution = obj.engine.config['execution']
            obj.settings.merge({
                "path": RESOURCES_DIR + "pbench/phantom.sh",
                "enhanced": True,
            })
            obj.prepare()

            pbench_conf = os.path.join(obj.engine.artifacts_dir, "pbench.conf")
            with open(pbench_conf) as conf_fds:
                config = conf_fds.read()
                self.assertIn(script_path, config)

        def test_pbench_payload_py3_crash(self):
            obj = PBenchExecutor()
            obj.engine = EngineEmul()
            obj.settings = BetterDict()
            obj.engine.config = BetterDict()
            obj.engine.config.merge({
                ScenarioExecutor.EXEC: {
                    "executor": "pbench",
                    "scenario": {"requests": ["test%d" % i for i in range(20)]}
                },
                "provisioning": "test",
            })
            obj.execution = obj.engine.config['execution']
            obj.settings.merge({
                "path": RESOURCES_DIR + "pbench/phantom.sh",
            })
            obj.prepare()

        def test_diagnostics(self):
            obj = PBenchExecutor()
            obj.engine = EngineEmul()
            obj.settings = BetterDict()
            obj.engine.config.merge({
                "provisioning": "test",
                ScenarioExecutor.EXEC: [
                    {
                        "throughput": 10,
                        "hold-for": 30,
                        "scenario": {
                            "default-address": "http://blazedemo.com/",
                            "requests": ["/"]}}]})
            obj.execution = obj.engine.config['execution'][0]
            obj.settings.merge({
                "path": RESOURCES_DIR + "pbench/phantom.sh",
            })
            obj.prepare()
            obj.startup()
            for _ in range(3):
                obj.check()
            obj.shutdown()
            obj.post_process()
            self.assertIsNotNone(obj.get_error_diagnostics())

    class TestScheduler(BZTestCase):
        def _get_pbench(self):
            obj = PBenchExecutor()
            obj.engine = EngineEmul()
            obj.settings = BetterDict()
            obj.engine.config = BetterDict()
            return obj

        def check_schedule_size_estimate(self, obj, execution):
            obj.engine.config = BetterDict()
            obj.engine.config.merge({
                ScenarioExecutor.EXEC: execution,
                "provisioning": "local",
            })
            obj.execution = obj.engine.config['execution']
            load = obj.get_load()
            obj.pbench = TaurusPBenchTool(obj, logging.getLogger(''))
            obj.pbench.generate_payload(obj.get_scenario())
            payload_count = len(obj.get_scenario().get('requests', []))
            sch = Scheduler(load, open(obj.pbench.payload_file, 'rb'), logging.getLogger(''))
            estimated_schedule_size = obj.pbench._estimate_schedule_size(load, payload_count)
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
            execution = {
                "concurrency": 10,
                "scenario": {
                    "requests": ["test1", "test2", "test3"]
                }
            }
            obj = self._get_pbench()
            self.check_schedule_size_estimate(obj, execution)

        def test_est_conc_iterations(self):
            execution = {
                "concurrency": 10,
                "iterations": 3,
                "scenario": {
                    "requests": ["test1", "test2", "test3"]
                }
            }
            obj = self._get_pbench()
            self.check_schedule_size_estimate(obj, execution)

        def test_est_conc_holdfor(self):
            execution = {
                "concurrency": 10,
                "hold-for": "10s",
                "scenario": {
                    "requests": ["test1", "test2", "test3"]
                }
            }
            obj = self._get_pbench()
            self.check_schedule_size_estimate(obj, execution)

        def test_est_conc_rampup(self):
            execution = {
                "concurrency": 5,
                "ramp-up": "20s",
                "scenario": {
                    "requests": ["test1", "test2", "test3"]
                }
            }
            obj = self._get_pbench()
            self.check_schedule_size_estimate(obj, execution)

        def test_est_tput_requests(self):
            execution = {
                "throughput": 100,
                "scenario": {
                    "requests": ["test1", "test2", "test3"]
                }
            }
            obj = self._get_pbench()
            self.check_schedule_size_estimate(obj, execution)

        def test_est_tput_iterations(self):
            execution = {
                "throughput": 100,
                "iterations": 10,
                "scenario": {
                    "requests": ["test1", "test2", "test3"]
                }
            }
            obj = self._get_pbench()
            self.check_schedule_size_estimate(obj, execution)

        def test_est_tput_hold(self):
            execution = {
                "throughput": 100,
                "hold-for": "20s",
                "scenario": {
                    "requests": ["test1", "test2"]
                }
            }
            obj = self._get_pbench()
            self.check_schedule_size_estimate(obj, execution)

        def test_est_tput_hold_iterations(self):
            execution = {
                "throughput": 100,
                "iterations": 10,
                "hold-for": "10s",
                "scenario": {
                    "requests": ["test1", "test2", "test3"]
                }
            }
            obj = self._get_pbench()
            self.check_schedule_size_estimate(obj, execution)

        def test_est_tput_rampup(self):
            execution = {
                "throughput": 100,
                "ramp-up": "10s",
                "scenario": {
                    "requests": ["test1", "test2"]
                }
            }
            obj = self._get_pbench()
            self.check_schedule_size_estimate(obj, execution)

        def test_est_tput_rampup_iterations(self):
            execution = {
                "throughput": 100,
                "ramp-up": "10s",
                "iterations": 20,
                "scenario": {
                    "requests": ["test1", "test2", "test3"]
                }
            }
            obj = self._get_pbench()
            self.check_schedule_size_estimate(obj, execution)


class DataPointLogger(AggregatorListener):
    def aggregated_second(self, data):
        current = data[DataPoint.CURRENT]['']
        logging.info("DataPoint %s: VU:%s RPS:%s/%s RT:%s", data[DataPoint.TIMESTAMP],
                     current[KPISet.CONCURRENCY], current[KPISet.SAMPLE_COUNT], current[KPISet.FAILURES],
                     current[KPISet.AVG_RESP_TIME])
