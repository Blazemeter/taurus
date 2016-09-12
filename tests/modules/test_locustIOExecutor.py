import logging
import time
import sys

from bzt import six
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.locustio import LocustIOExecutor, SlavesReader, LocustIOScriptBuilder
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestLocustIOExecutor(BZTestCase):
    def setUp(self):
        sys.path.append(__dir__() + "/../locust/")
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
                "script": __dir__() + "/../locust/simple.py"
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
        self.assertRaises(RuntimeWarning, self.obj.post_process)

    def test_locust_widget(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": __dir__() + "/../locust/simple.py"
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
                "script": __dir__() + "/../locust/simple.py"
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
        self.assertRaises(RuntimeWarning, self.obj.post_process)

    def test_locust_slave_results(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        obj = SlavesReader(__dir__() + "/../locust/locust-slaves.ldjson", 2, logging.getLogger(""))
        points = [x for x in obj.datapoints(True)]
        self.assertEquals(107, len(points))
        for point in points:
            self.assertGreater(point[DataPoint.CURRENT][''][KPISet.AVG_RESP_TIME], 0)

    def test_locust_resource_files(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": __dir__() + "/../locust/simple.py"
            }
        })
        resource_files = self.obj.resource_files()
        self.assertEqual(1, len(resource_files))

    def test_fail_on_zero_results(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        self.obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": __dir__() + "/../locust/simple.py"
            }
        })
        self.obj.prepare()
        self.assertRaises(RuntimeWarning, self.obj.post_process)

    def test_build_script(self):
        self.obj.engine.config.merge({
            "execution": [{
                "executor": "locust",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/"}]}}})
        self.obj.execution = self.obj.engine.config.get('execution')[0]
        self.obj.prepare()

        with open(self.obj.script) as generated:
            gen_contents = generated.readlines()
        with open(__dir__() + "/../locust/generated_from_requests.py") as sample:
            sample_contents = sample.readlines()

        # strip line terminators
        gen_contents = [line.rstrip() for line in gen_contents]
        sample_contents = [line.rstrip() for line in sample_contents]

        self.assertEqual(gen_contents, sample_contents)