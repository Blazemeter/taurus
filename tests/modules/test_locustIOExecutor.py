import logging
import time
import sys

from bzt import six
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.locustio import LocustIOExecutor, SlavesReader
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestLocustIOExecutor(BZTestCase):
    def setUp(self):
        sys.path.append(__dir__() + "/../locust/")

    def test_simple(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        obj = LocustIOExecutor()
        obj.engine = EngineEmul()
        obj.engine.config['provisioning'] = 'local'
        obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": __dir__() + "/../locust/simple.py"
            }
        })

        obj.prepare()
        obj.startup()
        try:
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        except RuntimeError:  # FIXME: not good, but what to do?
            pass
        obj.shutdown()
        self.assertRaises(RuntimeWarning, obj.post_process)

    def test_locust_widget(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        obj = LocustIOExecutor()
        obj.engine = EngineEmul()
        obj.engine.config['provisioning'] = 'local'
        obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": __dir__() + "/../locust/simple.py"
            }
        })

        obj.prepare()
        obj.startup()
        obj.get_widget()
        obj.check()
        self.assertEqual(obj.widget.duration, 30)
        self.assertTrue(obj.widget.widgets[0].text.endswith("simple.py"))
        obj.shutdown()

    def test_locust_master(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        obj = LocustIOExecutor()
        obj.engine = EngineEmul()
        obj.engine.config['provisioning'] = 'local'
        obj.execution.merge({
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

        obj.prepare()
        obj.startup()
        obj.get_widget()
        try:
            obj.check()
            time.sleep(2)
            obj.check()
        except RuntimeError:
            logging.warning("Do you use patched locust for non-GUI master?")
        obj.shutdown()
        self.assertRaises(RuntimeWarning, obj.post_process)

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

        obj = LocustIOExecutor()
        obj.engine = EngineEmul()
        obj.engine.config['provisioning'] = 'local'
        obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": __dir__() + "/../locust/simple.py"
            }
        })
        resource_files = obj.resource_files()
        self.assertEqual(1, len(resource_files))

    def test_fail_on_zero_results(self):
        if six.PY3:
            logging.warning("No locust available for python 3")

        obj = LocustIOExecutor()
        obj.engine = EngineEmul()
        obj.engine.config['provisioning'] = 'local'
        obj.execution.merge({
            "concurrency": 1,
            "iterations": 10,
            "hold-for": 30,
            "scenario": {
                "default-address": "http://blazedemo.com",
                "script": __dir__() + "/../locust/simple.py"
            }
        })
        obj.prepare()
        self.assertRaises(RuntimeWarning, obj.post_process)