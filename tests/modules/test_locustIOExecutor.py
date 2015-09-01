import logging
import time

from bzt import six
from bzt.modules.locustio import LocustIOExecutor
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class TestLocustIOExecutor(BZTestCase):
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
        obj.post_process()

    def test_locust_widget(self):
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
        self.assertTrue(obj.widget.script_name_widget.text.endswith("simple.py"))
        obj.shutdown()
