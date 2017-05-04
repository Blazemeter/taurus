import sys
import time

import os
from tests import BZTestCase, r, rc, __dir__

from bzt.engine import Provisioning, ScenarioExecutor
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.console import ConsoleStatusReporter
from bzt.modules.jmeter import JMeterExecutor
from bzt.modules.provisioning import Local
from bzt.utils import is_windows, EXE_SUFFIX
from tests.mocks import EngineEmul


class TestConsoleStatusReporter(BZTestCase):
    def __get_datapoint(self, n):
        point = DataPoint(n)
        overall = point[DataPoint.CURRENT].get('', KPISet())
        overall[KPISet.CONCURRENCY] = r(100)
        overall[KPISet.SAMPLE_COUNT] = int(100 * r(1000))
        overall[KPISet.FAILURES] = overall[KPISet.SAMPLE_COUNT] / 2.0
        overall[KPISet.AVG_RESP_TIME] = r(100)
        overall[KPISet.AVG_CONN_TIME] = overall[KPISet.AVG_RESP_TIME] / 3.0
        overall[KPISet.AVG_LATENCY] = 2.0 * overall[
            KPISet.AVG_RESP_TIME] / 3.0
        overall[KPISet.PERCENTILES]['25'] = r(10)
        overall[KPISet.PERCENTILES]['50'] = r(20)
        overall[KPISet.PERCENTILES]['75'] = r(30)
        overall[KPISet.PERCENTILES]['90'] = r(40)
        overall[KPISet.PERCENTILES]['99'] = r(50)
        overall[KPISet.PERCENTILES]['100'] = r(100)
        overall[KPISet.RESP_CODES][rc()] = 1
        point[DataPoint.CUMULATIVE][''] = overall
        return point

    def get_jmeter(self):
        dir_name = os.path.dirname(__file__)
        path = dir_name + "/../resources/jmeter/jmeter-loader" + EXE_SUFFIX
        obj = JMeterExecutor()
        obj.settings.merge({'path': path})
        obj.execution.merge({"scenario": {
            "script": __dir__() + "/../resources/jmeter/jmx/dummy.jmx"}})
        return obj

    def test_1(self):
        obj = ConsoleStatusReporter()
        self.sniff_log(obj.log)
        obj.engine = EngineEmul()
        obj.engine.provisioning = Local()
        obj.engine.provisioning.start_time = time.time()
        obj.engine.config[Provisioning.PROV] = ''
        jmeter = self.get_jmeter()
        jmeter.engine = obj.engine
        jmeter.execution[ScenarioExecutor.HOLD_FOR] = 10
        jmeter.execution.merge({'hold-for': 0, 'ramp-up': 0})
        jmeter.delay = 10
        jmeter.prepare()
        widget = jmeter.get_widget()
        widget.update()
        jmeter.startup()
        widget.update()
        obj.engine.provisioning.executors = [jmeter]
        obj.settings["disable"] = False
        obj.settings['dummy_cols'] = 160
        obj.settings['dummy_rows'] = 40
        obj.settings['disable'] = False
        obj.prepare()
        obj.startup()

        obj.check()
        obj.temp_stream.write("test1\n")
        obj.temp_stream.flush()
        obj.temp_stream.write("test1\n")
        obj.temp_stream.flush()
        obj.check()

        for n in range(0, 10):
            point = self.__get_datapoint(n)
            obj.aggregated_second(point)
            obj.temp_stream.write("test %s\n" % n)
            obj.temp_stream.flush()
            obj.check()
            self.assertTrue(obj.screen.started)

        point = self.__get_datapoint(11)
        point[DataPoint.CURRENT][''][KPISet.RESP_CODES][''] = 1
        obj.aggregated_second(point)

        obj.check()
        obj.shutdown()
        obj.post_process()
        self.assertNotIn('Failed', self.handler.warn_buff.getvalue())

    def test_2(self):
        obj = ConsoleStatusReporter()
        obj.engine = EngineEmul()
        obj.engine.provisioning = Local()
        obj.engine.provisioning.start_time = time.time()
        obj.engine.config[Provisioning.PROV] = ''
        jmeter = self.get_jmeter()
        jmeter.engine = obj.engine
        jmeter.execution[ScenarioExecutor.HOLD_FOR] = 10
        jmeter.delay = 10
        jmeter.prepare()
        widget = jmeter.get_widget()
        widget.update()
        jmeter.startup()
        widget.update()
        obj.engine.provisioning.executors = [jmeter]
        obj.settings["disable"] = False
        obj.settings['dummy_cols'] = 160
        obj.settings['dummy_rows'] = 401
        obj.settings['disable'] = False
        obj.prepare()
        obj.startup()

        for n in range(0, 10):
            point = self.__get_datapoint(0)
            obj.aggregated_second(point)
            obj.check()
            self.assertTrue(obj.screen.started)

        obj.check()
        obj.shutdown()
        obj.post_process()

    def test_disabled(self):
        obj = ConsoleStatusReporter()
        obj.engine = EngineEmul()
        obj.engine.provisioning = Local()
        obj.engine.config[Provisioning.PROV] = ''
        jmeter = self.get_jmeter()
        jmeter.engine = obj.engine
        jmeter.start_time = time.time()
        jmeter.execution[ScenarioExecutor.HOLD_FOR] = 10
        obj.engine.provisioning.executors = [jmeter]
        obj.settings["disable"] = True
        obj.settings['dummy_cols'] = 160
        obj.settings['dummy_rows'] = 40
        obj.prepare()
        obj.startup()

        for n in range(0, 10):
            point = self.__get_datapoint(0)
            obj.aggregated_second(point)
            obj.check()
            self.assertFalse(obj.screen.started)

        obj.check()
        obj.shutdown()
        obj.post_process()

    def test_disabled_1(self):
        obj = ConsoleStatusReporter()
        obj.engine = EngineEmul()
        obj.engine.provisioning = Local()
        obj.engine.config[Provisioning.PROV] = ''
        obj.settings["disable"] = 1
        obj.prepare()
        self.assertEquals(True, obj.disabled)

    def test_disabled_0(self):
        obj = ConsoleStatusReporter()
        obj.engine = EngineEmul()
        obj.engine.provisioning = Local()
        obj.engine.config[Provisioning.PROV] = ''
        obj.settings["disable"] = 0
        obj.prepare()
        self.assertEquals(False, obj.disabled)

    def test_screen(self):
        obj = ConsoleStatusReporter()
        obj.settings["screen"] = "console"
        if not sys.stdout.isatty():
            self.assertEqual(obj._get_screen_type(), "dummy")
        elif is_windows():
            self.assertEqual(obj._get_screen(), "gui")
        else:
            self.assertEqual(obj._get_screen_type(), "console")

    def test_screen_invalid(self):
        obj = ConsoleStatusReporter()
        obj.settings["screen"] = "invalid"
        if not sys.stdout.isatty():
            self.assertEqual(obj._get_screen_type(), "dummy")
        elif is_windows():
            self.assertEqual(obj._get_screen(), "gui")
        else:
            self.assertEqual(obj._get_screen_type(), "console")
