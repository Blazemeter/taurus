import sys
import time

from bzt.engine import Provisioning, ScenarioExecutor
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.console import ConsoleStatusReporter, ConsoleScreen, GUIScreen
from bzt.modules.jmeter import JMeterExecutor
from bzt.modules.provisioning import Local
from bzt.utils import is_windows
from tests import BZTestCase, r, rc
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

    def test_1(self):
        obj = ConsoleStatusReporter()
        obj.engine = EngineEmul()
        obj.engine.provisioning = Local()
        obj.engine.config[Provisioning.PROV] = ''
        jmeter = JMeterExecutor()
        jmeter.engine = obj.engine
        jmeter.start_time = time.time()
        jmeter.execution[ScenarioExecutor.HOLD_FOR] = 10
        obj.engine.provisioning.executors = [jmeter]
        obj.settings["disable"] = False
        obj.settings['dummy_cols'] = 160
        obj.settings['dummy_rows'] = 40
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

        obj.check()
        obj.shutdown()
        obj.post_process()

    def test_2(self):
        obj = ConsoleStatusReporter()
        obj.engine = EngineEmul()
        obj.engine.provisioning = Local()
        obj.engine.config[Provisioning.PROV] = ''
        jmeter = JMeterExecutor()
        jmeter.engine = obj.engine
        jmeter.start_time = time.time()
        jmeter.execution[ScenarioExecutor.HOLD_FOR] = 10
        obj.engine.provisioning.executors = [jmeter]
        obj.settings["disable"] = False
        obj.settings['dummy_cols'] = 160
        obj.settings['dummy_rows'] = 40
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
        jmeter = JMeterExecutor()
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

    def test_screen(self):
        isatty = sys.stdout.isatty
        sys.stdout.isatty = lambda: True
        obj = ConsoleStatusReporter()
        obj.settings["screen"] = "console"
        if not is_windows():
            self.assertIsInstance(obj._get_screen(), ConsoleScreen)
        else:
            self.assertIsInstance(obj._get_screen(), GUIScreen)
        sys.stdout.isatty = isatty

    def test_screen_invalid(self):
        isatty = sys.stdout.isatty
        sys.stdout.isatty = lambda: True
        obj = ConsoleStatusReporter()
        obj.settings["screen"] = "invalid"
        if not is_windows():
            self.assertIsInstance(obj._get_screen(), ConsoleScreen)
        else:
            self.assertIsInstance(obj._get_screen(), GUIScreen)
        sys.stdout.isatty = isatty
