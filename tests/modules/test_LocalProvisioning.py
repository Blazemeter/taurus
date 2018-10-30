import time

import datetime

from bzt import ToolError
from bzt.engine import ScenarioExecutor
from bzt.modules.provisioning import Local
from tests import BZTestCase
from tests.mocks import EngineEmul


class ScenarioExecutorEmul(object):
    def __init__(self):
        pass

    def startup(self):
        pass


class LocalProvisioningEmul(Local):
    def __init__(self):
        super(LocalProvisioningEmul, self).__init__()
        self.engine = EngineEmul()


class LocalProvisioningTest(BZTestCase):
    def check_started_list(self, start_time, delay, prepared, started):
        executor = ScenarioExecutorEmul()
        executor.delay = delay
        prov = LocalProvisioningEmul()
        prov.executors = [executor]
        prov.start_time = start_time

        if prepared:
            prov.engine.prepared = [executor]
        else:
            prov.engine.prepared = []

        if started:
            prov.engine.started = [executor]
        else:
            prov.engine.started = []

        prov._start_modules()

        return executor in prov.engine.started

    def test_delay_cycle(self):
        cur_time = time.time()

        self.assertTrue(self.check_started_list(cur_time, 0, True, False))  # all ok
        self.assertFalse(self.check_started_list(cur_time, 0, False, False))  # not prepared
        self.assertTrue(self.check_started_list(cur_time - 10, 5, True, False))  # time to run
        self.assertFalse(self.check_started_list(cur_time - 10, 20, True, False))  # too early
        self.assertFalse(self.check_started_list(cur_time - 10, 5, False, False))  # time to run but not prepared

    def test_start_shift(self):
        local = Local()

        _today = datetime.date.today()
        _yesterday = _today - datetime.timedelta(days=1)
        _tomorrow = _today + datetime.timedelta(days=1)
        _start_time = datetime.time(12, 30, 5)
        _scheduled_time = datetime.time(13, 31, 7)

        local.start_time = time.mktime(datetime.datetime.combine(_today, _start_time).timetuple())

        date = datetime.datetime.combine(_tomorrow, _scheduled_time).strftime('%Y-%m-%d %H:%M:%S')
        shift = local._get_start_shift(date)
        self.assertEqual(shift, 90062.0)

        date = datetime.datetime.combine(_yesterday, _scheduled_time).strftime('%Y-%m-%d %H:%M')
        shift = local._get_start_shift(date)
        self.assertEqual(shift, 3655.0)

        date = datetime.datetime.combine(_today, _scheduled_time).strftime('%H:%M:%S')
        shift = local._get_start_shift(date)
        self.assertEqual(shift, 3662.0)

        date = datetime.datetime.combine(_today, _scheduled_time).strftime('%H:%M')
        shift = local._get_start_shift(date)
        self.assertEqual(shift, 3655.0)

        shift = local._get_start_shift('')
        self.assertEqual(shift, 0)

        shift = local._get_start_shift('lorem ipsum')
        self.assertEqual(shift, 0)

    def test_start_sequential_global(self):
        local = Local()
        local.settings["sequential"] = True
        local.engine = EngineEmul()
        local.engine.config.merge({ScenarioExecutor.EXEC: [{}, {}]})
        local.engine.config.get("settings")["default-executor"] = "mock"
        local.engine.unify_config()
        local.prepare()
        local.startup()

        cnt = 0
        while not local.check():
            cnt += 1

        self.assertEqual(3, cnt)

        local.shutdown()

        for executor in local.executors:
            executor.is_has_results = True

        local.post_process()

    def test_exception(self):
        local = Local()
        local.engine = EngineEmul()
        local.engine.config.merge({ScenarioExecutor.EXEC: [{}]})
        local.engine.config.get("settings")["default-executor"] = "mock"
        local.engine.unify_config()
        local.prepare()
        local.startup()

        local.check()

        local.shutdown()
        try:
            local.post_process()
        except ToolError as exc:
            self.assertNotIn('DIAGNOSTICS', str(exc))
            self.assertIsNotNone(exc.diagnostics)
            self.assertEqual(exc.diagnostics, ['DIAGNOSTICS'])
        except BaseException as exc:
            self.fail("Was supposed to fail with ToolError, but crashed with %s" % exc)
