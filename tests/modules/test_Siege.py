import logging
import time
from os import path

from bzt.modules.siege import SiegeExecutor, DataLogReader
from tests import BZTestCase
from tests.mocks import EngineEmul


def get_res_path(resource):
    return path.join(path.dirname(__file__), '..', 'siege', resource)


class TestSiegeExecutor(BZTestCase):
    def test_iter_(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "executor-path": get_res_path('siege.sh'),
            "concurrency": 2,
            "iterations": 3,
            "scenario": {
                "think-time": "1s",
                "requests": ["http://blazedemo.com",
                             "http://ya.ru"]}
        })
        obj.prepare()
        obj.startup()

    def test_hold_(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "executor-path": get_res_path('siege.sh'),
            "concurrency": 2,
            "hold-for": '2s',
            "scenario": {
                "requests": ["http://blazedemo.com",
                             "http://ya.ru"]}})
        obj.prepare()
        obj.startup()

    def test_url_exceptions(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "executor-path": get_res_path('siege.sh'),
            "concurrency": 2,
            "hold-for": '2s',
            "scenario": {}})
        try:
            obj.prepare()
        except ValueError:
            return
        self.fail()

    def test_check_install_exceptions(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "executor-path": '*',
            "concurrency": 2,
            "hold-for": '2s',
            "scenario": {}})
        try:
            obj.prepare()
        except RuntimeError:
            return
        self.fail()

    def test_repetition_exceptions(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "executor-path": get_res_path('siege.sh'),
            "concurrency": 2,
            "scenario": {
                "requests": ["http://blazedemo.com",
                             "http://ya.ru"]}})
        obj.prepare()
        try:
            obj.startup()
        except ValueError:
            return
        self.fail

        try:
            obj.prepare()
        except ValueError:
            return
        self.fail()

    def test_full_execution(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "executor-path": get_res_path('siege.sh'),
            "concurrency": 2,
            "iterations": 3,
            "scenario": {
                "requests": ["http://blazedemo.com",
                             "http://ya.ru"]}
        })
        obj.prepare()
        obj.startup()
        try:
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()

        obj.post_process()
        self.assertNotEquals(obj.process, None)


class TestDataLogReader(BZTestCase):
    def test_read(self):
        log_path = path.join(get_res_path('siege.out'))
        obj = DataLogReader(log_path, logging.getLogger(''))
        list_of_values = list(obj._read())

        self.assertEqual(len(list_of_values), 10)

        for values in list_of_values:
            self.assertEqual(len(values), 9)
