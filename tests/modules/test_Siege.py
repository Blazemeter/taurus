import logging
import time
from os import path

from bzt import ToolError, TaurusConfigError
from bzt.modules.siege import SiegeExecutor, DataLogReader
from tests import BZTestCase
from tests.mocks import EngineEmul
from bzt.utils import EXE_SUFFIX


TOOL_NAME = 'siege' + EXE_SUFFIX


def get_res_path(resource):
    return path.join(path.dirname(__file__), '..', 'resources', 'siege', resource)


class TestSiegeExecutor(BZTestCase):
    def test_iter(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "concurrency": 2,
            "iterations": 3,
            "scenario": {
                "think-time": "1s",
                "requests": ["http://blazedemo.com",
                             "http://ya.ru"]}
        })
        obj.prepare()
        obj.get_widget()
        obj.startup()

    def test_hold(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "concurrency": 2,
            "hold-for": '2s',
            "scenario": {
                "headers": {
                    'h1': 'value1',
                    'h2': 'value2'},
                "variables": {
                    'v1': 1,
                    'v2': 'TWO'},
                "script": get_res_path('url-file')}})
        obj.prepare()
        obj.prepare()
        self.assertNotEqual(len(obj.resource_files()), 0)
        obj.get_widget()
        obj.startup()

    def test_url_exceptions(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "concurrency": 2,
            "hold-for": '2s',
            "scenario": {}})
        self.assertRaises(TaurusConfigError, obj.prepare)

    def test_check_install_exceptions(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": '*',})
        obj.execution.merge({
            "concurrency": 2,
            "hold-for": '2s',
            "scenario": {}})
        self.assertRaises(ToolError, obj.prepare)

    def test_repetition_exceptions(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "concurrency": 2,
            "scenario": {
                "requests": ["http://blazedemo.com",
                             "http://ya.ru"]}})
        obj.prepare()
        self.assertEqual(len(obj.resource_files()), 0)
        self.assertRaises(TaurusConfigError, obj.startup)

    def test_full_execution(self):
        obj = SiegeExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "concurrency": 2,
            "iterations": 3,
            "scenario": {
                "requests": ["http://blazedemo.com",
                             "http://ya.ru"]}
        })
        obj.prepare()
        try:
            obj.startup()
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
        list_of_values = list(obj.datapoints(True))

        self.assertEqual(len(list_of_values), 8)

        for values in list_of_values:
            self.assertTrue(1400000000 < values['ts'] < 1500000000)
            self.assertEqual(len(values), 5)
