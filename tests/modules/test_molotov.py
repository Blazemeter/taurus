import logging
import sys
import time
import unittest
from os import path

from bzt import ToolError
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.molotov import MolotovExecutor, MolotovReportReader
from bzt.utils import EXE_SUFFIX
from tests import BZTestCase
from tests.mocks import EngineEmul

TOOL_NAME = 'molotov-mock' + EXE_SUFFIX


def get_res_path(resource):
    return path.join(path.dirname(__file__), '..', 'resources', 'molotov', resource)


class TestMolotov(BZTestCase):
    def test_mocked(self):
        obj = MolotovExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "ramp-up": "10s",
            "hold-for": "20s",
            "scenario": {
                "script": get_res_path("loadtest.py")
            }
        })
        obj.prepare()
        obj.get_widget()
        try:
            obj.startup()
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()
        obj.post_process()
        self.assertNotEquals(obj.process, None)

    def test_no_tool(self):
        obj = MolotovExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": '*',})
        obj.execution.merge({
            "scenario": {
                "script": get_res_path("loadtest.py"),
            }})
        self.assertRaises(ToolError, obj.prepare)

    def test_diagnostics(self):
        obj = MolotovExecutor()
        obj.engine = EngineEmul()
        obj.settings.merge({
            "path": get_res_path(TOOL_NAME),})
        obj.execution.merge({
            "iterations": 1,
            "scenario": {
                "script": get_res_path("loadtest.py")
            }
        })
        obj.prepare()
        obj.startup()
        while not obj.check():
            time.sleep(obj.engine.check_interval)
        obj.shutdown()
        obj.post_process()
        self.assertIsNotNone(obj.get_error_diagnostics())

    def test_resource_files(self):
        obj = MolotovExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "scenario": {
                "script": get_res_path("loadtest.py")
            }
        })
        resources = obj.get_resource_files()
        self.assertEqual(resources, [get_res_path("loadtest.py")])

    @unittest.skipUnless(sys.version_info >= (3, 5), "enabled only on 3.5+")
    def test_full(self):
        obj = MolotovExecutor()
        obj.engine = EngineEmul()
        obj.execution.merge({
            "hold-for": "5s",
            "scenario": {
                "script": get_res_path("loadtest.py")
            }
        })
        obj.prepare()
        obj.get_widget()
        try:
            obj.startup()
            while not obj.check():
                time.sleep(obj.engine.check_interval)
        finally:
            obj.shutdown()
        obj.post_process()
        self.assertNotEquals(obj.process, None)
        self.assertTrue(path.exists(obj.report_file_name))


class TestReportReader(BZTestCase):
    def test_read(self):
        log_path = path.join(get_res_path('molotov-report.csv'))
        obj = MolotovReportReader(log_path, logging.getLogger(''))
        points = list(obj.datapoints(True))

        self.assertEqual(len(points), 3)

        for datapoint in points:
            self.assertTrue(datapoint['ts'] > 1500000000)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE][''][KPISet.SUCCESSES], 10)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE][''][KPISet.FAILURES], 2)
