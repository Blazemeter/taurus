import sys
import time
import unittest
from os.path import join

from bzt import ToolError
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.molotov import MolotovExecutor, MolotovReportReader
from bzt.utils import EXE_SUFFIX, is_windows
from tests import BZTestCase, ExecutorTestCase, RESOURCES_DIR, close_reader_file, ROOT_LOGGER

TOOL_NAME = 'molotov-mock' + EXE_SUFFIX
TOOL_PATH = join(RESOURCES_DIR, "molotov", TOOL_NAME)
LOADTEST_PY = join(RESOURCES_DIR, "molotov", "loadtest.py")


class TestMolotov(ExecutorTestCase):
    EXECUTOR = MolotovExecutor

    def tearDown(self):
        if self.obj.reader:
            close_reader_file(self.obj.reader.ldjson_reader)
        super(TestMolotov, self).tearDown()

    def test_mocked(self):
        self.obj.settings.merge({
            "path": TOOL_PATH})
        self.obj.execution.merge({
            "ramp-up": "10s",
            "hold-for": "20s",
            "scenario": {
                "script": LOADTEST_PY}})
        self.obj.prepare()
        self.obj.get_widget()
        try:
            self.obj.startup()
            while not self.obj.check():
                time.sleep(self.obj.engine.check_interval)
        finally:
            self.obj.shutdown()
        self.obj.post_process()
        self.assertNotEquals(self.obj.process, None)

    def test_no_tool(self):
        self.obj.settings.merge({
            "path": '*'})
        self.obj.execution.merge({
            "scenario": {
                "script": LOADTEST_PY}})
        self.assertRaises(ToolError, self.obj.prepare)

    def test_diagnostics(self):
        self.obj.settings.merge({
            "path": TOOL_PATH})
        self.obj.execution.merge({
            "iterations": 1,
            "scenario": {
                "script": LOADTEST_PY}})
        self.obj.prepare()
        self.obj.engine.start_subprocess = lambda **kwargs: None
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()
        self.assertIsNotNone(self.obj.get_error_diagnostics())

    def test_resource_files(self):
        self.obj.execution.merge({
            "scenario": {
                "script": LOADTEST_PY}})
        resources = self.obj.get_resource_files()
        self.assertEqual(resources, [LOADTEST_PY])

    @unittest.skipIf(is_windows(), "disabled on windows")
    def test_full(self):
        self.configure({"execution": {
            "concurrency": 3,
            "processes": 2,
            "hold-for": "5s",
            "iterations": 10,
            "scenario": {
                "script": LOADTEST_PY}}})
        self.obj.prepare()
        self.obj.engine.start_subprocess = lambda **kwargs: None
        self.obj.get_widget()
        self.obj.startup()
        self.obj.shutdown()
        self.obj.post_process()

    def test_think_time(self):
        self.CMD_LINE = []

        def start_subprocess(args, **kwargs):
            self.CMD_LINE = ' '.join(args)

        self.obj.settings.merge({
            "path": TOOL_PATH})
        self.configure({
            "execution": {
                "scenario": "simple"},
            "scenarios": {
                "simple": {
                    "think-time": 5,
                    "script": LOADTEST_PY
                }
            }})
        self.obj.prepare()
        self.obj.engine.start_subprocess = start_subprocess
        self.obj.startup()
        self.obj.post_process()
        self.assertTrue('--delay 5.0' in self.CMD_LINE)


class TestReportReader(BZTestCase):
    def test_read(self):
        log_path = join(RESOURCES_DIR, "molotov", "molotov-report.csv")
        obj = MolotovReportReader(log_path, ROOT_LOGGER)
        points = list(obj.datapoints(True))

        self.assertEqual(len(points), 3)

        for datapoint in points:
            self.assertTrue(datapoint['ts'] > 1500000000)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE][''][KPISet.SUCCESSES], 10)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE][''][KPISet.FAILURES], 2)
