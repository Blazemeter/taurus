import os
import shutil
import io
import unittest
import bzt

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.vegeta import VegetaExecutor, VegetaLogReader
from bzt.utils import EXE_SUFFIX, is_windows, ToolError
from tests.unit import BZTestCase, ExecutorTestCase, RESOURCES_DIR, ROOT_LOGGER, BUILD_DIR

TOOL_NAME = os.path.join(RESOURCES_DIR, 'vegeta', 'vegeta_mock' + EXE_SUFFIX)
VEGETA_SCRIPT = os.path.join(RESOURCES_DIR, 'vegeta', 'vegeta.in')


class TestVegetaExecutor(ExecutorTestCase):
    EXECUTOR = VegetaExecutor
    CMD_LINE = []

    def tearDown(self):
        self.CMD_LINE = []

    def assertCmd(self, option, value):
        self.assertIn(f'{option} {value}', ' '.join(self.CMD_LINE))

    def start_subprocess(self, args, **kwargs):
        self.CMD_LINE.extend(args)
        setattr(self.obj,
                'process',
                type('', (object,), {'stdout': io.StringIO('foo'),
                                     'poll': lambda: True}))
        return self.obj.process

    def simple_run(self, config):
        self.configure(config)

        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("", "")
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac

        self.obj.engine.start_subprocess = self.start_subprocess

        self.obj.startup()
        self.obj.post_process()

    def test_full(self):
        self.configure({'execution': {
            'throughput': 5,
            'hold-for': '30',
            'scenario': {'script': VEGETA_SCRIPT}}
        })
        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("", "")
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac

        self.obj.engine.start_subprocess = self.start_subprocess

        self.obj.get_widget()
        self.obj.vegeta.tool_name = TOOL_NAME
        self.obj.startup()
        self.obj.check()
        self.obj.post_process()

    def test_no_bin_file(self):
        self.simple_run({
            'execution': {
                'scenario': {'script': VEGETA_SCRIPT},
                'executor': 'vegeta'
            },
        })
        self.assertNotIn('-output', self.CMD_LINE)

    def test_throughput(self):
        self.simple_run({
            'execution': {
                'throughput': '5',
                'scenario': {'script': VEGETA_SCRIPT},
                'executor': 'vegeta'
            },
        })
        self.assertCmd('-rate', 5)

    def test_hold_for(self):
        self.simple_run({
            'execution': {
                'hold-for': '30',
                'scenario': {'script': VEGETA_SCRIPT},
                'executor': 'vegeta'
            }
        })
        self.assertCmd('-duration', '30s')

    def test_concurrency(self):
        self.simple_run({
            'execution': {
                'concurrency': '100',
                'scenario': {'script': VEGETA_SCRIPT},
                'executor': 'vegeta'
            }
        })
        self.assertCmd('-max-workers', '100')

    def test_timeout(self):
        self.simple_run({
            'execution': {
                'scenario': {'script': VEGETA_SCRIPT, 'timeout': 30},
                'executor': 'vegeta'
            }
        })
        self.assertCmd('-timeout', '30s')

    def test_script(self):
        self.simple_run({
            'execution': {
                'iterations': '100',
                'scenario': {'script': VEGETA_SCRIPT},
                'executor': 'vegeta'
            },
        })
        self.assertCmd('-targets', VEGETA_SCRIPT)

    def test_requests_no_body(self):
        self.simple_run({
            'execution': {
                'iterations': '100',
                'scenario': 'vegeta-test',
                'executor': 'vegeta'
            },
            'scenarios': {
                'vegeta-test': {
                    'requests': [{
                        'url': 'http://localhost:8000',
                        'method': 'HEAD',
                        'headers': {'X-Account-ID': 8675309}
                    }]
                }
            }
        })
        with open(self.obj.script, 'r') as f:
            self.assertEqual(f.read(),
                             'HEAD http://localhost:8000\nX-Account-ID: 8675309\n\n')

    def test_requests_with_body(self):
        self.simple_run({
            'execution': {
                'iterations': '100',
                'scenario': 'vegeta-test',
                'executor': 'vegeta'
            },
            'scenarios': {
                'vegeta-test': {
                    'requests': [{
                        'url': 'http://localhost:8080',
                        'method': 'POST',
                        'headers': {'Content-Type': 'application/json'},
                        'body': {'str': 'something', 'number': 1.25, 'boolean': True}
                    }]
                }
            }
        })
        json_file = os.path.join(self.engine.artifacts_dir, 'body-0.json')
        with open(self.obj.script, 'r') as f:
            self.assertEqual(f.read(),
                             f'POST http://localhost:8080\nContent-Type: application/json\n@{json_file}\n\n')
        with open(json_file, 'r') as f:
            self.assertEqual(f.read(),
                             '{"str": "something", "number": 1.25, "boolean": true}')

    @unittest.skipIf(is_windows(), "disabled on windows")
    def test_install_vegeta(self):
        path = os.path.abspath(BUILD_DIR + 'vegeta')
        shutil.rmtree(os.path.dirname(path), ignore_errors=True)

        download_link = "file:///" + RESOURCES_DIR + "vegeta/vegeta-dist-{version}.tar.gz"
        vegeta_version = '12.8.4'

        self.assertFalse(os.path.exists(path))
        self.obj.settings.merge({
            "path": path,
            "download-link": download_link,
            "version": vegeta_version})

        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "vegeta/vegeta.in"}})

        self.obj.prepare()
        self.assertTrue(os.path.exists(path))

    @unittest.skipIf(not is_windows(), "only for windows")
    def test_install_vegeta_win(self):
        self.obj.settings.merge({
            "path": os.path.abspath(BUILD_DIR + 'vegeta'),
            "download-link": "",
            "version": '12.8.4'})

        self.obj.execution.merge({"scenario": {"script": RESOURCES_DIR + "vegeta/vegeta.in"}})
        self.assertRaises(ToolError, self.obj.prepare)


class TestVegetaReader(BZTestCase):
    def test_read(self):
        log_path = os.path.join(RESOURCES_DIR, 'vegeta', 'vegeta_kpi.csv')
        obj = VegetaLogReader(log_path, ROOT_LOGGER)
        points = list(obj.datapoints(True))

        for datapoint in points:
            self.assertTrue(datapoint['ts'] > 1500000000)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE][''][KPISet.SUCCESSES], 3)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE][''][KPISet.FAILURES], 1)
