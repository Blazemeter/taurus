import importlib.util
import json
import os
import tempfile

import yaml
from types import SimpleNamespace
from unittest import mock

from bzt.modules._remote_pytest import RemotePyTestExecutor, RemotePyTestLoadReader
from bzt.modules.functional import LoadSamplesReader
from bzt.utils import BetterDict, RESOURCES_DIR, load_class
from tests.unit import BZTestCase
from tests.unit.mocks import EngineEmul


def load_runner():
    spec = importlib.util.spec_from_file_location("pytest_runner_under_test",
                                                  os.path.join(RESOURCES_DIR, "pytest_runner.py"))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def report(test, when, duration=0.1):
    return SimpleNamespace(nodeid="t.py::" + test, location=("t.py", 1, test), when=when, duration=duration,
                           passed=True, failed=False, skipped=False)


class TestRemoteRecordingPlugin(BZTestCase):
    def setUp(self):
        super(TestRemoteRecordingPlugin, self).setUp()
        self.runner = load_runner()
        self.report_path = tempfile.mktemp(suffix=".ldjson")

    def tearDown(self):
        if os.path.exists(self.report_path):
            os.remove(self.report_path)
        super(TestRemoteRecordingPlugin, self).tearDown()

    def _rows(self):
        with open(self.report_path) as fds:
            return [json.loads(line) for line in fds if line.strip()]

    def _feed(self, plugin, reports):
        plugin.prepare()
        for rep in reports:
            plugin.pytest_runtest_logreport(rep)
        plugin.post_process()

    def test_interleaved_workers(self):
        """Reports of two tests running in parallel (pytest-xdist) must not be mixed up"""
        plugin = self.runner.RemoteRecordingPlugin(self.report_path)
        self._feed(plugin, [
            report("test_a", "setup"), report("test_b", "setup"),
            report("test_a", "call"), report("test_b", "call"),
            report("test_a", "teardown"), report("test_b", "teardown"),
        ])
        rows = self._rows()
        self.assertEqual(["test_a", "test_b"], sorted(row["test_case"] for row in rows))
        self.assertEqual([2, 2], [row["concurrency"] for row in rows])
        self.assertEqual({"PASSED"}, {row["status"] for row in rows})

    def test_sequential_tests(self):
        plugin = self.runner.RemoteRecordingPlugin(self.report_path)
        self._feed(plugin, [
            report("test_a", "setup"), report("test_a", "call"), report("test_a", "teardown"),
            report("test_b", "setup"), report("test_b", "call"), report("test_b", "teardown"),
        ])
        rows = self._rows()
        self.assertEqual(["test_a", "test_b"], [row["test_case"] for row in rows])
        self.assertEqual([1, 1], [row["concurrency"] for row in rows])

    def test_state_is_cleaned_after_teardown(self):
        plugin = self.runner.RemoteRecordingPlugin(self.report_path)
        self._feed(plugin, [report("test_a", "setup"), report("test_a", "call"), report("test_a", "teardown")])
        self.assertEqual({}, plugin._samples)
        self.assertEqual({}, plugin._peaks)

    def test_default_plugin_output_is_unchanged(self):
        """Without --remote-metrics the rows have no extra keys"""
        plugin = self.runner.RecordingPlugin(self.report_path)
        self._feed(plugin, [report("test_a", "setup"), report("test_a", "call"), report("test_a", "teardown")])
        rows = self._rows()
        self.assertEqual(1, len(rows))
        self.assertNotIn("concurrency", rows[0])
        self.assertNotIn("workerID", rows[0])

    def _plugin_used(self, **kwargs):
        used = []

        def fake_main(args, plugins):
            plugins[0].test_count = 1
            used.append(plugins[0])
            return 0

        with mock.patch.object(self.runner.pytest, "main", side_effect=fake_main):
            self.runner.run_pytest(["t.py"], self.report_path, 1, 0, "0", **kwargs)
        return used[0]

    def test_plugin_choice(self):
        self.assertIsInstance(self._plugin_used(remote_metrics=True), self.runner.RemoteRecordingPlugin)
        self.assertNotIsInstance(self._plugin_used(), self.runner.RemoteRecordingPlugin)


class TestRemotePyTestLoadReader(BZTestCase):
    ROW = {"test_case": "test_a", "test_suite": "s", "status": "PASSED", "start_time": 1000.5, "duration": 1.5}

    def _reader(self):
        return RemotePyTestLoadReader(tempfile.mktemp(), mock.MagicMock())

    def test_concurrency_from_row(self):
        sample = self._reader().extract_sample(dict(self.ROW, concurrency=3))
        self.assertEqual(3, sample[2])
        self.assertEqual("test_a", sample[1])
        self.assertEqual(1.5, sample[3])

    def test_no_concurrency_in_row(self):
        sample = self._reader().extract_sample(dict(self.ROW))
        self.assertEqual(1, sample[2])

    def test_skipped_is_ignored(self):
        self.assertIsNone(self._reader().extract_sample(dict(self.ROW, status="SKIPPED", concurrency=3)))

    def test_default_reader_ignores_concurrency(self):
        reader = LoadSamplesReader(tempfile.mktemp(), mock.MagicMock())
        self.assertEqual(1, reader.extract_sample(dict(self.ROW, concurrency=3))[2])


class TestRemotePyTestExecutor(BZTestCase):
    def test_load_reader_class(self):
        obj = RemotePyTestExecutor()
        obj.log = mock.MagicMock()
        self.assertIsInstance(obj.create_load_reader("report.ldjson"), RemotePyTestLoadReader)

    def test_runner_gets_remote_metrics_flag(self):
        obj = RemotePyTestExecutor()
        obj.engine = EngineEmul()
        obj.settings = BetterDict()
        obj.execution = BetterDict.from_dict({"scenario": {"script": "t.py"}})
        obj.script = "t.py"
        obj.remote_artifacts_path = "C:/art"
        obj.remote_report_path = "C:/art/report.ldjson"
        obj.report_file = "report.ldjson"
        obj.file_url = "http://bridge/file"
        with mock.patch.object(obj, "upload_file"), \
                mock.patch.object(obj, "command", return_value={"pid": 1}) as command, \
                mock.patch("bzt.modules._remote_pytest.BridgeFilePuller"):
            obj.startup()
        self.assertIn("--remote-metrics", command.call_args[0][0])


class TestRemoteModuleRegistry(BZTestCase):
    REMOTE = {
        "remote-pytest": "bzt.modules._remote_pytest.RemotePyTestExecutor",
        "remote-jmeter": "bzt.modules.jmeter.RemoteJmeterExecutor",
        "remote-junit": "bzt.modules.java.RemoteJUnitTester",
        "remote-selenium": "bzt.modules._selenium.RemoteSeleniumExecutor",
        "remote-playwright": "bzt.modules._remote_playwright.RemotePlaywrightExecutor",
    }
    NORMAL = {
        "pytest": "bzt.modules._pytest.PyTestExecutor",
        "jmeter": "bzt.modules.jmeter.JMeterExecutor",
        "junit": "bzt.modules.java.JUnitTester",
        "selenium": "bzt.modules._selenium.SeleniumExecutor",
        "playwright": "bzt.modules.javascript.PlaywrightTester",
    }

    def setUp(self):
        super(TestRemoteModuleRegistry, self).setUp()
        with open(os.path.join(RESOURCES_DIR, "10-base-config.yml")) as fds:
            documents = [doc for doc in yaml.safe_load_all(fds) if doc]
        self.modules = next(doc["modules"] for doc in documents if "modules" in doc)

    def test_remote_aliases_are_registered(self):
        for alias, class_name in self.REMOTE.items():
            self.assertEqual(class_name, self.modules[alias]["class"], alias)
            load_class(class_name)

    def test_normal_aliases_are_unchanged(self):
        for alias, class_name in self.NORMAL.items():
            self.assertEqual(class_name, self.modules[alias]["class"], alias)

    def test_remote_jmeter_has_protocol_handlers(self):
        self.assertTrue(self.modules["jmeter"]["protocol-handlers"])
        self.assertEqual(self.modules["jmeter"]["protocol-handlers"],
                         self.modules["remote-jmeter"]["protocol-handlers"])
