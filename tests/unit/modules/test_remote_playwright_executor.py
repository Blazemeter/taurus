import json
import os
import tempfile
from unittest import mock

from bzt import TaurusConfigError
from bzt.engine import EXEC
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.functional import FunctionalAggregator
from bzt.modules._remote_playwright import RemotePlaywrightExecutor
from bzt.modules.javascript import PlaywrightLogReader, PlaywrightFuncReader
from bzt.utils import BetterDict
from tests.unit.cases import BZTestCase
from tests.unit.mocks import EngineEmul


class FakeRemoteExecutor:
    """Stand-in for RemoteExecutor so no real bridge HTTP happens."""
    def __init__(self):
        self.remote_artifacts_path = "/tmp/art"
        self.file_url = "http://bridge:8080/file"
        self.bridge_os = "linux"
        self.runner_pid = 0
        self.prepared = False
        self.uploaded = []
        self.commands = []
        self.shutdown_called = False

    def prepare(self):
        self.prepared = True

    def upload_file(self, local, remote):
        self.uploaded.append((local, remote))

    def command(self, command, wait_for_completion=True, use_shell=False, workingDir=None):
        self.commands.append(command)
        return {"pid": 4242}

    def check(self):
        return True

    def shutdown(self):
        self.shutdown_called = True


def _make_executor(engine):
    obj = RemotePlaywrightExecutor()
    obj.engine = engine
    obj.settings = BetterDict()
    obj.settings.merge({"bridge-url": "http://bridge:8080"})
    return obj


def _configure(obj, config):
    obj.engine.config.merge({"settings": {"default-executor": "mock"}})
    obj.engine.config.merge(config)
    obj.engine.unify_config()
    obj.execution = obj.engine.config.get(EXEC)[0]


class TestRemotePlaywrightExecutor(BZTestCase):

    def setUp(self):
        super(TestRemotePlaywrightExecutor, self).setUp()
        self.engine = EngineEmul()
        self.obj = _make_executor(self.engine)
        # create a dummy script
        self.script_dir = tempfile.mkdtemp()
        self.script_path = os.path.join(self.script_dir, "test.spec.ts")
        with open(self.script_path, "w") as f:
            f.write("// playwright test")
        # use FakeRemoteExecutor to avoid real bridge HTTP
        self._fake = FakeRemoteExecutor()

    def _patch_bridge(self):
        """Replace bridge methods on the executor with FakeRemoteExecutor methods."""
        self.obj.remote_artifacts_path = self._fake.remote_artifacts_path
        self.obj.file_url = self._fake.file_url
        self.obj.bridge_os = self._fake.bridge_os
        self.obj.runner_pid = self._fake.runner_pid
        self.obj.upload_file = self._fake.upload_file
        self.obj.command = self._fake.command

    def test_prepare_requires_script(self):
        self.engine.aggregator = ConsolidatingAggregator()
        self.engine.aggregator.engine = self.engine
        _configure(self.obj, {
            EXEC: {
                "executor": "remote-playwright",
                "scenario": {"requests": ["http://example.com"]}}})

        with mock.patch("bzt.modules.requests.get") as mock_get:
            mock_get.return_value = mock.Mock(
                status_code=200, json=lambda: {"rootPath": "/tmp", "os": "linux"})
            mock_get.return_value.raise_for_status = lambda: None
            with mock.patch("bzt.modules.requests.post") as mock_post:
                mock_post.return_value = mock.Mock(
                    status_code=200, json=lambda: {})
                with self.assertRaises(TaurusConfigError):
                    self.obj.prepare()

    def test_prepare_sets_up_reader_and_remote_path(self):
        self.engine.aggregator = ConsolidatingAggregator()
        self.engine.aggregator.engine = self.engine
        _configure(self.obj, {
            EXEC: {
                "executor": "remote-playwright",
                "scenario": {"script": self.script_path}}})

        with mock.patch("bzt.modules.requests.get") as mock_get:
            mock_get.return_value = mock.Mock(
                status_code=200, json=lambda: {"rootPath": "/tmp", "os": "linux"})
            mock_get.return_value.raise_for_status = lambda: None
            with mock.patch("bzt.modules.requests.post") as mock_post:
                mock_post.return_value = mock.Mock(
                    status_code=200, json=lambda: {})
                self.obj.prepare()

        self.assertIsNotNone(self.obj.reader)
        self.assertIsInstance(self.obj.reader, PlaywrightLogReader)
        self.assertTrue(self.obj.remote_report_path.endswith("taurus-playwright-reporter.jsonl"))
        self.assertTrue(self.obj.report_file.endswith(".jsonl"))

    def test_func_reader_used_in_functional_mode(self):
        self.engine.aggregator = FunctionalAggregator()
        self.engine.aggregator.engine = self.engine
        _configure(self.obj, {
            EXEC: {
                "executor": "remote-playwright",
                "scenario": {"script": self.script_path}}})

        with mock.patch("bzt.modules.requests.get") as mock_get:
            mock_get.return_value = mock.Mock(
                status_code=200, json=lambda: {"rootPath": "/tmp", "os": "linux"})
            mock_get.return_value.raise_for_status = lambda: None
            with mock.patch("bzt.modules.requests.post") as mock_post:
                mock_post.return_value = mock.Mock(
                    status_code=200, json=lambda: {})
                self.obj.prepare()

        self.assertIsInstance(self.obj.reader, PlaywrightFuncReader)

    def test_startup_uploads_script_and_builds_command(self):
        self.engine.aggregator = ConsolidatingAggregator()
        self.engine.aggregator.engine = self.engine
        _configure(self.obj, {
            EXEC: {
                "executor": "remote-playwright",
                "iterations": 3,
                "concurrency": 2,
                "scenario": {"script": self.script_path}}})

        with mock.patch("bzt.modules.requests.get") as mock_get:
            mock_get.return_value = mock.Mock(
                status_code=200, json=lambda: {"rootPath": "/tmp", "os": "linux"})
            mock_get.return_value.raise_for_status = lambda: None
            with mock.patch("bzt.modules.requests.post") as mock_post:
                mock_post.return_value = mock.Mock(
                    status_code=200, json=lambda: {})
                self.obj.prepare()

        self._patch_bridge()

        with mock.patch("bzt.modules._remote_playwright.BridgeFilePuller") as MockPuller:
            self.obj.startup()

        # script was uploaded
        self.assertTrue(self._fake.uploaded)
        local, remote = self._fake.uploaded[0]
        self.assertEqual(os.path.basename(local), "test.spec.ts")
        self.assertTrue(remote.endswith("test.spec.ts"))

        # command was issued
        self.assertEqual(1, len(self._fake.commands))
        cmd = self._fake.commands[0]
        self.assertIn("npx playwright test", cmd)
        self.assertIn("--workers 2", cmd)
        self.assertIn("--repeat-each 6", cmd)  # concurrency * iterations = 2 * 3
        self.assertIn("@taurus/playwright-custom-reporter", cmd)
        self.assertIn("TAURUS_PWREPORT_DIR=", cmd)
        self.assertIn("TAURUS_PWREPORT_STDOUT=true", cmd)

        self.assertEqual(4242, self.obj.runner_pid)

    def test_startup_starts_bridge_file_puller(self):
        self.engine.aggregator = ConsolidatingAggregator()
        self.engine.aggregator.engine = self.engine
        _configure(self.obj, {
            EXEC: {
                "executor": "remote-playwright",
                "iterations": 1,
                "scenario": {"script": self.script_path}}})

        with mock.patch("bzt.modules.requests.get") as mock_get:
            mock_get.return_value = mock.Mock(
                status_code=200, json=lambda: {"rootPath": "/tmp", "os": "linux"})
            mock_get.return_value.raise_for_status = lambda: None
            with mock.patch("bzt.modules.requests.post") as mock_post:
                mock_post.return_value = mock.Mock(
                    status_code=200, json=lambda: {})
                self.obj.prepare()

        self._patch_bridge()

        with mock.patch("bzt.modules._remote_playwright.BridgeFilePuller") as MockPuller:
            mock_instance = MockPuller.return_value
            self.obj.startup()

        MockPuller.assert_called_once()
        mock_instance.start.assert_called_once()
        self.assertEqual(1, len(self.obj._pullers))

    def test_shutdown_stops_pullers(self):
        mock_puller = mock.Mock()
        self.obj._pullers = [mock_puller]
        self.obj.runner_pid = 0
        self.obj.bridge_command_url = "http://bridge:8080/command"

        with mock.patch("bzt.modules.requests.post") as mock_post:
            mock_post.return_value = mock.Mock(
                status_code=200, json=lambda: {"output": ""})
            self.obj.shutdown()

        mock_puller.stop.assert_called_once()

    def test_startup_with_duration(self):
        self.engine.aggregator = ConsolidatingAggregator()
        self.engine.aggregator.engine = self.engine
        _configure(self.obj, {
            EXEC: {
                "executor": "remote-playwright",
                "hold-for": "30s",
                "concurrency": 4,
                "scenario": {"script": self.script_path}}})

        with mock.patch("bzt.modules.requests.get") as mock_get:
            mock_get.return_value = mock.Mock(
                status_code=200, json=lambda: {"rootPath": "/tmp", "os": "linux"})
            mock_get.return_value.raise_for_status = lambda: None
            with mock.patch("bzt.modules.requests.post") as mock_post:
                mock_post.return_value = mock.Mock(
                    status_code=200, json=lambda: {})
                self.obj.prepare()

        self._patch_bridge()

        with mock.patch("bzt.modules._remote_playwright.BridgeFilePuller"):
            self.obj.startup()

        cmd = self._fake.commands[0]
        self.assertIn("--workers 4", cmd)
        self.assertIn("--repeat-each 1000", cmd)  # hold-for without iterations = 1000
        self.assertIn("TAURUS_PWREPORT_DURATION=30000", cmd)

    def test_startup_uploads_playwright_config(self):
        # create a playwright.config.ts alongside the script
        config_path = os.path.join(self.script_dir, "playwright.config.ts")
        with open(config_path, "w") as f:
            f.write("// config")

        self.engine.aggregator = ConsolidatingAggregator()
        self.engine.aggregator.engine = self.engine
        _configure(self.obj, {
            EXEC: {
                "executor": "remote-playwright",
                "iterations": 1,
                "scenario": {"script": self.script_path}}})

        with mock.patch("bzt.modules.requests.get") as mock_get:
            mock_get.return_value = mock.Mock(
                status_code=200, json=lambda: {"rootPath": "/tmp", "os": "linux"})
            mock_get.return_value.raise_for_status = lambda: None
            with mock.patch("bzt.modules.requests.post") as mock_post:
                mock_post.return_value = mock.Mock(
                    status_code=200, json=lambda: {})
                self.obj.prepare()

        self._patch_bridge()

        with mock.patch("bzt.modules._remote_playwright.BridgeFilePuller"):
            self.obj.startup()

        # both the script and config were uploaded
        uploaded_names = [os.path.basename(u[0]) for u in self._fake.uploaded]
        self.assertIn("test.spec.ts", uploaded_names)
        self.assertIn("playwright.config.ts", uploaded_names)


class TestPlaywrightFuncReader(BZTestCase):

    def test_maps_pass_fail(self):
        tmpfile = tempfile.NamedTemporaryFile(mode="w", suffix=".jsonl", delete=False)
        try:
            lines = [
                {"label": "suite > test_pass", "timestamp": 1000, "duration": 500},
                {"label": "suite > test_fail", "timestamp": 2000, "duration": 300, "error": "assertion failed"},
                {"label": "standalone_test", "timestamp": 3000, "duration": 100},
            ]
            for line in lines:
                tmpfile.write(json.dumps(line) + "\n")
            tmpfile.flush()
            tmpfile.close()

            import logging
            reader = PlaywrightFuncReader(tmpfile.name, EngineEmul(), logging.getLogger("test"))
            samples = list(reader.read(last_pass=True))

            self.assertEqual(3, len(samples))

            # first: passed
            self.assertEqual("test_pass", samples[0].test_case)
            self.assertEqual("suite", samples[0].test_suite)
            self.assertEqual("PASSED", samples[0].status)
            self.assertAlmostEqual(0.5, samples[0].duration, places=1)

            # second: failed
            self.assertEqual("test_fail", samples[1].test_case)
            self.assertEqual("suite", samples[1].test_suite)
            self.assertEqual("FAILED", samples[1].status)
            self.assertEqual("assertion failed", samples[1].error_msg)

            # third: no suite separator
            self.assertEqual("standalone_test", samples[2].test_case)
            self.assertEqual("Playwright", samples[2].test_suite)
            self.assertEqual("PASSED", samples[2].status)
        finally:
            os.unlink(tmpfile.name)

    def test_handles_ansi_in_error(self):
        tmpfile = tempfile.NamedTemporaryFile(mode="w", suffix=".jsonl", delete=False)
        try:
            line = {"label": "test", "timestamp": 1000, "duration": 100,
                    "error": "\x1b[31mfailed\x1b[0m"}
            tmpfile.write(json.dumps(line) + "\n")
            tmpfile.flush()
            tmpfile.close()

            import logging
            reader = PlaywrightFuncReader(tmpfile.name, EngineEmul(), logging.getLogger("test"))
            samples = list(reader.read(last_pass=True))

            self.assertEqual(1, len(samples))
            self.assertEqual("FAILED", samples[0].status)
            self.assertEqual("failed", samples[0].error_msg)  # ANSI stripped
        finally:
            os.unlink(tmpfile.name)
