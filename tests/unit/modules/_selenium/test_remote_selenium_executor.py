import os
from unittest import mock

import bzt
import bzt.modules._apiritif
from bzt.engine import EXEC
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules._selenium import RemoteSeleniumExecutor
from tests.unit import RESOURCES_DIR
from tests.unit.modules._selenium import SeleniumTestCase, MockPythonTool


class FakeRemoteExecutor:
    """Stand-in for RemoteExecutor so no real bridge HTTP happens."""
    def __init__(self):
        self.remote_artifacts_path = "C:/art"
        self.file_url = "http://bridge:8080/file"
        self.bridge_os = "windows"
        self.runner_pid = 0
        self.prepared = False
        self.uploaded = []
        self.commands = []
        self.shutdown_called = False
        self._list = []

    def prepare(self):
        self.prepared = True

    def upload_file(self, local, remote):
        self.uploaded.append((local, remote))

    def command(self, command, wait_for_completion=True, use_shell=False, workingDir=None):
        self.commands.append(command)
        return {"pid": 4242}

    def list_files(self, path, glob):
        return self._list

    def check(self):
        return True

    def shutdown(self):
        self.shutdown_called = True


class TestRemoteSeleniumExecutor(SeleniumTestCase):
    EXECUTOR = RemoteSeleniumExecutor

    def setUp(self):
        self._vd_patches = [
            mock.patch("bzt.modules.services.VirtualDisplay.startup"),
            mock.patch("bzt.modules.services.VirtualDisplay.shutdown"),
        ]
        for patch in self._vd_patches:
            patch.start()
        super(TestRemoteSeleniumExecutor, self).setUp()
        self.obj.remote_executor = FakeRemoteExecutor()

    def tearDown(self):
        super(TestRemoteSeleniumExecutor, self).tearDown()
        for patch in self._vd_patches:
            patch.stop()

    def obj_prepare(self):
        tmp = bzt.modules._apiritif.executor.Apiritif
        try:
            bzt.modules._apiritif.executor.Apiritif = MockPythonTool
            bzt.modules._selenium.Selenium.version = "3"
            self.obj.prepare()
        finally:
            bzt.modules._apiritif.executor.Apiritif = tmp

    def test_non_apiritif_runner_delegates_to_super(self):
        # a junit runner must NOT use the remote bridge path
        self.configure({
            EXEC: {
                "executor": "selenium",
                "runner": "junit",
                "scenario": {"script": RESOURCES_DIR + "selenium/jar/dummy.jar"}}})
        with mock.patch("bzt.modules._selenium.SeleniumExecutor.prepare") as super_prepare:
            self.obj.prepare()
        super_prepare.assert_called_once()
        self.assertFalse(self.obj._is_remote_apiritif)
        self.assertFalse(self.obj.remote_executor.prepared)

    def test_prepare_uploads_script_and_sets_reader(self):
        self.engine.aggregator = ConsolidatingAggregator()
        self.engine.aggregator.engine = self.engine
        self.configure({
            EXEC: {
                "executor": "selenium",
                "iterations": 1,
                "scenario": {
                    "requests": [{
                        "label": "exec_it",
                        "actions": ["go(http://blazedemo.com)"]}]}}})
        self.obj_prepare()
        self.assertTrue(self.obj._is_remote_apiritif)
        self.assertTrue(self.obj.remote_executor.prepared)
        # the generated apiritif script was uploaded to the remote artifacts dir
        self.assertTrue(self.obj.remote_executor.uploaded)
        local, remote = self.obj.remote_executor.uploaded[0]
        self.assertEqual(os.path.basename(local), os.path.basename(remote))
        self.assertTrue(remote.startswith("C:/art/"))
        # a reader was created and registered with the aggregator
        self.assertIsNotNone(self.obj.reader)
        self.assertIn(self.obj.reader, self.obj.engine.aggregator.underlings)

    def test_startup_builds_loadgen_command(self):
        self.configure({
            EXEC: {
                "executor": "selenium",
                "iterations": 5,
                "concurrency": 2,
                "scenario": {
                    "requests": [{
                        "label": "exec_it",
                        "actions": ["go(http://blazedemo.com)"]}]}}})
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj_prepare()
        self.obj.startup()
        self.assertEqual(1, len(self.obj.remote_executor.commands))
        cmd = self.obj.remote_executor.commands[0]
        self.assertIn("apiritif.loadgen", cmd)
        self.assertIn("--result-file-template", cmd)
        self.assertIn("--iterations 5", cmd)
        self.assertIn("--concurrency 2", cmd)
        # windows path separators in the assembled command
        self.assertIn("C:\\art", cmd)
        self.assertEqual(4242, self.obj.remote_executor.runner_pid)

    def test_check_registers_only_new_nonempty_files(self):
        self.configure({
            EXEC: {
                "executor": "selenium",
                "iterations": 1,
                "scenario": {
                    "requests": [{
                        "label": "exec_it",
                        "actions": ["go(http://blazedemo.com)"]}]}}})
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj_prepare()
        self.obj.startup()

        registered = []
        self.obj.reader.register_file = lambda f: registered.append(f)

        with mock.patch("bzt.modules._selenium.BridgeFilePuller") as MockPuller:
            mock_instance = MockPuller.return_value
            # tick 1: one ready file + one empty file (skipped)
            self.obj.remote_executor._list = [
                {"name": "apiritif.0.csv", "size": 12},
                {"name": "apiritif.1.csv", "size": 0}]
            self.obj.check()
            # tick 2: the empty one now has data; the first must not re-register
            self.obj.remote_executor._list = [
                {"name": "apiritif.0.csv", "size": 30},
                {"name": "apiritif.1.csv", "size": 8}]
            self.obj.check()

        self.assertEqual(
            [os.path.join(self.obj.engine.artifacts_dir, "apiritif.0.csv"),
             os.path.join(self.obj.engine.artifacts_dir, "apiritif.1.csv")],
            registered)
        self.assertEqual(2, MockPuller.call_count)
        self.assertEqual(2, mock_instance.start.call_count)

    def test_shutdown_and_post_process_use_remote_executor(self):
        self.configure({
            EXEC: {
                "executor": "selenium",
                "iterations": 1,
                "scenario": {
                    "requests": [{
                        "label": "exec_it",
                        "actions": ["go(http://blazedemo.com)"]}]}}})
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj_prepare()
        self.obj.startup()
        self.obj.remote_executor._list = []
        self.obj.shutdown()
        self.assertTrue(self.obj.remote_executor.shutdown_called)
        # must not raise (base post_process touches self.selenium which is None here)
        self.obj.post_process()
        self.assertEqual([], self.obj.get_error_diagnostics())

    def test_has_results_reads_own_reader_on_remote_path(self):
        self.configure({
            EXEC: {
                "executor": "selenium",
                "iterations": 1,
                "scenario": {
                    "requests": [{
                        "label": "exec_it",
                        "actions": ["go(http://blazedemo.com)"]}]}}})
        self.obj.engine.aggregator = ConsolidatingAggregator()
        self.obj_prepare()
        # nothing read yet
        self.assertFalse(self.obj.has_results())
        # simulate the reader having read records
        class _U:
            read_records = 5
        self.obj.reader.underlings.append(_U())
        self.assertTrue(self.obj.has_results())

    def test_non_apiritif_lifecycle_delegates_to_super(self):
        # For a non-apiritif runner, startup/check/shutdown must delegate to the base
        # SeleniumExecutor (the runner), NOT run the apiritif/bridge path.
        self.configure({
            EXEC: {
                "executor": "selenium",
                "runner": "junit",
                "scenario": {"script": RESOURCES_DIR + "selenium/jar/dummy.jar"}}})
        with mock.patch("bzt.modules._selenium.SeleniumExecutor.prepare"), \
             mock.patch("bzt.modules._selenium.SeleniumExecutor.startup") as su, \
             mock.patch("bzt.modules._selenium.SeleniumExecutor.check") as ck, \
             mock.patch("bzt.modules._selenium.SeleniumExecutor.shutdown") as sd:
            self.obj.prepare()
            self.obj.startup()
            self.obj.check()
            self.obj.shutdown()
        su.assert_called_once()
        ck.assert_called_once()
        sd.assert_called_once()
        # the bridge path must NOT have been used
        self.assertEqual([], self.obj.remote_executor.commands)
        self.assertFalse(self.obj.remote_executor.shutdown_called)
