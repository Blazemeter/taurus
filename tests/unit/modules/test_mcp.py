import json
import os
import tarfile
import tempfile
from os.path import join
from unittest.mock import patch, MagicMock

import bzt

from bzt import TaurusConfigError
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.mcp import MCPExecutor, MCPLogReader, AgentBenchmark
from bzt.utils import EXE_SUFFIX
from tests.unit import BZTestCase, ExecutorTestCase, RESOURCES_DIR, ROOT_LOGGER

TOOL_NAME = join(RESOURCES_DIR, "mcp", "agent-benchmark_mock" + EXE_SUFFIX)
MCP_SCRIPT = join(RESOURCES_DIR, "mcp", "mcp_config.yaml")


class TestMCPExecutor(ExecutorTestCase):
    EXECUTOR = MCPExecutor
    CMD_LINE = None

    def start_subprocess(self, args, **kwargs):
        self.CMD_LINE = " ".join(args)

    def simple_run(self, config, settings=None):
        self.configure(config)
        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("v0.1.0", "")
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac
        if settings:
            self.obj.settings.merge(settings)
        self.obj.engine.start_subprocess = self.start_subprocess
        self.obj.startup()
        self.obj.post_process()

    def test_full(self):
        self.configure({"execution": {
            "scenario": {"script": MCP_SCRIPT},
            "executor": "mcp",
        }})
        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("v0.1.0", "")
            self.obj.prepare()
        finally:
            bzt.utils.exec_and_communicate = tmp_eac

        self.obj.get_widget()
        self.obj.tool.tool_name = TOOL_NAME
        self.obj.startup()
        self.obj.check()
        self.obj.shutdown()
        self.obj.post_process()

    def test_single_file_mode(self):
        self.simple_run({"execution": {"scenario": {"script": MCP_SCRIPT}, "executor": "mcp"}})
        self.assertIn("-f", self.CMD_LINE)
        self.assertNotIn("-s", self.CMD_LINE)
        self.assertIn("-reportType realtime", self.CMD_LINE)

    def test_suite_mode(self):
        self.simple_run(
            {"execution": {"scenario": {"script": MCP_SCRIPT}, "executor": "mcp"}},
            settings={"suite": True},
        )
        self.assertIn("-s", self.CMD_LINE)
        self.assertNotIn("-f", self.CMD_LINE)

    def test_verbose(self):
        self.simple_run(
            {"execution": {"scenario": {"script": MCP_SCRIPT}, "executor": "mcp"}},
            settings={"verbose": True},
        )
        self.assertIn("-verbose", self.CMD_LINE)

    def test_extra_cmdline(self):
        self.simple_run(
            {"execution": {"scenario": {"script": MCP_SCRIPT}, "executor": "mcp"}},
            settings={"cmdline": "-l /tmp/mcp.log"},
        )
        self.assertIn("-l /tmp/mcp.log", self.CMD_LINE)

    def test_no_script_raises(self):
        self.configure({"execution": {"executor": "mcp", "scenario": {}}})
        tmp_eac = bzt.utils.exec_and_communicate
        try:
            bzt.utils.exec_and_communicate = lambda *args, **kwargs: ("v0.1.0", "")
            self.assertRaises(TaurusConfigError, self.obj.prepare)
        finally:
            bzt.utils.exec_and_communicate = tmp_eac


class TestMCPReader(BZTestCase):
    def test_read(self):
        log_path = join(RESOURCES_DIR, "mcp", "mcp_report.jsonl")
        obj = MCPLogReader(log_path, ROOT_LOGGER)
        points = list(obj.datapoints(True))

        self.assertEqual(len(points), 2)

        for datapoint in points:
            self.assertGreater(datapoint['ts'], 1500000000)

        # cumulative totals at last datapoint
        self.assertEqual(points[-1][DataPoint.CUMULATIVE]['claude-agent/List files'][KPISet.SUCCESSES], 1)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE]['claude-agent/List files'][KPISet.FAILURES], 0)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE]['claude-agent/Delete file'][KPISet.FAILURES], 1)
        self.assertEqual(points[-1][DataPoint.CUMULATIVE]['claude-agent/Delete file'][KPISet.SUCCESSES], 0)

    def test_read_response_times(self):
        log_path = join(RESOURCES_DIR, "mcp", "mcp_report.jsonl")
        obj = MCPLogReader(log_path, ROOT_LOGGER)
        points = list(obj.datapoints(True))

        # first test: latencyMs=3000, toolCalls=[{duration_ms:1000}]
        first = points[0][DataPoint.CURRENT]['claude-agent/List files']
        self.assertAlmostEqual(first[KPISet.AVG_RESP_TIME], 3.0, places=2)

    def test_read_skips_non_test_lines(self):
        log_path = join(RESOURCES_DIR, "mcp", "mcp_report.jsonl")
        obj = MCPLogReader(log_path, ROOT_LOGGER)
        points = list(obj.datapoints(True))
        # summary line and END should be ignored — only 2 test entries
        self.assertEqual(len(points), 2)

    def _write_jsonl(self, rows):
        fd, path = tempfile.mkstemp(suffix=".jsonl")
        with os.fdopen(fd, "w") as f:
            for row in rows:
                f.write(json.dumps(row) + "\n")
        self.addCleanup(os.unlink, path)
        return path

    def _make_row(self, test_name, passed, exec_errors=None, assertions=None, end_time="2026-01-01T10:00:01+00:00"):
        return {
            "type": "test",
            "data": {
                "execution": {
                    "testName": test_name,
                    "agentName": "agent",
                    "endTime": end_time,
                    "toolCalls": [],
                    "tokensUsed": 100,
                    "latencyMs": 1000,
                    "errors": exec_errors or [],
                    "sessionName": "",
                    "activeCount": 1,
                },
                "assertions": assertions or [],
                "passed": passed,
            },
        }

    def test_read_multiple_assertions(self):
        path = self._write_jsonl([self._make_row(
            "Multi assert test", passed=False,
            assertions=[
                {"passed": False, "message": "First assertion failed"},
                {"passed": False, "message": "Second assertion failed"},
                {"passed": True, "message": "This one passed"},
            ],
        )])
        obj = MCPLogReader(path, ROOT_LOGGER)
        points = list(obj.datapoints(True))
        self.assertEqual(len(points), 1)
        errors = points[0][DataPoint.CURRENT]["agent/Multi assert test"][KPISet.ERRORS]
        msgs = {e["msg"] for e in errors}
        self.assertIn("First assertion failed", msgs)
        self.assertIn("Second assertion failed", msgs)
        self.assertNotIn("This one passed", msgs)

    def test_read_assertion_error_type(self):
        path = self._write_jsonl([self._make_row(
            "Assert type test", passed=False,
            assertions=[{"passed": False, "message": "Assertion failed"}],
        )])
        obj = MCPLogReader(path, ROOT_LOGGER)
        points = list(obj.datapoints(True))
        errors = points[0][DataPoint.CURRENT]["agent/Assert type test"][KPISet.ERRORS]
        self.assertEqual(len(errors), 1)
        self.assertEqual(errors[0]["type"], KPISet.ERRTYPE_ASSERT)

    def test_read_errors_and_assertions_combined(self):
        path = self._write_jsonl([self._make_row(
            "Combined test", passed=False,
            exec_errors=["Runtime error"],
            assertions=[{"passed": False, "message": "Assertion failed"}],
        )])
        obj = MCPLogReader(path, ROOT_LOGGER)
        points = list(obj.datapoints(True))
        errors = points[0][DataPoint.CURRENT]["agent/Combined test"][KPISet.ERRORS]
        types = {e["msg"]: e["type"] for e in errors}
        self.assertEqual(types.get("Runtime error"), KPISet.ERRTYPE_ERROR)
        self.assertEqual(types.get("Assertion failed"), KPISet.ERRTYPE_ASSERT)


class TestAgentBenchmarkInstall(BZTestCase):
    def _make_tool(self, config=None):
        tool = AgentBenchmark(config=config, log=ROOT_LOGGER)
        return tool

    def test_check_found_in_path(self):
        tool = self._make_tool()
        with patch.object(tool, "call", return_value=("v0.1.0", "")) as mock_call:
            self.assertTrue(tool.check_if_installed())
            self.assertEqual(tool.tool_name, "agent-benchmark")
            mock_call.assert_called_once_with(["agent-benchmark", "-v"])

    def test_check_not_found(self):
        tool = self._make_tool()
        with patch.object(tool, "call", side_effect=OSError("not found")):
            # PATH fails, local path doesn't exist either
            self.assertFalse(tool.check_if_installed())

    def test_check_found_in_local_path(self):
        tool = self._make_tool()
        call_results = {
            ("agent-benchmark", "-v"): OSError("not in PATH"),
        }

        def fake_call(args, **kwargs):
            key = tuple(args)
            exc = call_results.get(key)
            if exc:
                raise exc
            return "v0.1.0", ""

        with patch.object(tool, "call", side_effect=fake_call):
            with patch("os.path.isfile", return_value=True):
                self.assertTrue(tool.check_if_installed())
                self.assertEqual(tool.tool_name, tool.tool_path)

    def test_install_downloads_and_extracts(self):
        tool = self._make_tool(config={"version": "v0.1.0"})

        # Create a minimal tar.gz in memory to simulate download
        import io
        buf = io.BytesIO()
        with tarfile.open(fileobj=buf, mode="w:gz") as tar:
            content = b"#!/bin/sh\necho v0.1.0\n"
            info = tarfile.TarInfo(name="agent-benchmark")
            info.size = len(content)
            tar.addfile(info, io.BytesIO(content))
        archive_bytes = buf.getvalue()

        def fake_download(suffix=".zip", use_link=False):
            # Write fake archive to a temp file and return its path
            import tempfile
            fd, path = tempfile.mkstemp(suffix=suffix)
            os.write(fd, archive_bytes)
            os.close(fd)
            return path

        with patch.object(tool, "_download", side_effect=fake_download):
            with patch.object(tool, "check_if_installed", return_value=True):
                tool.install()

        # tool_name should be set to the local path after install
        self.assertEqual(tool.tool_name, tool.tool_path)

    def test_get_latest_version(self):
        tool = self._make_tool()
        mock_response = MagicMock()
        mock_response.__enter__ = lambda s: s
        mock_response.__exit__ = MagicMock(return_value=False)
        mock_response.read.return_value = b'{"tag_name": "v0.2.0"}'

        with patch("urllib.request.urlopen", return_value=mock_response):
            version = tool._get_latest_version()
        self.assertEqual(version, "v0.2.0")

    def test_build_download_link(self):
        tool = self._make_tool()
        link = tool._build_download_link("v0.1.0")
        self.assertIn("v0.1.0", link)
        self.assertIn("agent-benchmark", link)
        self.assertTrue(link.endswith(".tar.gz") or link.endswith(".zip"))
