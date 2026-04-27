"""
Copyright 2024 BlazeMeter Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""
import json
import os
import platform
import urllib.request
from collections import Counter
from datetime import datetime

from bzt import TaurusConfigError, ToolError
from bzt.modules import ScenarioExecutor
from bzt.modules.console import ExecutorWidget
from bzt.modules.aggregator import ResultsReader, ConsolidatingAggregator, KPISet, DataPoint
from bzt.utils import RequiredTool, CALL_PROBLEMS, FileReader, shutdown_process, \
    unzip, untar, is_windows, is_mac, get_full_path


class MCPExecutor(ScenarioExecutor):
    def __init__(self):
        super(MCPExecutor, self).__init__()
        self.process = None
        self.tool = None
        self.script = None
        self.kpi_file = None
        self.report_prefix = None

    def prepare(self):
        super(MCPExecutor, self).prepare()
        self.install_required_tools()

        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("'script' should be present for mcp executor")

        self.stdout = open(self.engine.create_artifact("mcp", ".out"), "w")
        self.stderr = open(self.engine.create_artifact("mcp", ".err"), "w")

        self.report_prefix = self.engine.create_artifact("mcp-report", "")
        self.kpi_file = self.report_prefix + ".jsonl"
        self.reader = MCPLogReader(self.kpi_file, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def startup(self):
        flag = "-s" if self.settings.get("suite", False) else "-f"
        cmdline = [self.tool.tool_name, flag, self.script, "-o", self.report_prefix, "-reportType", "realtime"]

        if self.settings.get("verbose", False):
            cmdline += ["-verbose"]

        user_cmd = self.settings.get("cmdline")
        if user_cmd:
            cmdline += user_cmd.split(" ")

        self.process = self._execute(cmdline)

    def get_widget(self):
        if not self.widget:
            label = "%s" % self
            self.widget = ExecutorWidget(self, "MCP: " + label.split('/')[1])
        return self.widget

    def check(self):
        retcode = self.process.poll()
        if retcode is not None:
            if retcode != 0:
                raise ToolError(f"agent-benchmark exited with non-zero code: {retcode}")
            return True
        return False

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def post_process(self):
        if self.kpi_file:
            self.engine.existing_artifact(self.kpi_file)
        super(MCPExecutor, self).post_process()

    def install_required_tools(self):
        self.tool = self._get_tool(AgentBenchmark, config=self.settings)
        if not self.tool.check_if_installed():
            self.tool.install()

    def resource_files(self):
        return [self.get_script_path(required=True)]


class MCPLogReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(MCPLogReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self._error_details = {}  # {t_stamp: {label: {"exec": [...], "assert": [...]}}}

    def _read(self, last_pass=False):
        lines = list(self.file.get_lines(size=1024 * 1024, last_pass=last_pass))
        for line in lines:
            line = line.strip()
            if not line or line == "END":
                continue
            try:
                row = json.loads(line)
            except json.JSONDecodeError:
                self.log.warning("Failed to parse JSONL line: %s", line[:100])
                continue

            if row.get("type") != "test":
                continue

            data = row.get("data", {})
            execution = data.get("execution", {})
            passed = data.get("passed", True)

            end_time = execution.get("endTime", "")
            try:
                dt = datetime.fromisoformat(end_time)
                t_stamp = int(dt.timestamp())
            except (ValueError, AttributeError):
                self.log.warning("Could not parse timestamp: %s", end_time)
                continue

            label = execution.get("agentName", "") + "/" + execution.get("testName", "")
            concurrency = execution.get("activeCount", 1)
            latency_ms = execution.get("latencyMs", 0)
            r_time = latency_ms / 1000.0

            tool_calls = execution.get("toolCalls", [])
            con_time = sum(tc.get("duration_ms", 0) for tc in tool_calls) / 1000.0
            latency = max(0.0, r_time - con_time)

            r_code = "200" if passed else "500"
            error_msg = None
            if not passed:
                exec_errors = [str(e) for e in execution.get("errors", [])]
                failed_assertions = [
                    a.get("message", "Assertion failed")
                    for a in data.get("assertions", [])
                    if not a.get("passed", True)
                ]
                self._error_details.setdefault(t_stamp, {})[label] = {
                    "exec": exec_errors,
                    "assert": failed_assertions,
                }
                all_messages = exec_errors + failed_assertions
                error_msg = "; ".join(all_messages) if all_messages else "Test failed"

            trname = execution.get("sessionName", "")
            byte_count = execution.get("tokensUsed", 0)

            yield (t_stamp, label, concurrency, r_time, con_time, latency, r_code, error_msg, trname, byte_count)

    def _calculate_datapoints(self, final_pass=False):
        for point in super(MCPLogReader, self)._calculate_datapoints(final_pass):
            t_stamp = point[DataPoint.TIMESTAMP]
            label_details = self._error_details.pop(t_stamp, {})
            current = point[DataPoint.CURRENT]
            for label, details in label_details.items():
                if label not in current:
                    continue
                kpi = current[label]
                new_errors = []
                for msg in details["exec"]:
                    item = KPISet.error_item_skel(msg, "500", 1, KPISet.ERRTYPE_ERROR, Counter(), None)
                    KPISet.inc_list(new_errors, ("msg", msg), item)
                for msg in details["assert"]:
                    item = KPISet.error_item_skel(msg, "500", 1, KPISet.ERRTYPE_ASSERT, Counter(), None)
                    KPISet.inc_list(new_errors, ("msg", msg), item)
                if new_errors:
                    kpi[KPISet.ERRORS] = new_errors
            yield point


class AgentBenchmark(RequiredTool):
    GITHUB_REPO = "mykhaliev/agent-benchmark"
    LOCAL_PATH = "~/.bzt/agent-benchmark"

    def __init__(self, config=None, **kwargs):
        settings = config or {}
        self._config_version = settings.get("version", None)  # None = fetch latest at install time
        exe_suffix = ".exe" if is_windows() else ""
        install_dir = get_full_path(settings.get("path", self.LOCAL_PATH))
        tool_path = os.path.join(install_dir, "agent-benchmark" + exe_suffix)
        super(AgentBenchmark, self).__init__(tool_path=tool_path, installable=True, **kwargs)
        self.tool_name = "agent-benchmark"
        self.install_dir = install_dir

    def check_if_installed(self):
        """Check PATH first, then the Taurus-managed install location."""
        # 1. Try from system PATH
        try:
            out, err = self.call(["agent-benchmark", "-v"])
            self.log.debug("Found agent-benchmark in PATH: %s", (out + err).strip())
            self.tool_name = "agent-benchmark"
            return True
        except CALL_PROBLEMS:
            pass

        # 2. Try Taurus-managed location
        if os.path.isfile(self.tool_path):
            try:
                out, err = self.call([self.tool_path, "-v"])
                self.log.debug("Found agent-benchmark at %s: %s", self.tool_path, (out + err).strip())
                self.tool_name = self.tool_path
                return True
            except CALL_PROBLEMS as exc:
                self.log.warning("agent-benchmark at %s is not executable: %s", self.tool_path, exc)

        return False

    def _get_latest_version(self):
        """Fetch the latest release tag from the GitHub API."""
        url = f"https://api.github.com/repos/{self.GITHUB_REPO}/releases/latest"
        self.log.debug("Fetching latest agent-benchmark version from %s", url)
        try:
            req = urllib.request.Request(url, headers={"Accept": "application/vnd.github+json"})
            with urllib.request.urlopen(req, timeout=15) as resp:
                data = json.loads(resp.read())
                tag = data.get("tag_name", "")
                if not tag:
                    raise ToolError("GitHub API returned no tag_name for latest release")
                self.log.debug("Latest agent-benchmark version: %s", tag)
                return tag
        except ToolError:
            raise
        except Exception as exc:
            raise ToolError(f"Failed to fetch latest agent-benchmark version from GitHub: {exc}") from exc

    def _build_download_link(self, version):
        """Construct the GitHub release download URL for the current platform."""
        machine = platform.machine().lower()
        arch = "arm64" if machine in ("arm64", "aarch64") else "amd64"

        if is_windows():
            os_name, ext = "windows", "zip"
        elif is_mac():
            os_name, ext = "darwin", "tar.gz"
        else:
            os_name, ext = "linux", "tar.gz"

        archive_name = f"agent-benchmark_{version}_{os_name}_{arch}.{ext}"
        return f"https://github.com/{self.GITHUB_REPO}/releases/download/{version}/{archive_name}"

    def install(self):
        if self.dry_install:
            self.log.info("Dry install for %s", self.tool_name)
            return

        version = self._config_version or self._get_latest_version()
        self.download_link = self._build_download_link(version)

        self.log.info("Installing agent-benchmark %s to %s", version, self.install_dir)

        if not os.path.exists(self.install_dir):
            os.makedirs(self.install_dir)

        suffix = ".zip" if is_windows() else ".tar.gz"
        archive = self._download(suffix=suffix, use_link=True)
        if not archive:
            raise ToolError("agent-benchmark download failed")

        try:
            if self.download_link.endswith(".zip"):
                self.log.info("Unzipping %s to %s", archive, self.install_dir)
                unzip(archive, self.install_dir)
            else:
                self.log.info("Extracting %s to %s", archive, self.install_dir)
                untar(archive, self.install_dir)
        finally:
            if os.path.exists(archive):
                os.remove(archive)

        if not is_windows():
            os.chmod(self.tool_path, 0o755)

        self.tool_name = self.tool_path
        self.log.info("agent-benchmark %s installed successfully", version)

        if not self.check_if_installed():
            raise ToolError("Unable to run agent-benchmark after installation!")
