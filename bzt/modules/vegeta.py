"""
Copyright 2021 BlazeMeter Inc.

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
from subprocess import PIPE
from bzt import TaurusConfigError, ToolError
from bzt.modules import ScenarioExecutor
from bzt.modules.console import ExecutorWidget
from bzt.modules.aggregator import ResultsReader, ConsolidatingAggregator
from bzt.utils import RequiredTool, CALL_PROBLEMS, FileReader, shutdown_process, get_full_path, is_windows, is_mac, \
    untar


class VegetaExecutor(ScenarioExecutor):
    def __init__(self):
        super(VegetaExecutor, self).__init__()
        self.output_file = None
        self.log_file = None
        self.script = None
        self.process = None
        self.vegeta = None
        self.kpi_file = None
        self.scenario = None

    def prepare(self):
        super(VegetaExecutor, self).prepare()
        self.scenario = self.get_scenario()
        self.install_required_tools()

        self.script = self.get_script_path()
        if not self.script:
            requests = self.scenario.get_requests()
            if not requests:
                raise TaurusConfigError("Either 'script' or 'scenario' should be present for Vegeta executor")
            self.script = os.path.join(self.engine.artifacts_dir, "vegeta.txt")
            with open(self.script, "w") as f:
                i = 0
                for request in requests:
                    f.write("{} {}\n".format(request.method, request.url))
                    headers = "\n".join(["{}: {}".format(key, value) for key, value in request.headers.items()])
                    if headers:
                        f.write("{}\n".format(headers))
                    if request.body:
                        json_body_file = os.path.join(self.engine.artifacts_dir, "body-{}.json".format(i))
                        with open(json_body_file, "w") as g:
                            g.write(json.dumps(request.body))
                        f.write("@{}\n".format(json_body_file))
                    f.write("\n")
                    i += 1

        self.stdout = open(self.engine.create_artifact("Vegeta", ".out"), "w")
        self.stderr = open(self.engine.create_artifact("Vegeta", ".err"), "w")

        self.kpi_file = self.engine.create_artifact("kpi", ".csv")
        self.reader = VegetaLogReader(self.kpi_file, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def startup(self):
        cmdline = [self.vegeta.tool_path, "attack", "-targets", self.script]
        load = self.get_load()

        if load.throughput:
            cmdline += ['-rate', str(load.throughput)]

        if load.hold:
            cmdline += ['-duration', str(int(load.hold)) + "s"]

        if load.concurrency:
            cmdline += ['-max-workers', str(int(load.concurrency))]

        if self.scenario and 'timeout' in self.scenario:
            cmdline += ['-timeout', str(int(self.scenario.get('timeout'))) + "s"]

        user_cmd = self.settings.get("cmdline")
        if user_cmd:
            cmdline += user_cmd.split(" ")

        self.process = self._execute(cmdline, stdout=PIPE, shell=False)
        with open(self.kpi_file, 'wb') as f:
            self._execute([self.vegeta.tool_path, "encode", "-to=csv"], stdin=self.process.stdout, stdout=f, shell=False)

    def get_widget(self):
        if not self.widget:
            label = "%s" % self
            self.widget = ExecutorWidget(self, "Vegeta: " + label.split('/')[1])
        return self.widget

    def check(self):
        retcode = self.process.poll()
        if retcode is not None:
            ToolError(f"Vegeta tool exited with non-zero code: {retcode}")
            return True
        return False

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def post_process(self):
        if self.kpi_file:
            self.engine.existing_artifact(self.kpi_file)
        super(VegetaExecutor, self).post_process()

    def install_required_tools(self):
        self.vegeta = self._get_tool(Vegeta, config=self.settings)
        self.vegeta.tool_name = self.vegeta.tool_name.lower()
        if not self.vegeta.check_if_installed():
            self.vegeta.install()

    def resource_files(self):
        return [self.get_script_path(required=True)]


class VegetaLogReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(VegetaLogReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.file = FileReader(filename=filename, parent_logger=self.log)

    def _read(self, last_pass=False):
        lines = self.file.get_lines(size=1024 * 1024, last_pass=last_pass)

        for line in lines:
            log_vals = [val.strip() for val in line.split(',')]

            _tstamp = int(log_vals[0][:10])
            _url = log_vals[10]
            _concur = 1
            _etime = float(log_vals[2]) / 1000000000.0
            _con_time = 0
            _latency = 0
            _rstatus = log_vals[1]
            _error = log_vals[5] or None
            _bytes = int(log_vals[4])

            yield _tstamp, _url, _concur, _etime, _con_time, _latency, _rstatus, _error, '', _bytes


class Vegeta(RequiredTool):
    DOWNLOAD_LINK = \
        "https://github.com/tsenart/vegeta/releases/download/v{version}/vegeta_{version}_{platform}_amd64.tar.gz "
    VERSION = "12.8.4"
    LOCAL_PATH = "~/.bzt/vegeta-taurus/{version}/"

    def __init__(self, config=None, **kwargs):
        settings = config or {}
        version = settings.get("version", self.VERSION)
        self.tool_path = get_full_path(settings.get("path", self.LOCAL_PATH.format(version=version) + 'vegeta'))
        if not is_windows():
            platform = 'darwin' if is_mac() else 'linux'
            download_link = settings.get("download-link", self.DOWNLOAD_LINK).format(version=version, platform=platform)
        else:
            download_link = ''
        super(Vegeta, self).__init__(tool_path=self.tool_path, download_link=download_link, version=version, **kwargs)

    def check_if_installed(self):
        self.log.debug('Checking Vegeta Framework: %s' % self.tool_path)
        try:
            out, err = self.call([self.tool_path, '-version'])
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            return False

        if err:
            out += err
        self.log.debug("Vegeta output: %s", out)
        return True

    def install(self):
        if is_windows():
            raise ToolError("Unable to install Vegeta on Windows! Manual installation required.")

        dest = get_full_path(self.tool_path, step_up=1)
        if not os.path.exists(dest):
            os.makedirs(dest)

        self.log.info("Will install %s into %s", self.tool_name, dest)
        vegeta_dist = self._download(use_link=True)

        self.log.info("Untaring %s", vegeta_dist)
        untar(vegeta_dist, dest, rel_path='vegeta')
        os.remove(vegeta_dist)
        os.chmod(get_full_path(self.tool_path), 0o755)
        self.log.info("Installed Vegeta successfully")

        if not self.check_if_installed():
            raise ToolError("Unable to run %s after installation!" % self.tool_name)
