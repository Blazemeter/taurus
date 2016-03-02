"""
Module holds all stuff regarding Gatling tool usage

Copyright 2015 BlazeMeter Inc.

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
import os
import re
import shutil
import subprocess
import time

from bzt.engine import ScenarioExecutor, Scenario, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, SidebarWidget
from bzt.utils import BetterDict, TclLibrary, MirrorsManager, EXE_SUFFIX, dehumanize_time
from bzt.utils import unzip, shell_exec, RequiredTool, JavaVM, shutdown_process


class GatlingScriptBuilder(object):
    def __init__(self, load, scenario, parent_logger, class_name):
        super(GatlingScriptBuilder, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.load = load
        self.scenario = scenario
        self.class_name = class_name

    # add prefix 'http://' if user forgot it
    @staticmethod
    def fixed_addr(addr):
        if len(addr) > 0 and not addr.startswith('http'):
            return 'http://' + addr
        else:
            return addr

    def _get_http(self):
        http_str = 'http.baseURL("%(addr)s")\n'
        http_str = http_str % {'addr': self.fixed_addr(self.scenario.get('default-address', ''))}

        scenario_headers = self.scenario.get_headers()
        for key in scenario_headers:
            http_str += '\t\t.header("%(key)s", "%(val)s")\n' % {'key': key, 'val': scenario_headers[key]}
        return http_str

    def _get_exec(self):
        exec_str = ''
        for req in self.scenario.get_requests():
            if len(exec_str) > 0:
                exec_str += '.'

            if len(self.scenario.get('default-address')) > 0:
                url = req.url
            else:
                url = self.fixed_addr(req.url)

            exec_template = 'exec(\n\t\t\thttp("%(req_label)s").%(method)s("%(url)s")\n'
            exec_str += exec_template % {'req_label': req.label, 'method': req.method.lower(), 'url': url}

            for key in req.headers:
                exec_str += '\t\t\t\t.header("%(key)s", "%(val)s")\n' % {'key': key, 'val': req.headers[key]}

            if req.body is not None:
                if isinstance(req.body, str):
                    exec_str += '\t\t\t\t.body(%(method)s(""""%(body)s"""))\n'
                    exec_str = exec_str % {'method': 'StringBody', 'body': req.body}
                else:
                    self.log.warning('Only string and file are supported body content, "%s" ignored' % str(req.body))

            if req.think_time is None:
                think_time = 0
            else:
                think_time = int(dehumanize_time(req.think_time))
            exec_str += '\t\t).pause(%(think_time)s)' % {'think_time': think_time}

        return exec_str

    def gen_test_case(self):
        template_path = os.path.join(os.path.dirname(__file__), os.pardir, 'resources', "gatling_script_template.scala")

        with open(template_path) as template_file:
            template_line = template_file.read()

        params = {
            'class_name': self.class_name,
            'httpConf': self._get_http(),
            '_exec': self._get_exec()
        }
        return template_line % params


class GatlingExecutor(ScenarioExecutor, WidgetProvider, FileLister):
    """
    Gatling executor module
    """
    MIRRORS_SOURCE = "http://gatling.io/views/download.html"
    DOWNLOAD_LINK = "https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle" \
                    "/{version}/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    VERSION = "2.1.7"

    def __init__(self):
        super(GatlingExecutor, self).__init__()
        self.script = None
        self.process = None
        self.start_time = None
        self.end_time = None
        self.retcode = None
        self.reader = None
        self.stdout_file = None
        self.stderr_file = None
        self.widget = None

    def prepare(self):
        self._check_installed()

        scenario = self.get_scenario()

        if Scenario.SCRIPT in scenario:
            self.script = self.__get_script()
            self.engine.existing_artifact(self.script)
            with open(os.path.join(self.engine.artifacts_dir, os.path.basename(self.script)), 'rt') as fds:
                script_contents = fds.read()
            resource_files = GatlingExecutor.__get_res_files_from_script(script_contents)
            if resource_files:
                modified_contents = GatlingExecutor.__modify_res_paths_in_scala(script_contents, resource_files)
                with open(os.path.join(self.engine.artifacts_dir, os.path.basename(self.script)), 'wt') as fds:
                    fds.write(modified_contents)
                self.__cp_res_files_to_artifacts_dir(resource_files)

        elif "requests" in scenario:
            self.get_scenario()['simulation'], self.script = self.__generate_script()
        else:
            raise ValueError("There must be a script file to run Gatling")

        self.reader = DataLogReader(self.engine.artifacts_dir, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def __generate_script(self):
        simulation = "TaurusSimulation_%s" % id(self)
        file_name = self.engine.create_artifact(simulation, ".scala")
        gen_script = GatlingScriptBuilder(self.get_load(), self.get_scenario(), self.log, simulation)
        with open(file_name, 'wt') as script:
            script.write(gen_script.gen_test_case())

        return simulation, file_name

    def startup(self):
        """
        Should start the tool as fast as possible.
        """

        simulation = self.get_scenario().get("simulation", "")
        if not simulation:
            # TODO: guess simulation from script file
            raise ValueError("No simulation set")

        datadir = os.path.realpath(self.engine.artifacts_dir)

        cmdline = [self.settings["path"]]
        cmdline += ["-sf", datadir, "-df", datadir, "-rf ", datadir]
        cmdline += ["-on", "gatling-bzt", "-m", "-s", simulation]

        self.start_time = time.time()
        out = self.engine.create_artifact("gatling-stdout", ".log")
        err = self.engine.create_artifact("gatling-stderr", ".log")
        self.stdout_file = open(out, "w")
        self.stderr_file = open(err, "w")

        params_for_scala = {}
        load = self.get_load()
        scenario = self.get_scenario()

        if scenario.get('timeout', None) is not None:
            params_for_scala['gatling.http.ahc.requestTimeout'] = int(dehumanize_time(scenario.get('timeout')) * 1000)
        if scenario.get('keepalive', None) is not None:
            params_for_scala['gatling.http.ahc.keepAlive'] = scenario.get('keepalive').lower()
        if load.concurrency is not None:
            params_for_scala['concurrency'] = load.concurrency
        if load.ramp_up is not None:
            params_for_scala['ramp-up'] = int(load.ramp_up)
        if load.hold is not None:
            params_for_scala['hold-for'] = int(load.hold)
        if load.iterations is not None and load.iterations != 0:
            params_for_scala['iterations'] = int(load.iterations)

        env = BetterDict()
        env.merge(dict(os.environ))

        java_opts = "".join([" -D%s=%s" % (key, params_for_scala[key]) for key in params_for_scala])
        java_opts += " " + env.get("JAVA_OPTS", "") + " " + self.engine.config.get("java_opts", "")

        env.merge({"JAVA_OPTS": java_opts})

        self.process = shell_exec(cmdline, cwd=self.engine.artifacts_dir, stdout=self.stdout_file,
                                  stderr=self.stderr_file, env=env)

    def check(self):
        """
        Checks if tool is still running. Also checks if resulting logs contains
        any data and throws exception otherwise.

        :return: bool
        :raise RuntimeWarning:
        """
        if self.widget:
            self.widget.update()

        self.retcode = self.process.poll()

        with open(self.stdout_file.name) as out:
            file_header = out.read(1024)
        if file_header.find("Choose a simulation number:") != -1:  # gatling can't select test scenario
            raise ValueError('You must select proper gatling simulation')

        if self.retcode is not None:
            if self.retcode != 0:
                self.log.info("Gatling tool exit code: %s", self.retcode)
                raise RuntimeError("Gatling tool exited with non-zero code")

            return True
        return False

    def shutdown(self):
        """
        If tool is still running - let's stop it.
        """
        shutdown_process(self.process, self.log)

        if self.stdout_file:
            self.stdout_file.close()
        if self.stderr_file:
            self.stderr_file.close()

        if self.start_time:
            self.end_time = time.time()
            self.log.debug("Gatling worked for %s seconds", self.end_time - self.start_time)

    def post_process(self):
        """
        Save data log as artifact
        """
        if self.reader:
            if self.reader.filename:
                self.engine.existing_artifact(self.reader.filename)
            if not self.reader.buffer:
                raise RuntimeWarning("Empty results, most likely Gatling failed")

    def _check_installed(self):
        required_tools = [TclLibrary(self.log), JavaVM("", "", self.log)]
        gatling_path = self.settings.get("path", "~/.bzt/gatling-taurus/bin/gatling" + EXE_SUFFIX)
        gatling_path = os.path.abspath(os.path.expanduser(gatling_path))
        self.settings["path"] = gatling_path
        gatling_version = self.settings.get("version", GatlingExecutor.VERSION)
        required_tools.append(Gatling(gatling_path, self.log, gatling_version))

        for tool in required_tools:
            if not tool.check_if_installed():
                self.log.info("Installing %s", tool.tool_name)
                tool.install()

    def get_widget(self):
        if not self.widget:
            if self.script is not None:
                label = "Script: %s" % os.path.basename(self.script)
            else:
                label = None
            self.widget = SidebarWidget(self, label)
        return self.widget

    def resource_files(self):
        if not self.script:
            self.script = self.__get_script()
        resource_files = []

        if self.script and os.path.exists(self.script):
            script_contents = open(self.script, 'rt').read()
            resource_files = GatlingExecutor.__get_res_files_from_script(script_contents)

            if resource_files:
                script_name, script_ext = os.path.splitext(self.script)
                script_name = os.path.basename(script_name)
                modified_script = self.engine.create_artifact(script_name, script_ext)
                modified_contents = GatlingExecutor.__modify_res_paths_in_scala(script_contents, resource_files)
                self.__cp_res_files_to_artifacts_dir(resource_files)
                with open(modified_script, 'wt') as _fds:
                    _fds.write(modified_contents)
                resource_files.append(modified_script)
            else:
                shutil.copy2(self.script, self.engine.artifacts_dir)
                resource_files.append(self.script)

        return resource_files

    @staticmethod
    def __get_res_files_from_script(script_contents):
        """
        Get resource files list from scala script
        :param script_contents:
        :return:
        """
        resource_files = []
        search_patterns = [re.compile(r'\.formUpload\(".*?"\)'),
                           re.compile(r'RawFileBody\(".*?"\)'),
                           re.compile(r'RawFileBodyPart\(".*?"\)'),
                           re.compile(r'ELFileBody\(".*?"\)'),
                           re.compile(r'ELFileBodyPart\(".*?"\)'),
                           re.compile(r'csv\(".*?"\)'),
                           re.compile(r'tsv\(".*?"\)'),
                           re.compile(r'ssv\(".*?"\)'),
                           re.compile(r'jsonFile\(".*?"\)'),
                           re.compile(r'separatedValues\(".*?"\)')]
        for search_pattern in search_patterns:
            found_samples = search_pattern.findall(script_contents)
            for found_sample in found_samples:
                param_list = found_sample.split(",")
                param_index = 0 if "separatedValues" in search_pattern.pattern else -1  # first or last param
                file_path = re.compile(r'\".*?\"').findall(param_list[param_index])[0].strip('"')
                resource_files.append(file_path)

        return resource_files

    @staticmethod
    def __modify_res_paths_in_scala(scala_script_contents, file_list):
        """

        :param scala_path:
        :param file_list:
        :return:
        """
        for file_path in file_list:
            scala_script_contents = scala_script_contents.replace(file_path, os.path.basename(file_path))
        return scala_script_contents

    def __cp_res_files_to_artifacts_dir(self, resource_files_list):
        """

        :param resource_files_list:
        :return:
        """
        for resource_file in resource_files_list:
            resource_file = self.engine.find_file(resource_file)
            if os.path.exists(resource_file):
                try:
                    shutil.copy(resource_file, self.engine.artifacts_dir)
                except BaseException:
                    self.log.warning("Cannot copy file: %s", resource_file)
            else:
                self.log.warning("File not found: %s", resource_file)

    def __get_script(self):
        """
        get script name
        :return:
        """
        scenario = self.get_scenario()
        if Scenario.SCRIPT not in scenario:
            return None

        fname = scenario[Scenario.SCRIPT]
        if fname is not None:
            return self.engine.find_file(fname)
        else:
            return None


class DataLogReader(ResultsReader):
    """ Class to read KPI from data log """

    def __init__(self, basedir, parent_logger):
        super(DataLogReader, self).__init__()
        self.concurrency = 0
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.basedir = basedir
        self.filename = False
        self.fds = None
        self.partial_buffer = ""
        self.delimiter = "\t"
        self.offset = 0

    def _read(self, last_pass=False):
        """
        Generator method that returns next portion of data

        :param last_pass:
        """
        while not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            yield None

        self.log.debug("Reading gatling results")
        self.fds.seek(self.offset)  # without this we have a stuck reads on Mac
        if last_pass:
            lines = self.fds.readlines()  # unlimited
        else:
            lines = self.fds.readlines(1024 * 1024)  # 1MB limit to read
        self.offset = self.fds.tell()

        for line in lines:
            if not line.endswith("\n"):
                self.partial_buffer += line
                continue

            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""

            line = line.strip()
            fields = line.split(self.delimiter)

            # $scenario  $userId  ${RequestRecordHeader.value}
            # ${serializeGroups(groupHierarchy)}  $name
            # 5requestStartDate  6requestEndDate
            # 7responseStartDate  8responseEndDate
            # 9status
            # ${serializeMessage(message)}${serializeExtraInfo(extraInfo)}$Eol"
            if fields[2].strip() == "USER":
                if fields[3].strip() == "START":
                    self.concurrency += 1
                elif fields[3].strip() == "END":
                    self.concurrency -= 1

            if fields[2].strip() != "REQUEST":
                continue
            label = fields[4]
            t_stamp = int(fields[8]) / 1000.0

            r_time = (int(fields[8]) - int(fields[5])) / 1000.0
            latency = (int(fields[7]) - int(fields[6])) / 1000.0
            con_time = (int(fields[6]) - int(fields[5])) / 1000.0

            if fields[-1] == 'OK':
                r_code = '200'
            else:
                _tmp_rc = fields[-1].split(" ")[-1]
                r_code = _tmp_rc if _tmp_rc.isdigit() else 'No RC'

            if len(fields) >= 11 and fields[10]:
                error = fields[10]
            else:
                error = None

            yield int(t_stamp), label, self.concurrency, r_time, con_time, latency, r_code, error, ''

    def __open_fds(self):
        """
        open gatling simulation.log
        """
        prog = re.compile("^gatling-bzt-[0-9]+$")

        for fname in os.listdir(self.basedir):
            if prog.match(fname):
                self.filename = os.path.join(self.basedir, fname, "simulation.log")
                break

        if not self.filename:
            self.log.debug("File is empty: %s", self.filename)
            return False

        self.fds = open(self.filename)
        return True


class Gatling(RequiredTool):
    """
    Gatling tool
    """

    def __init__(self, tool_path, parent_logger, version):
        super(Gatling, self).__init__("Gatling", tool_path)
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.version = version
        self.mirror_manager = GatlingMirrorsManager(self.log, self.version)

    def check_if_installed(self):
        self.log.debug("Trying Gatling: %s", self.tool_path)
        try:
            gatling_proc = shell_exec([self.tool_path, '--help'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            gatling_output = gatling_proc.communicate()
            self.log.debug("Gatling check: %s", gatling_output)
            return True
        except OSError:
            self.log.debug("Gatling check failed.")
            return False

    def install(self):
        dest = os.path.dirname(os.path.dirname(os.path.expanduser(self.tool_path)))
        dest = os.path.abspath(dest)
        gatling_dist = super(Gatling, self).install_with_mirrors(dest, ".zip")
        self.log.info("Unzipping %s", gatling_dist.name)
        unzip(gatling_dist.name, dest, 'gatling-charts-highcharts-bundle-' + self.version)
        gatling_dist.close()
        os.remove(gatling_dist.name)
        os.chmod(os.path.expanduser(self.tool_path), 0o755)
        self.log.info("Installed Gatling successfully")
        if not self.check_if_installed():
            raise RuntimeError("Unable to run %s after installation!" % self.tool_name)


class GatlingMirrorsManager(MirrorsManager):
    def __init__(self, parent_logger, gatling_version):
        self.gatling_version = gatling_version
        super(GatlingMirrorsManager, self).__init__(GatlingExecutor.MIRRORS_SOURCE, parent_logger)

    def _parse_mirrors(self):
        links = []
        if self.page_source is not None:
            self.log.debug('Parsing mirrors...')
            a_search_pattern = re.compile(r'<a class="lead" href=".*?">Gatling bundle \(zip\)</a>')
            href_search_pattern = re.compile(r'href=".*?">')
            select_element = a_search_pattern.findall(self.page_source)

            if select_element:
                href_elements = href_search_pattern.findall(select_element[0])
                links = [link.strip('href=').strip('">') for link in href_elements]
        default_link = GatlingExecutor.DOWNLOAD_LINK.format(version=self.gatling_version)
        if default_link not in links:
            links.append(default_link)
        self.log.debug('Total mirrors: %d', len(links))
        return links
