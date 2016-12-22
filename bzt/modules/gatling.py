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
import subprocess
import time

from bzt import TaurusConfigError, ToolError
from bzt.engine import ScenarioExecutor, Scenario, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.modules.services import HavingInstallableTools
from bzt.utils import BetterDict, TclLibrary, EXE_SUFFIX, dehumanize_time, get_full_path
from bzt.utils import unzip, shell_exec, RequiredTool, JavaVM, shutdown_process, ensure_is_dict, is_windows


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

            exec_str += self.__get_assertions(req.config.get('assert', []))

            if req.think_time is None:
                think_time = 0
            else:
                think_time = int(dehumanize_time(req.think_time))
            exec_str += '\t\t).pause(%(think_time)s)' % {'think_time': think_time}

        return exec_str

    @staticmethod
    def __get_check_template(assertion):
        a_not = assertion.get('not', False)
        a_regexp = assertion.get('regexp', False)
        a_subject = assertion.get('subject', Scenario.FIELD_BODY)

        if a_subject == Scenario.FIELD_RESP_CODE:
            if a_not:
                res = 'status.not(%(sample)s)'
            else:
                res = 'status.is(%(sample)s)'
        elif a_subject == Scenario.FIELD_HEADERS:
            res = ''
        else:  # FIELD_BODY
            if a_regexp:
                res = 'regex("""%(sample)s""").'
            else:
                res = 'substring("""%(sample)s""").'
            if a_not:
                res += 'notExists'
            else:
                res += 'exists'
        return res

    def __get_assertions(self, assertions):
        if len(assertions) == 0:
            return ''

        first_check = True
        check_result = '\t' * 4 + '.check(\n'

        for idx, assertion in enumerate(assertions):
            assertion = ensure_is_dict(assertions, idx, "contains")

            error_str = 'You must specify "contains" parameter for assertion item'
            a_contains = assertion.get('contains', TaurusConfigError(error_str))

            check_template = self.__get_check_template(assertion)

            if check_template == '':  # FIELD_HEADERS
                self.log.warning('Sorry, but "headers" subject is not implemented for gatling asserts')
                return ''

            if not isinstance(a_contains, list):
                a_contains = [a_contains]
            for sample in a_contains:
                if not first_check:
                    check_result += ',\n'
                check_result += '\t' * 5 + check_template % {'sample': sample}
                first_check = False

        check_result += ')\n'

        return check_result

    def gen_test_case(self):
        template_path = os.path.join(os.path.dirname(__file__), os.pardir, 'resources', "gatling_script.tpl")

        with open(template_path) as template_file:
            template_line = template_file.read()

        params = {
            'class_name': self.class_name,
            'httpConf': self._get_http(),
            '_exec': self._get_exec()
        }
        return template_line % params


class GatlingExecutor(ScenarioExecutor, WidgetProvider, FileLister, HavingInstallableTools):
    """
    Gatling executor module
    """
    DOWNLOAD_LINK = "https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle" \
                    "/{version}/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    VERSION = "2.1.7"

    def __init__(self):
        super(GatlingExecutor, self).__init__()
        self.script = None
        self.process = None
        self.end_time = None
        self.retcode = None
        self.stdout_file = None
        self.stderr_file = None
        self.simulation_started = False
        self.dir_prefix = ''
        self.launcher = None
        self.jar_list = ''

    def __build_launcher(self):
        modified_launcher = self.engine.create_artifact('gatling-launcher', EXE_SUFFIX)
        origin_launcher = get_full_path(self.settings['path'])
        origin_dir = get_full_path(origin_launcher, step_up=2)
        with open(origin_launcher) as origin:
            origin_lines = origin.readlines()

        modified_lines = []

        mod_success = False
        for line in origin_lines:
            if is_windows() and line.startswith('set COMPILATION_CLASSPATH=""'):
                mod_success = True
                continue
            if not is_windows() and line.startswith('COMPILATION_CLASSPATH='):
                mod_success = True
                line = line.rstrip() + '":${COMPILATION_CLASSPATH}"\n'
            modified_lines.append(line)

        if not mod_success:
            raise ToolError("Can't modify gatling launcher for jar usage, ability isn't supported")

        if is_windows():
            first_line = 'set "GATLING_HOME=%s"\n' % origin_dir
        else:
            first_line = 'GATLING_HOME="%s"\n' % origin_dir
        modified_lines.insert(1, first_line)

        with open(modified_launcher, 'w') as modified:
            modified.writelines(modified_lines)

        if not is_windows():
            os.chmod(modified_launcher, 0o755)

        return modified_launcher

    def prepare(self):
        self.install_required_tools()
        scenario = self.get_scenario()

        jar_files = []
        files = self.execution.get('files', [])
        for candidate in files:
            candidate = get_full_path(self.engine.find_file(candidate))
            if os.path.isfile(candidate) and candidate.lower().endswith('.jar'):
                jar_files.append(candidate)
            elif os.path.isdir(candidate):
                for element in os.listdir(candidate):
                    element = os.path.join(candidate, element)
                    if os.path.isfile(element) and element.lower().endswith('.jar'):
                        jar_files.append(element)

        self.log.debug("JAR files list for Gatling: %s", jar_files)
        if jar_files:
            separator = os.pathsep
            self.jar_list = separator + separator.join(jar_files)

        if is_windows() or jar_files:
            self.log.debug("Building Gatling launcher")
            self.launcher = self.__build_launcher()
        else:
            self.log.debug("Will not build Gatling launcher")
            self.launcher = self.settings["path"]

        if Scenario.SCRIPT in scenario and scenario[Scenario.SCRIPT]:
            self.script = self.get_script_path()
        elif "requests" in scenario:
            self.get_scenario()['simulation'], self.script = self.__generate_script()
        else:
            msg = "There must be a script file or requests for its generation "
            msg += "to run Gatling tool (%s)" % self.execution.get('scenario')
            raise TaurusConfigError(msg)

        self.dir_prefix = 'gatling-%s' % id(self)
        self.reader = DataLogReader(self.engine.artifacts_dir, self.log, self.dir_prefix)
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
        self.start_time = time.time()
        out = self.engine.create_artifact("gatling-stdout", ".log")
        err = self.engine.create_artifact("gatling-stderr", ".log")
        self.stdout_file = open(out, "w")
        self.stderr_file = open(err, "w")

        if os.path.isfile(self.script):
            if self.script.endswith('.jar'):
                self.jar_list += os.pathsep + self.script
                simulation_folder = None
            else:
                simulation_folder = get_full_path(self.script, step_up=1)
        else:
            simulation_folder = self.script

        self.process = self.execute(self.__get_cmdline(simulation_folder),
                                    stdout=self.stdout_file,
                                    stderr=self.stderr_file,
                                    env=self.__get_env())

    def __get_env(self):
        env = BetterDict()
        env.merge(dict(os.environ))

        java_opts = env.get('JAVA_OPTS', '') + ' ' + self.settings.get('java-opts', '')
        java_opts += ' ' + self.__get_params_for_scala()

        env.merge({"JAVA_OPTS": java_opts, "NO_PAUSE": "TRUE"})

        if self.jar_list:
            java_classpath = env.get('JAVA_CLASSPATH', '')
            compilation_classpath = env.get('COMPILATION_CLASSPATH', '')
            java_classpath += self.jar_list
            compilation_classpath += self.jar_list
            env.merge({'JAVA_CLASSPATH': java_classpath, 'COMPILATION_CLASSPATH': compilation_classpath})
        return env

    def __get_cmdline(self, simulation_folder):
        simulation = self.get_scenario().get("simulation")
        data_dir = os.path.realpath(self.engine.artifacts_dir)

        cmdline = [self.launcher]
        cmdline += ["-df", data_dir, "-rf", data_dir]
        cmdline += ["-on", self.dir_prefix, "-m"]

        if simulation_folder:
            cmdline += ["-sf", simulation_folder]

        if simulation:
            cmdline += ["-s", simulation]

        return cmdline

    def __get_params_for_scala(self):
        params_for_scala = self.settings.get('properties')
        load = self.get_load()
        scenario = self.get_scenario()

        if scenario.get('timeout', None) is not None:
            params_for_scala['gatling.http.ahc.requestTimeout'] = int(dehumanize_time(scenario.get('timeout')) * 1000)
        if scenario.get('keepalive', True):
            params_for_scala['gatling.http.ahc.allowPoolingConnections'] = 'true'
            params_for_scala['gatling.http.ahc.allowPoolingSslConnections'] = 'true'
        else:
            params_for_scala['gatling.http.ahc.allowPoolingConnections'] = 'false'
            params_for_scala['gatling.http.ahc.allowPoolingSslConnections'] = 'false'
        if load.concurrency is not None:
            params_for_scala['concurrency'] = load.concurrency
        if load.ramp_up is not None:
            params_for_scala['ramp-up'] = int(load.ramp_up)
        if load.hold is not None:
            params_for_scala['hold-for'] = int(load.hold)
        if load.iterations is not None and load.iterations != 0:
            params_for_scala['iterations'] = int(load.iterations)
        if load.throughput:
            if load.duration:
                params_for_scala['throughput'] = load.throughput
            else:
                self.log.warning("You should set up 'ramp-up' and/or 'hold-for' for usage of 'throughput'")

        return ''.join([" -D%s=%s" % (key, params_for_scala[key]) for key in params_for_scala])

    def check(self):
        """
        Checks if tool is still running. Also checks if resulting logs contains
        any data and throws exception otherwise.

        :return: bool
        :raise TaurusConfigError:
        :raise TaurusToolError:
        """
        self.retcode = self.process.poll()

        # detect interactive mode and raise exception if it found
        if not self.simulation_started:
            wrong_line = "Choose a simulation number:"
            with open(self.stdout_file.name) as out:
                file_header = out.read(1024)
            if wrong_line in file_header:  # gatling can't select test scenario
                scenarios = file_header[file_header.find(wrong_line) + len(wrong_line):].rstrip()
                msg = 'Several gatling simulations are found, you must '
                msg += 'specify one of them to use in "simulation" option: %s' % scenarios
                raise TaurusConfigError(msg)
            if 'started...' in file_header:
                self.simulation_started = True

        if self.retcode is not None:
            if self.retcode != 0:
                raise ToolError("Gatling tool exited with non-zero code: %s" % self.retcode)

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
        if self.reader and self.reader.filename:
            self.engine.existing_artifact(self.reader.filename)

    def install_required_tools(self):
        required_tools = [TclLibrary(self.log), JavaVM("", "", self.log)]
        gatling_path = self.settings.get("path", "~/.bzt/gatling-taurus/bin/gatling" + EXE_SUFFIX)
        gatling_path = os.path.abspath(os.path.expanduser(gatling_path))
        self.settings["path"] = gatling_path
        download_link = self.settings.get("download-link", GatlingExecutor.DOWNLOAD_LINK)
        gatling_version = self.settings.get("version", GatlingExecutor.VERSION)
        required_tools.append(Gatling(gatling_path, self.log, download_link, gatling_version))

        for tool in required_tools:
            if not tool.check_if_installed():
                tool.install()

    def get_widget(self):
        if not self.widget:
            simulation = self.get_scenario().get('simulation', None)
            if simulation == "TaurusSimulation_%s" % id(self):
                simulation = 'generated script'
            if simulation is None:
                simulation = os.path.basename(self.script)
            self.widget = ExecutorWidget(self, 'Gatling: %s' % simulation)
        return self.widget

    def resource_files(self):
        scenario = self.get_scenario()
        script = scenario.get(Scenario.SCRIPT, None)
        if script:
            return [script]
        else:
            return []


class DataLogReader(ResultsReader):
    """ Class to read KPI from data log """

    def __init__(self, basedir, parent_logger, dir_prefix):
        super(DataLogReader, self).__init__()
        self.concurrency = 0
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.basedir = basedir
        self.filename = None
        self.fds = None
        self.partial_buffer = ""
        self.delimiter = "\t"
        self.offset = 0
        self.dir_prefix = dir_prefix
        self.guessed_gatling_version = None

    def _extract_log_gatling_21(self, fields):
        """
        Extract stats from Gatling 2.1 format.

        :param fields:
        :return:
        """
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
            return None

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
        return int(t_stamp), label, r_time, con_time, latency, r_code, error

    def _extract_log_gatling_22(self, fields):
        """
        Extract stats from Gatling 2.2 format
        :param fields:
        :return:
        """
        # 0 ${RequestRecordHeader.value}
        # 1 $scenario
        # 2 $userId
        # 3 ${serializeGroups(groupHierarchy)}
        # 4 $label
        # 5 $startTimestamp
        # 6 $endTimestamp
        # 7 $status
        # [8] ${serializeMessage(message)}${serializeExtraInfo(extraInfo)}

        if fields[0].strip() == "USER":
            if fields[3].strip() == "START":
                self.concurrency += 1
            elif fields[3].strip() == "END":
                self.concurrency -= 1

        if fields[0].strip() != "REQUEST":
            return None

        label = fields[4]
        t_stamp = int(fields[6]) / 1000.0

        r_time = (int(fields[6]) - int(fields[5])) / 1000.0
        latency = 0.0
        con_time = 0.0

        if fields[7] == 'OK':
            r_code = '200'
        else:
            _tmp_rc = fields[-1].split(" ")[-1]
            r_code = _tmp_rc if _tmp_rc.isdigit() else 'No RC'

        if len(fields) >= 9 and fields[8]:
            error = fields[8]
        else:
            error = None
        return int(t_stamp), label, r_time, con_time, latency, r_code, error

    def _guess_gatling_version(self, fields):
        if fields[0].strip() in ["USER", "REQUEST", "RUN"]:
            self.log.debug("Parsing Gatling 2.2 stats")
            return "2.2"
        elif fields[2].strip() in ["USER", "REQUEST", "RUN"]:
            self.log.debug("Parsing Gatling 2.1 stats")
            return "2.1"
        else:
            return None

    def _extract_log_data(self, fields):
        if self.guessed_gatling_version is None:
            self.guessed_gatling_version = self._guess_gatling_version(fields)

        if self.guessed_gatling_version == "2.1":
            return self._extract_log_gatling_21(fields)
        elif self.guessed_gatling_version == "2.2":
            return self._extract_log_gatling_22(fields)
        else:
            return None

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

            data = self._extract_log_data(fields)
            if data is None:
                continue

            t_stamp, label, r_time, con_time, latency, r_code, error = data
            bytes_count = None
            yield t_stamp, label, self.concurrency, r_time, con_time, latency, r_code, error, '', bytes_count

    def __open_fds(self):
        """
        open gatling simulation.log
        """
        prog = re.compile("^%s-[0-9]+$" % self.dir_prefix)

        for fname in os.listdir(self.basedir):
            if prog.match(fname):
                self.filename = os.path.join(self.basedir, fname, "simulation.log")
                break

        if not self.filename:
            self.log.debug("File is empty: %s", self.filename)
            return False

        self.fds = open(self.filename)
        return True

    def __del__(self):
        if self.fds:
            self.fds.close()


class Gatling(RequiredTool):
    """
    Gatling tool
    """

    def __init__(self, tool_path, parent_logger, download_link, version):
        super(Gatling, self).__init__("Gatling", tool_path, download_link.format(version=version))
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.version = version

    def check_if_installed(self):
        self.log.debug("Trying Gatling: %s", self.tool_path)
        try:
            gatling_proc = shell_exec([self.tool_path, '--help'], stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            gatling_output = gatling_proc.communicate()
            self.log.debug("Gatling check is successful: %s", gatling_output)
            return True
        except OSError:
            self.log.info("Gatling check failed.")
            return False

    def install(self):
        dest = get_full_path(self.tool_path, step_up=2)
        self.log.info("Will install %s into %s", self.tool_name, dest)
        gatling_dist = self._download(use_link=True)
        self.log.info("Unzipping %s", gatling_dist)
        unzip(gatling_dist, dest, 'gatling-charts-highcharts-bundle-' + self.version)
        os.remove(gatling_dist)
        os.chmod(os.path.expanduser(self.tool_path), 0o755)
        self.log.info("Installed Gatling successfully")
        if not self.check_if_installed():
            raise ToolError("Unable to run %s after installation!" % self.tool_name)
