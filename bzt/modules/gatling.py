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
import codecs
import json
import os
import re
import time
from collections import defaultdict
from distutils.version import LooseVersion

from bzt import TaurusConfigError, ToolError
from bzt.engine import ScenarioExecutor, Scenario, FileLister, HavingInstallableTools, SelfDiagnosable
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.requests_model import HTTPRequest
from bzt.utils import TclLibrary, EXE_SUFFIX, dehumanize_time, get_full_path, FileReader, RESOURCES_DIR, BetterDict
from bzt.utils import simple_body_dict, CALL_PROBLEMS, numeric_types
from bzt.utils import unzip, RequiredTool, JavaVM, shutdown_process, ensure_is_dict, is_windows


def is_gatling2(ver):
    return LooseVersion(ver) < LooseVersion("3")


class GatlingScriptBuilder(object):
    def __init__(self, load, scenario, parent_logger, class_name, gatling_version=None):
        super(GatlingScriptBuilder, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.load = load
        self.feeder_names = {}
        self.scenario = scenario
        self.class_name = class_name
        if gatling_version is None:
            self.gatling_version = Gatling.VERSION
        else:
            self.gatling_version = gatling_version

    # add prefix 'http://' if user forgot it
    @staticmethod
    def fixed_addr(addr):
        if len(addr) > 0 and not addr.startswith('http'):
            return 'http://' + addr
        else:
            return addr

    @staticmethod
    def indent(text, level):
        return "  " * level + text

    def _get_http(self):
        default_address = self.scenario.get("default-address", "")

        http_str = '("%(addr)s")\n' % {'addr': self.fixed_addr(default_address)}

        if self.scenario.get("retrieve-resources", False):
            regex = self.scenario.get("retrieve-resources-regex")
            params = 'BlackList(), WhiteList("""%s""")' % regex if regex else ""
            http_str += self.indent(".inferHtmlResources(%s)\n" % params, level=2)

        if not self.scenario.get('store-cache', True):
            http_str += self.indent('.disableCaching\n', level=2)

        scenario_headers = self.scenario.get_headers()
        for key in scenario_headers:
            http_str += self.indent('.header("%(key)s", "%(val)s")\n' % {'key': key, 'val': scenario_headers[key]},
                                    level=2)
        return http_str

    def _get_exec(self):
        exec_str = ''
        for req in self.scenario.get_requests():
            if not isinstance(req, HTTPRequest):
                msg = "Gatling simulation generator doesn't support '%s' blocks, skipping"
                self.log.warning(msg, req.NAME)
                continue

            if len(exec_str) > 0:
                exec_str += '.'

            default_address = self.scenario.get("default-address")
            if default_address:
                url = req.url
            else:
                url = self.fixed_addr(req.url)

            exec_str += 'exec(\n'
            exec_template = self.indent('http("%(req_label)s").%(method)s("%(url)s")\n', level=2)
            exec_str += exec_template % {'req_label': req.label, 'method': req.method.lower(), 'url': url}

            for key in req.headers:
                exec_template = self.indent('.header("%(key)s", "%(val)s")\n', level=3)
                exec_str += exec_template % {'key': key, 'val': req.headers[key]}

            # todo: join with the same in get_sampler_pair
            if isinstance(req.body, (dict, list, numeric_types)):
                if req.get_header('content-type') == 'application/json' or isinstance(req.body, numeric_types):
                    req.body = json.dumps(req.body)
                elif not simple_body_dict(req.body):
                    self.log.debug('Header "Content-Type: application/json" is required for body: "%s"', req.body)
                    req.body = json.dumps(req.body)

            if isinstance(req.body, str):
                exec_str += self.indent('.body(%(method)s("""%(body)s"""))\n', level=3)
                exec_str = exec_str % {'method': 'StringBody', 'body': req.body}
            elif isinstance(req.body, dict):
                for key in sorted(req.body.keys()):
                    exec_str += self.indent('.formParam("%(key)s", "%(val)s")\n', level=3)
                    exec_str = exec_str % {'key': key, 'val': req.body[key]}
            elif req.body is not None:
                self.log.warning("Unknown body type: %s", req.body)

            exec_str += self.__get_assertions(req.config.get('assert', []))

            if not req.priority_option('follow-redirects', default=True):
                exec_str += self.indent('.disableFollowRedirect\n', level=3)

            exec_str += self.indent(')', level=1)

            think_time = int(dehumanize_time(req.get_think_time()))
            if think_time:
                exec_str += '.pause(%(think_time)s)' % {'think_time': think_time}

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
        check_result = self.indent('.check(\n', level=3)

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
                check_result += self.indent(check_template % {'sample': sample}, level=4)
                first_check = False

        check_result += '\n' + self.indent(')', level=3) + '\n'

        return check_result

    def _get_feeder_name(self, source_filename):
        base_feeder_name = ".".join(os.path.basename(source_filename).split(".")[:-1])
        base_feeder_name = re.sub(r'[^A-Za-z0-9_]', '', base_feeder_name) + "Feed"

        index = 0
        feeder_name = base_feeder_name
        while feeder_name in self.feeder_names and self.feeder_names[feeder_name] != source_filename:
            index += 1
            feeder_name = base_feeder_name + "_%s" % index

        if feeder_name not in self.feeder_names:
            self.feeder_names[feeder_name] = source_filename

        return feeder_name

    def _get_feeders(self):
        feeders_def = ""
        feeding = ""

        for source in self.scenario.get_data_sources():
            path = self.scenario.engine.find_file(source["path"])

            delimiter = source.get('delimiter', None)
            loop_over = source.get("loop", True)
            var_name = self._get_feeder_name(path)
            params = dict(varname=var_name, filename=path, delimiter=delimiter)
            if delimiter is not None:
                tpl = """val %(varname)s = separatedValues("%(filename)s", '%(delimiter)s')"""
            else:
                tpl = 'val %(varname)s = csv("%(filename)s")'
            line = self.indent(tpl % params, level=1)
            if loop_over:
                line += '.circular'
            feeders_def += line + '\n'
            feeding += "feed(%s)." % var_name

        if feeders_def:
            feeders_def = '\n' + feeders_def

        return feeders_def, feeding

    def gen_test_case(self):
        if is_gatling2(self.gatling_version):
            version = 2
        else:
            version = 3
        template_path = os.path.join(RESOURCES_DIR, "gatling", ("v%s_script.tpl" % version))

        with open(template_path) as template_file:
            template_line = template_file.read()

        feeders_def, feeding = self._get_feeders()

        params = {
            'class_name': self.class_name,
            'httpConf': self._get_http(),
            '_exec': self._get_exec(),
            'feeders': feeders_def,
            'feeding': feeding,
        }
        return template_line % params


class GatlingExecutor(ScenarioExecutor, WidgetProvider, FileLister, HavingInstallableTools, SelfDiagnosable):
    """
    Gatling executor module
    """

    def __init__(self):
        super(GatlingExecutor, self).__init__()
        self.script = None
        self.process = None
        self.end_time = None
        self.retcode = None
        self.simulation_started = False
        self.dir_prefix = "gatling-%s" % id(self)
        self.tool = None

    def get_cp_from_files(self):
        jar_files = []
        files = self.execution.get('files', [])
        for candidate in files:
            candidate = self.engine.find_file(candidate)
            if os.path.isfile(candidate) and candidate.lower().endswith('.jar'):
                jar_files.append(candidate)
            elif os.path.isdir(candidate):
                for element in os.listdir(candidate):
                    element = os.path.join(candidate, element)
                    if os.path.isfile(element) and element.lower().endswith('.jar'):
                        jar_files.append(element)

        return jar_files

    def get_additional_classpath(self):
        cp = self.get_scenario().get("additional-classpath", [])
        cp.extend(self.settings.get("additional-classpath", []))
        return cp

    def prepare(self):
        super(GatlingExecutor, self).prepare()

        self.install_required_tools()
        scenario = self.get_scenario()

        self.env.set({"GATLING_HOME": self.tool.tool_dir})

        cpath = self.get_additional_classpath()
        self.log.debug("Classpath for Gatling: %s", cpath)

        for element in cpath:
            self.env.add_path({"JAVA_CLASSPATH": element})
            self.env.add_path({"COMPILATION_CLASSPATH": element})

        new_name = self.engine.create_artifact('gatling-launcher', EXE_SUFFIX)
        self.log.debug("Building Gatling launcher: %s", new_name)
        self.tool.build_launcher(new_name)

        self.script = self.get_script_path()
        if not self.script:
            if "requests" in scenario:
                self.get_scenario()['simulation'], self.script = self.__generate_script()
            else:
                msg = "There must be a script file or requests for its generation "
                msg += "to run Gatling tool (%s)" % self.execution.get('scenario')
                raise TaurusConfigError(msg)

        self.dir_prefix = self.settings.get("dir-prefix", self.dir_prefix)

        self.stdout = open(self.engine.create_artifact("gatling", ".out"), "w")
        self.stderr = open(self.engine.create_artifact("gatling", ".err"), "w")

        self.reader = DataLogReader(self.engine.artifacts_dir, self.log, self.dir_prefix)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def __generate_script(self):
        simulation = "TaurusSimulation_%s" % id(self)
        file_name = self.engine.create_artifact(simulation, ".scala")
        gen_script = GatlingScriptBuilder(self.get_load(), self.get_scenario(), self.log, simulation, self.tool.version)
        with codecs.open(file_name, 'w', encoding='utf-8') as script:
            script.write(gen_script.gen_test_case())

        return simulation, file_name

    def _get_simulation_props(self):
        props = {}
        if os.path.isfile(self.script):
            if self.script.endswith('.jar'):
                self.env.add_path({"JAVA_CLASSPATH": self.script})
                self.env.add_path({"COMPILATION_CLASSPATH": self.script})
            else:
                props['gatling.core.directory.simulations'] = get_full_path(self.script, step_up=1)
        else:
            props['gatling.core.directory.simulations'] = self.script

        simulation = self.get_scenario().get("simulation")
        if simulation:
            props['gatling.core.simulationClass'] = simulation
        else:
            props['gatling.core.runDescription'] = "Taurus_Test"
        return props

    def _get_load_props(self):
        load = self.get_load()
        props = {}
        if load.concurrency:
            props['concurrency'] = load.concurrency
        if load.ramp_up is not None:
            props['ramp-up'] = int(load.ramp_up)
        if load.hold is not None:
            props['hold-for'] = int(load.hold)
        if load.iterations:
            props['iterations'] = int(load.iterations)
        if load.throughput:
            if load.duration:
                props['throughput'] = load.throughput
            else:
                self.log.warning("You should set up 'ramp-up' and/or 'hold-for' for usage of 'throughput'")
        return props

    def _get_scenario_props(self):
        props = {}
        scenario = self.get_scenario()
        timeout = scenario.get('timeout', None)
        if timeout is not None:
            props['gatling.http.ahc.requestTimeout'] = int(dehumanize_time(timeout) * 1000)

        if scenario.get('keepalive', True):
            # gatling <= 2.2.0
            props['gatling.http.ahc.allowPoolingConnections'] = 'true'
            props['gatling.http.ahc.allowPoolingSslConnections'] = 'true'
            # gatling > 2.2.0
            props['gatling.http.ahc.keepAlive'] = 'true'
        else:
            # gatling <= 2.2.0
            props['gatling.http.ahc.allowPoolingConnections'] = 'false'
            props['gatling.http.ahc.allowPoolingSslConnections'] = 'false'
            # gatling > 2.2.0
            props['gatling.http.ahc.keepAlive'] = 'false'
        return props

    def _set_env(self):
        props = BetterDict()
        props.merge(self.settings.get('properties'))
        props.merge(self.get_scenario().get("properties"))

        props['gatling.core.outputDirectoryBaseName'] = self.dir_prefix
        props['gatling.core.directory.resources'] = self.engine.artifacts_dir
        props['gatling.core.directory.results'] = self.engine.artifacts_dir

        props.merge(self._get_simulation_props())
        props.merge(self._get_load_props())
        props.merge(self._get_scenario_props())
        for key in sorted(props.keys()):
            prop = props[key]
            val_tpl = "%s"

            if isinstance(prop, str):
                if not is_windows():  # extend properties support (contained separators/quotes/etc.) on lin/mac
                    val_tpl = "%r"

            self.env.add_java_param({"JAVA_OPTS": ("-D%s=" + val_tpl) % (key, prop)})

        self.env.set({"NO_PAUSE": "TRUE"})
        self.env.add_java_param({"JAVA_OPTS": self.settings.get("java-opts", None)})

        self.log.debug('JAVA_OPTS: "%s"', self.env.get("JAVA_OPTS"))

    def startup(self):
        self._set_env()
        self.process = self._execute(self._get_cmdline())

    def _get_cmdline(self):
        cmdline = [self.tool.tool_path]

        if is_gatling2(self.tool.version):
            cmdline += ["-m"]  # default for 3.0.0

        return cmdline

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
            with open(self.stdout.name) as out:
                file_header = out.read(1024)
            if wrong_line in file_header:  # gatling can't select test scenario
                scenarios = file_header[file_header.find(wrong_line) + len(wrong_line):].rstrip()
                msg = 'Several gatling simulations are found, you must '
                msg += 'specify one of them to use in "simulation" option: %s' % scenarios
                raise TaurusConfigError(msg)
            if 'started...' in file_header:
                self.simulation_started = True

        if self.retcode is None:
            return False
        elif self.retcode == 0:
            return True
        else:
            raise ToolError("Gatling tool exited with non-zero code: %s" % self.retcode, self.get_error_diagnostics())

    def shutdown(self):
        """
        If tool is still running - let's stop it.
        """
        shutdown_process(self.process, self.log)

        if self.start_time:
            self.end_time = time.time()
            self.log.debug("Gatling worked for %s seconds", self.end_time - self.start_time)

    def post_process(self):
        """
        Save data log as artifact
        """
        if self.reader and self.reader.file and self.reader.file.name:
            self.engine.existing_artifact(self.reader.file.name)
        super(GatlingExecutor, self).post_process()

    def install_required_tools(self):
        self.tool = self._get_tool(Gatling, config=self.settings)
        java = self._get_tool(JavaVM)
        required_tools = [self._get_tool(TclLibrary), java, self.tool]

        for tool in required_tools:
            if not tool.check_if_installed():
                tool.install()

        # old gatling compiler (zinc) is incompatible with new jre
        new_java = java.version and int(java.version) > 8

        if is_gatling2(self.tool.version) and new_java:
            self.log.warning('Gatling v%s is incompatible with Java %s', self.tool.version, java.version)

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
        files = []
        script = self.get_script_path()
        if script:
            files.append(script)
        else:
            for source in self.get_scenario().get_data_sources():
                source_path = self.engine.find_file(source["path"])
                files.append(source_path)
        files.extend(self.get_additional_classpath())
        return files

    def get_error_diagnostics(self):
        diagnostics = []
        if self.stdout is not None:
            with open(self.stdout.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("Gatling STDOUT:\n" + contents)
        if self.stderr is not None:
            with open(self.stderr.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("Gatling STDERR:\n" + contents)
        if self.reader and self.reader.file and self.reader.file.name:
            with open(self.reader.file.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("Simulation log:\n" + contents)
        return diagnostics


class DataLogReader(ResultsReader):
    """ Class to read KPI from data log """

    def __init__(self, basedir, parent_logger, dir_prefix):
        super(DataLogReader, self).__init__()
        self.concurrency = 0
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.basedir = basedir
        self.file = FileReader(file_opener=self.open_fds, parent_logger=self.log)
        self.partial_buffer = ""
        self.delimiter = "\t"
        self.dir_prefix = dir_prefix
        self.guessed_gatling_version = None
        self._group_errors = defaultdict(lambda: defaultdict(set))

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
            user_id = fields[2]
            if fields[3].strip() == "START":
                self.concurrency += 1
                self._group_errors[user_id].clear()
            elif fields[3].strip() == "END":
                self.concurrency -= 1
                self._group_errors.pop(user_id)

        if fields[0].strip() == "GROUP":
            return self.__parse_group(fields)
        elif fields[0].strip() == "REQUEST":
            del fields[0]
            if self.guessed_gatling_version != "3.X":
                del fields[0]
            return self.__parse_request(fields)
        else:
            return None

    def __parse_group(self, fields):
        latency = 0.0
        con_time = 0.0

        if len(fields) < 4:
            label = ""
            t_stamp = int(fields[2]) / 1000.0
            r_time = 0
            error = fields[1]
            r_code = "N/A"
        else:
            if self.guessed_gatling_version != "3.X":
                del fields[1]
            user_id = fields[1]
            label = fields[2]
            if ',' in label:
                return None  # skip nested groups for now
            t_stamp = int(fields[4]) / 1000.0
            r_time = int(fields[5]) / 1000.0

            if label in self._group_errors[user_id]:
                error = ';'.join(self._group_errors[user_id].pop(label))
            else:
                error = None

            if fields[6] == 'OK':
                r_code = '200'
            else:
                r_code = self.__rc_from_msg(fields[-1])
                assert error, label

        return int(t_stamp), label, r_time, con_time, latency, r_code, error

    def __parse_request(self, fields):
        # see LogFileDataWriter.ResponseMessageSerializer in gatling-core

        if len(fields) >= 7 and fields[6]:
            error = fields[6]
        else:
            error = None

        req_hierarchy = fields[1].split(',')[0]
        if req_hierarchy:
            user_id = fields[0]
            if error:
                self._group_errors[user_id][req_hierarchy].add(error)
            return None

        label = fields[2]
        t_stamp = int(fields[4]) / 1000.0
        r_time = (int(fields[4]) - int(fields[3])) / 1000.0
        latency = 0.0
        con_time = 0.0
        if fields[5] == 'OK':
            r_code = '200'
        else:
            r_code = self.__rc_from_msg(fields[-1])

        return int(t_stamp), label, r_time, con_time, latency, r_code, error

    def __rc_from_msg(self, msg):
        _tmp_rc = msg.split("but actually ")[-1]  # gatling-core/src/main/scala/io/gatling/core/check/Validator.scala

        if _tmp_rc.startswith("unexpectedly "):
            _tmp_rc = _tmp_rc[len("unexpectedly "):]
        if _tmp_rc.startswith("found "):
            _tmp_rc = _tmp_rc[len("found "):]

        parts = _tmp_rc.split(' ')
        if len(parts) > 1 and parts[1] == 'is':
            _tmp_rc = parts[0]

        return _tmp_rc if _tmp_rc.isdigit() else 'N/A'

    def _guess_gatling_version(self, fields):
        if fields and fields[-1].strip().startswith("3"):
            return "3.X"
        elif fields[0].strip() in ["USER", "REQUEST", "RUN"]:
            self.log.debug("Parsing Gatling 2.2+ stats")
            return "2.2+"
        elif len(fields) >= 3 and fields[2].strip() in ["USER", "REQUEST", "RUN"]:
            self.log.debug("Parsing Gatling 2.1 stats")
            return "2.1"
        else:
            return None

    def _extract_log_data(self, fields):
        if self.guessed_gatling_version is None:
            self.guessed_gatling_version = self._guess_gatling_version(fields)

        if self.guessed_gatling_version == "2.1":
            return self._extract_log_gatling_21(fields)
        elif self.guessed_gatling_version in ["2.2+", "3.X"]:
            return self._extract_log_gatling_22(fields)
        else:
            return None

    def _read(self, last_pass=False):
        """
        Generator method that returns next portion of data

        :param last_pass:
        """
        lines = self.file.get_lines(size=1024 * 1024, last_pass=last_pass)

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

    def open_fds(self, filename):
        """
        open gatling simulation.log
        """
        if os.path.isdir(self.basedir):
            prog = re.compile("^%s-[0-9]+$" % self.dir_prefix)

            for fname in os.listdir(self.basedir):
                if prog.match(fname):
                    filename = os.path.join(self.basedir, fname, "simulation.log")
                    break

            if not filename or not os.path.isfile(filename):
                self.log.debug('simulation.log not found')
                return
        elif os.path.isfile(self.basedir):
            filename = self.basedir
        else:
            self.log.debug('Path not found: %s', self.basedir)
            return

        if not os.path.getsize(filename):
            self.log.debug('simulation.log is empty')
        else:
            return open(filename, 'rb')


class Gatling(RequiredTool):
    """
    Gatling tool
    """
    DOWNLOAD_LINK = "https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle" \
                    "/{version}/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    VERSION = "3.1.2"
    LOCAL_PATH = "~/.bzt/gatling-taurus/{version}/bin/gatling{suffix}"

    def __init__(self, config=None, **kwargs):
        settings = config or {}
        version = settings.get("version", self.VERSION)
        def_path = self.LOCAL_PATH.format(version=version, suffix=EXE_SUFFIX)
        gatling_path = get_full_path(settings.get("path", def_path))
        download_link = settings.get("download-link", self.DOWNLOAD_LINK).format(version=version)
        super(Gatling, self).__init__(tool_path=gatling_path, download_link=download_link, version=version, **kwargs)

        self.tool_dir = get_full_path(self.tool_path, step_up=2)

    def check_if_installed(self):
        self.log.debug("Trying Gatling...")
        try:
            out, err = self.call([self.tool_path, '--help'])
            self.log.debug("Gatling check output: %s", out)
        except CALL_PROBLEMS as exc:
            self.log.info("Gatling check failed: %s", exc)
            return False

        if err:
            self.log.warning("Gatling check stderr: %s", err)
        return True

    def install(self):
        dest = get_full_path(self.tool_path, step_up=2)
        self.log.info("Will install %s into %s", self.tool_name, dest)
        gatling_dist = self._download(use_link=True)
        self.log.info("Unzipping %s", gatling_dist)
        unzip(gatling_dist, dest, 'gatling-charts-highcharts-bundle-' + self.version)
        os.remove(gatling_dist)
        os.chmod(get_full_path(self.tool_path), 0o755)
        self.log.info("Installed Gatling successfully")
        if not self.check_if_installed():
            raise ToolError("Unable to run %s after installation!" % self.tool_name)

    def build_launcher(self, new_name):  # legacy, for v2 only
        def convert_v2():
            modified_lines = []
            mod_success = False

            with open(self.tool_path) as fds:
                for line in fds.readlines():
                    if is_windows():
                        if line.startswith('set COMPILATION_CLASSPATH=""'):
                            mod_success = True
                            continue  # don't add it to modified_lines - just remove
                    else:
                        if line.startswith('COMPILATION_CLASSPATH='):
                            mod_success = True
                            line = line.rstrip() + ':"${COMPILATION_CLASSPATH}"\n'  # add from env
                        elif line.startswith('"$JAVA"'):
                            line = 'eval ' + line

                    modified_lines.append(line)

            if not mod_success:
                raise ToolError("Can't modify gatling launcher for jar usage, ability isn't supported")

            return modified_lines

        def convert_v3():
            modified_lines = []
            mod_success = False

            with open(self.tool_path) as fds:
                for line in fds.readlines():
                    if is_windows():
                        if line.startswith('set COMPILER_CLASSPATH='):
                            mod_success = True
                            line = line.rstrip() + ';%COMPILATION_CLASSPATH%\n'  # add from env
                        elif line.startswith('set GATLING_CLASSPATH='):
                            mod_success = True
                            line = line.rstrip() + ';%JAVA_CLASSPATH%\n'  # add from env
                    else:
                        if line.startswith('COMPILER_CLASSPATH='):
                            mod_success = True
                            line = line.rstrip()[:-1] + '${COMPILATION_CLASSPATH}"\n'  # add from env
                        elif line.startswith('GATLING_CLASSPATH='):
                            mod_success = True
                            line = line.rstrip()[:-1] + '${JAVA_CLASSPATH}"\n'  # add from env
                        elif line.startswith('"$JAVA"'):
                            line = 'eval ' + line
                    modified_lines.append(line)

            if not mod_success:
                raise ToolError("Can't modify gatling launcher for jar usage, ability isn't supported")

            return modified_lines

        if is_gatling2(self.version):
            converted_lines = convert_v2()
        else:
            converted_lines = convert_v3()

        self.tool_path = new_name

        with open(self.tool_path, 'w') as modified:
            modified.writelines(converted_lines)

        if not is_windows():
            os.chmod(self.tool_path, 0o755)
