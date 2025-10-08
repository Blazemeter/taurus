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
import os
import re
import shutil
import struct
import time
import xml.etree.ElementTree as ET
from collections import defaultdict
from functools import wraps
from io import BytesIO
from typing import Any
from pathlib import Path

from packaging import version

from bzt import TaurusConfigError, ToolError
from bzt.engine import ScenarioExecutor, Scenario
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import ExecutorWidget
from bzt.requests_model import HTTPRequest, SetVariables, HierarchicRequestParser
from bzt.utils import TclLibrary, EXE_SUFFIX, dehumanize_time, get_full_path, FileReader, RESOURCES_DIR, BetterDict
from bzt.utils import CALL_PROBLEMS, convert_body_to_string
from bzt.utils import unzip, RequiredTool, JavaVM, shutdown_process, ensure_is_dict, is_windows


class GatlingScriptBuilder(object):
    def __init__(self, executor,load, scenario, parent_logger, class_name, gatling_version=None):
        super(GatlingScriptBuilder, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.executor = executor
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
    def _fixed_addr_ext(addr,url):
        if len(addr) > 0 and not addr.startswith('http') or not addr.startswith('www') or not addr.startswith('${'):
            if url.startswith('http') or url.startswith('www') or url.startswith('${'):
                return url
            elif not addr.startswith('http'):
                addr = 'http://' + addr
        if not addr.endswith('/') and not url.startswith('/'):
            url ='/' + url
        return addr + url 

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
        dynamicExtractor={}
        default_address = self.scenario.get("default-address", "")
        for req in self.scenario.get_requests(parser=HierarchicRequestParser):
            if isinstance(req, SetVariables):
                if len(exec_str) > 0:
                    exec_str += '.'

                exec_str += "exec(\n"
                exec_str += self.indent("_", level=2)
                for k, v in sorted(req.mapping.items()):
                    exec_str += '.set("%s", "%s")' % (k.replace("\"", "\\\""), v.replace("\"", "\\\""))

                exec_str += "\n" + self.indent(")", level=1)
                continue

            if not isinstance(req, HTTPRequest):
                msg = "Processing '%s' from scenario:%s"
                self.log.info(msg, req.NAME,req.scenario_name)
                _scenario = self.executor.get_scenario(name=req.scenario_name)
                _default_address = _scenario.get("default-address","")
                if _default_address is None or _default_address == '':
                    _default_address = default_address
                requests = _scenario.get_requests(parser=HierarchicRequestParser)
                for request in requests:

                    _list = list(())
                    self._getListIncludes(_list,_default_address,request)
                    for _req in _list:
                        if len(exec_str) > 0:
                            exec_str += '.'
                        exec_str += self._stitchScenarioModel(_req[0],_req[1],dynamicExtractor)        
                continue

            if len(exec_str) > 0:
                exec_str += '.'

            exec_str += self._stitchScenarioModel(req,self.scenario.get("default-address",""),dynamicExtractor) 

        return exec_str

    def _getListIncludes(self,_list,defaultAddress,req):
        if not isinstance(req, HTTPRequest):
            self.log.info("Discovering and binding deeper include-scenario:%s",req.scenario_name)
            _scenario = self.executor.get_scenario(name=req.scenario_name)
            _address = _scenario.get("default-address","")
            requests = _scenario.get_requests(parser=HierarchicRequestParser)
            for request in requests:
                if request.NAME == 'request':
                    #print("Scenario default-address:"+_address)
                    if _address is None or _address =='':
                        _address = defaultAddress
                    self.log.info("Discovered and binding deeper include-scenario:%s default-address:%s",request.label,_address)
                    _list.append((request,_address))
                else:
                    self._getListIncludes(_list,defaultAddress,request)
        else:
            _list.append((req,defaultAddress))

    def _stitchScenarioModel(self,req,address,dynamicExtractor):
        url = self._fixed_addr_ext(address,req.url)

        exec_str = 'exec(\n'
        exec_template = self.indent('http("%(req_label)s").%(method)s("%(url)s")\n', level=2)
        normalizedUrl = self._normalizeContent(dynamicExtractor,url)
        exec_str += exec_template % {'req_label': req.label, 'method': req.method.lower(), 'url': normalizedUrl}

        for key in req.headers:
            exec_template = self.indent('.header("%(key)s", "%(val)s")\n', level=3)
            exec_str += exec_template % {'key': key, 'val': self._normalizeContent(dynamicExtractor,req.headers[key])}

        convert_body_to_string(req)
        if isinstance(req.body, str):
            normalizedBody = self._normalizeContent(dynamicExtractor,req.body)
            stmt = '.body(%(method)s("""%(body)s"""))\n' % {'method': 'StringBody', 'body': normalizedBody}
            exec_str += self.indent(stmt, level=3)
        elif isinstance(req.body, dict):
            for key in sorted(req.body.keys()):
                normalizedFormParam = self._normalizeContent(dynamicExtractor,req.body[key])
                stmt = '.formParam("%(key)s", "%(val)s")\n' % {'key': key, 'val': normalizedFormParam}
                exec_str += self.indent(stmt, level=3)
#        elif req.body is not None:
#            self.log.warning("Unknown body type: %s", req.body)

        exec_str += self.__add_extractors(req,dynamicExtractor)
        exec_str += self.__get_assertions(req.config.get('assert', []),dynamicExtractor)   

        if not req.priority_option('follow-redirects', default=True):
            exec_str += self.indent('.disableFollowRedirect\n', level=3)

        exec_str += self.indent(')', level=1)

        think_time = int(dehumanize_time(req.get_think_time()))
        if think_time:
            exec_str += '.pause(%(think_time)s)' % {'think_time': think_time}

        return exec_str     


    def _normalizeContent(self,dynamicExtractor,subject):
        for key in dynamicExtractor:
            value = dynamicExtractor[key]
            subject = subject.replace(key,value)
        return subject

    @staticmethod
    def _safeEscape(subject):
        subject = re.escape(subject)
        subject = subject.replace("\^","^").replace("\(","(").replace("\)",")").replace("\+","+").replace("\*","*").replace("\.",".").replace("\?","?").replace('"','\\"')
        return subject

    def _get_regex_extractor(self,varname, regexp, match_no,defaults=None):
        str1 =''

        #regexp = GatlingScriptBuilder._safeEscape(regexp)

        str1 = self.indent('.check(\n', level=3)

        str1 += self.indent('regex("', level=4)
        str1 += regexp +'")\n'
        str1 += self.indent('.ofType[(String)]', level=4)

        if defaults:
           str1 += '.withDefault("'
           str1 += defaults+'")'
        
        str1 += '\n'+self.indent('.saveAs("', level=3)
        str1 += varname+'")'
        str1 += '\n' + self.indent(')', level=3) + '\n'
 
        return str1

    def _get_jsonPath_extractor(self,varname, jsonPath,defaults=None):
        str =''
        str = self.indent('.check(\n', level=3)

        str += self.indent('jmesPath("', level=4)
        str += jsonPath +'")\n'
        str += self.indent('.ofType[(String)]', level=4)
        
        if defaults:
           str += '.withDefault("'
           str += defaults+'")'

        str += '\n' + self.indent('.saveAs("', level=3)
        str += varname+'")'
        str += '\n' + self.indent(')', level=3) + '\n'
        return str

    def _get_xpath_extractor(self,varname, jsonPath,defaults=None):
        str =''
        str = self.indent('.check(\n', level=3)

        str += self.indent('xpath("', level=4)
        str += jsonPath +'")'
        if defaults:
           str += '.withDefault("'
           str += defaults+'")'

        str += '\n' + self.indent('.saveAs("', level=3)
        str += varname+'")'
        str += '\n' + self.indent(')', level=3) + '\n'
        return str

    def _get_css_extractor(self,varname, jsonPath,defaults=None):
        str =''
        str = self.indent('.check(\n', level=3)

        str += self.indent('css("', level=4)
        str += jsonPath +'")'
        if defaults:
           str += '.withDefault("'
           str += defaults+'")'

        str += '\n' + self.indent('.saveAs("', level=3)
        str += varname+'")'
        str += '\n' + self.indent(')', level=3) + '\n'
        return str

    def __add_extractors(self, req, dynamicExtractor):
        str =''
        str = self.__add_regexp_ext( req.config.get("extract-regexp",[]),dynamicExtractor)
        str +=self.__add_json_ext( req.config.get("extract-jsonpath",[]),dynamicExtractor)
        str +=self.__add_xpath_ext( req.config.get("extract-xpath",[]),dynamicExtractor)
        str +=self.__add_css_ext( req.config.get("extract-css-jquery",[]),dynamicExtractor)
        return str

    def __add_regexp_ext(self, extractors,dynamicExtractor):
        str =''
        for varname in extractors:
            key = "${"+varname+"}"
            value = "#{"+varname+"}"
            dynamicExtractor[key]=value
            cfg = ensure_is_dict(extractors, varname, "regexp")
            str +=  self._get_regex_extractor(varname,GatlingScriptBuilder._safeEscape(cfg['regexp']),cfg['match-no'],cfg['default'])
        return str

    def __add_json_ext(self, extractors,dynamicExtractor):
        str =''
        for varname in extractors:
            key = "${"+varname+"}"
            value = "#{"+varname+"}"
            dynamicExtractor[key]=value
            cfg = ensure_is_dict(extractors, varname, "jsonpath")
            str +=  self._get_jsonPath_extractor(varname,cfg['jsonpath'],cfg['default'])
        return str

    def __add_xpath_ext(self, extractors,dynamicExtractor):
        str =''
        for varname in extractors:
            key = "${"+varname+"}"
            value = "#{"+varname+"}"
            dynamicExtractor[key]=value
            cfg = ensure_is_dict(extractors, varname, "xpath")
            str +=  self._get_xpath_extractor(varname,cfg['xpath'],cfg['default'])
        return str

    def __add_css_ext(self, extractors,dynamicExtractor):
        str =''
        for varname in extractors:
            key = "${"+varname+"}"
            value = "#{"+varname+"}"
            dynamicExtractor[key]=value
            cfg = ensure_is_dict(extractors, varname, "expression")
            str +=  self._get_css_extractor(varname,cfg['expression'],cfg['default'])
        return str

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

    def __get_assertions(self, assertions,dynamicExtractor):
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

                if str(sample) in dynamicExtractor:
                    sample = dynamicExtractor[str(sample)]
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
        template_path = os.path.join(RESOURCES_DIR, "gatling", "v3_script.tpl")

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


class GatlingExecutor(ScenarioExecutor):
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

        self.script = self.get_script_path()
        if not self.script:
            if "requests" in scenario:
                self.get_scenario()['simulation'], self.script = self.__generate_script()
            else:
                msg = "There must be a script file or requests for its generation "
                msg += "to run Gatling tool (%s)" % self.execution.get('scenario')
                raise TaurusConfigError(msg)

        new_name = self.engine.create_artifact('gatling-launcher', EXE_SUFFIX)
        self.log.debug("Building Gatling launcher: %s", new_name)
        self.tool.build_launcher(new_name, scenario, cpath, self.engine.artifacts_dir, self.script.endswith('scala'))

        self.dir_prefix = self.settings.get("dir-prefix", self.dir_prefix)

        self.stdout = open(self.engine.create_artifact("gatling", ".out"), "w")
        self.stderr = open(self.engine.create_artifact("gatling", ".err"), "w")

        # handle jar file from script/cpath variable -> for gatling 3.8.0 and higher copy files to gatling_home/user-files/lib
        if version.parse(self.tool.version) >= version.parse("3.8.0"):
            self._copy_dependencies()

        self.reader = DataLogReader(self.engine.artifacts_dir, self.log, self.dir_prefix, binary_log=self.tool.is_mvn_gatling())
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def __generate_script(self):
        simulation = "TaurusSimulation_%s" % id(self)
        file_name = self.engine.create_artifact(simulation, ".scala")
        gen_script = GatlingScriptBuilder(self,self.get_load(), self.get_scenario(), self.log, simulation, self.tool.version)
        with codecs.open(file_name, 'w', encoding='utf-8') as script:
            script.write(gen_script.gen_test_case())

        return simulation, file_name

    def _copy_dependencies(self):
        #script + additional jars - using logic for cloud deployment
        self.log.debug("Going to copy test dependencies")
        if self.tool.is_mvn_gatling():
            target_dir = os.path.join(self.tool.tool_dir, "lib")
            os.makedirs(target_dir, exist_ok=True)
        else:
            target_dir = os.path.join(self.tool.tool_dir, "user-files/lib")

        self.log.debug("target dir: %s", target_dir)
        rfiles = self.resource_files()
        for file in rfiles:
            if os.path.exists(file):
                self.log.debug("... gatling dependency is copied: %s", file)
                shutil.copy(file, target_dir)
            else:
                self.log.warning("... gatling dependency not available: %s", file)
        # copy scala file in case it was generated from yml config or included as a script
        if self.script.endswith(".scala"):
            if self.tool.is_mvn_gatling():
                scala_target_dir = os.path.join(self.tool.tool_dir, "src", "test", "scala")
            else:
                scala_target_dir = os.path.join(self.tool.tool_dir, "user-files/simulations")
            os.makedirs(scala_target_dir, exist_ok=True)
            shutil.copy(self.script, scala_target_dir)

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
        return props

    def _set_env(self):
        props = BetterDict()
        props.merge(self.settings.get('properties'))
        props.merge(self.get_scenario().get("properties"))

        props['gatling.core.outputDirectoryBaseName'] = self.dir_prefix #this one is not used in mvn gatling
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

        self.env.add_java_param({"MAVEN_OPTS": self.settings.get("maven-opts", None)})

        self.log.debug('JAVA_OPTS: "%s"', self.env.get("JAVA_OPTS"))

    def startup(self):
        self._set_env()
        self.process = self._execute([self.tool.tool_path])

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

    def __init__(self, basedir, parent_logger, dir_prefix, binary_log=False):
        super(DataLogReader, self).__init__()
        self.concurrency = 0
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.basedir = basedir
        self.file = FileReader(file_opener=self.open_fds, parent_logger=self.log)
        self.binary_log = binary_log
        if self.binary_log:
            self.binary_log_decoder = BinaryLogReader()
        self.partial_buffer = ""
        self.delimiter = "\t"
        self.dir_prefix = dir_prefix
        self.guessed_gatling_version = None
        self._group_errors = defaultdict(set)

    def _extract_log(self, fields):
        """
        Extract stats from Gatling format of version 3.1 and after
        :param fields:
        :return:
        """
        # 0 ${RequestRecordHeader.value}
        # 1 $scenario
        # -|2 $userId, absent in Gatling 3.4+
        # 2 ${serializeGroups(groupHierarchy)}
        # 3 $label
        # 4 $startTimestamp
        # 5 $endTimestamp
        # 6 $status
        # [7] ${serializeMessage(message)}${serializeExtraInfo(extraInfo)}

        if fields[0].strip() == "USER":
            if self.compare_versions(self.guessed_gatling_version, "3.4") < 0:
                del fields[2] # ignore obsolete $userId
            # if self.guessed_gatling_version < "3.4+":
            #     del fields[2]  # ignore obsolete $userId
            if fields[2].strip() == "START":
                self.concurrency += 1
            elif fields[2].strip() == "END":
                self.concurrency -= 1

        elif fields[0].strip() == "GROUP":
            del fields[0]
            return self.__parse_group(fields)
        elif fields[0].strip() == "REQUEST":
            del fields[0]
            return self.__parse_request(fields)
        else:
            return None

    def __parse_group(self, fields):
        latency = 0.0
        con_time = 0.0

        if len(fields) < 3:
            label = ""
            t_stamp = int(fields[1]) / 1000.0
            r_time = 0
            error = fields[0]
            r_code = "N/A"
        else:
            if self.compare_versions(self.guessed_gatling_version, "3.4") < 0:
                del fields[0]  # ignore obsolete $userId
            label = fields[0]
            if ',' in label:
                return None  # skip nested groups for now
            t_stamp = int(fields[2]) / 1000.0
            r_time = int(fields[3]) / 1000.0

            if label in self._group_errors:
                error = ';'.join(self._group_errors.pop(label))
            else:
                error = None

            if fields[4] == 'OK':
                r_code = '200'
            else:
                r_code = self.__rc_from_msg(fields[-1])

        return int(t_stamp), label, r_time, con_time, latency, r_code, error

    def __parse_request(self, fields):
        # see LogFileDataWriter.ResponseMessageSerializer in gatling-core

        if self.compare_versions(self.guessed_gatling_version,"3.4") < 0:
            del fields[0]  # ignore obsolete $userId

        if len(fields) >= 6 and fields[5]:
            error = fields[5]
        else:
            error = None

        req_hierarchy = fields[0].split(',')[0]
        if req_hierarchy:
            if error:
                self._group_errors[req_hierarchy].add(error)
            return None

        label = fields[1]
        t_stamp = int(fields[3]) / 1000.0
        r_time = (int(fields[3]) - int(fields[2])) / 1000.0
        latency = 0.0
        con_time = 0.0
        if fields[4] == 'OK':
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
        curr_version = fields[-1].strip()
        try:
            version.parse(curr_version)
        except Exception:
            return None
        if self.compare_versions(curr_version, "3.4") < 0:
            return "3.3"
        elif self.compare_versions(curr_version, "3.10") < 0:
            return "3.4"
        elif self.compare_versions(curr_version, "3.11") >= 0:
            return "3.11"
        else:
            return ""

    def compare_versions(self, v1, v2):
        parsed1, parsed2 = version.parse(v1), version.parse(v2)
        if parsed1 < parsed2:
            return -1
        elif parsed1 > parsed2:
            return 1
        else:
            return 0


    def _extract_log_data(self, fields):
        if self.guessed_gatling_version is None:
            self.guessed_gatling_version = self._guess_gatling_version(fields)

        return self._extract_log(fields) if self.guessed_gatling_version else None

    def _read(self, last_pass=False):
        """
        Generator method that returns next portion of data

        :param last_pass:
        """
        if self.binary_log:
            lines = self.file.get_lines_with_decoder(self.binary_log_decoder, last_pass=last_pass)
        else:
            lines = self.file.get_lines(size=1024 * 1024, last_pass=last_pass)

        for line in lines:
            #split here, binary decoder could return directly the extracted_log_data??
            self.log.debug("reading line: %s", line)
            if not line.endswith("\n"):
                self.partial_buffer += line
                continue

            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""

            line = line.strip()
            fields = line.split(self.delimiter)

            data = self._extract_log_data(fields)
            if data is None:
                self.log.debug("... no data processed!...")
                continue

            self.log.debug("processed line data: %s", data)
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
            # find simulation.log everywhere in case of binary log
            if not filename and self.binary_log:
                logfiles = [str(f) for f in Path(self.basedir).rglob("simulation.log")]
                if logfiles:
                    filename = logfiles[0]

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
    VERSION = "3.9.5"
    LOCAL_PATH = "~/.bzt/gatling-taurus/{version}/bin/gatling{suffix}"
    LOCAL_PATH_MVN = "~/.bzt/gatling-taurus/{version}/gatling{suffix}"
    LOCAL_PATH_MVN_POM = "~/.bzt/gatling-taurus/{version}/pom.xml"

    def __init__(self, config=None, **kwargs):
        settings = config or {}
        version = settings.get("version", self.VERSION)
        if self.is_mvn_gatling(version):
            def_path = self.LOCAL_PATH_MVN.format(version=version, suffix=EXE_SUFFIX)
            level = 1
        else:
            def_path = self.LOCAL_PATH.format(version=version, suffix=EXE_SUFFIX)
            level = 2
        gatling_path = get_full_path(settings.get("path", def_path))
        download_link = settings.get("download-link", self.DOWNLOAD_LINK).format(version=version)
        super(Gatling, self).__init__(tool_path=gatling_path, download_link=download_link, version=version, **kwargs)

        self.tool_dir = get_full_path(self.tool_path, step_up=level)

    def is_mvn_gatling(self, ver=None):
        #from version 3.11.0 gatling is available as maven plugin only
        ver_str = ver if ver is not None else self.version
        return version.parse(ver_str) >= version.parse("3.11.0")

    def check_if_installed(self):
        if self.is_mvn_gatling():
            return self.check_if_installed_mvn()
        else:
            return self.check_if_installed_old()

    def check_if_installed_mvn(self):
        self.log.debug("Trying Gatling (mvn)...")
        if os.path.exists(self.tool_dir + os.sep + "mvnw"):
            return True
        self.log.debug("File not exists: %s", self.tool_dir + os.sep + "pom.xml")
        return False

    def check_if_installed_old(self):
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
        dest = get_full_path(self.tool_path, step_up=1 if self.is_mvn_gatling() else 2)
        self.log.info("Will install %s into %s", self.tool_name, dest)
        gatling_dist = self._download(use_link=True)
        self.log.info("Unzipping %s", gatling_dist)
        unzip(gatling_dist, dest, 'gatling-charts-highcharts-bundle-' + self.version)
        os.remove(gatling_dist)
        if self.is_mvn_gatling():
            os.chmod(get_full_path(self.tool_dir + os.sep + "mvnw"), 0o755)
        else:
            os.chmod(get_full_path(self.tool_path), 0o755)
        self.log.info("Installed Gatling successfully")
        if not self.check_if_installed():
            raise ToolError("Unable to run %s after installation!" % self.tool_name)

    def build_launcher(self, new_name, scenario, cpath, log_folder, needs_scala_plugin=False):
        if self.is_mvn_gatling():
            self.build_mvn_launcher(new_name, scenario.get('simulation', None), log_folder)
            files = list(cpath)
            if scenario.data.get('script', "").endswith(".jar"):
                files.append(scenario.data.get('script', None))
            self.patch_mvn_config(files, needs_scala_plugin)
        else:
            self.build_launcher_sh(new_name)

    def patch_mvn_config(self, additional_jars, needs_scala_plugin):
        """
        Patch the Maven config file to use the correct simulation class
        """

        # 1) check if pom_bacup.xml exists -> if not create it as copy of original pom.xml
        if not os.path.exists(self.tool_dir + os.sep + "pom_backup.xml"):
            shutil.copy(self.tool_dir + os.sep + "pom.xml", self.tool_dir + os.sep + "pom_backup.xml")
        # 2) delete original pom.xml
        os.remove(self.tool_dir + os.sep + "pom.xml")
        # 3) create new pom.xml from backup with added dependency
        shutil.copy(self.tool_dir + os.sep + "pom_backup.xml", self.tool_dir + os.sep + "pom.xml")

        ET.register_namespace('', "http://maven.apache.org/POM/4.0.0")
        tree = ET.parse(self.tool_dir + os.sep + "pom_backup.xml")
        root = tree.getroot()

        ns = {'m': 'http://maven.apache.org/POM/4.0.0'}
        # ns = {'m': root.tag.split('}')[0].strip('{')} #otestovat dynamicke ziskavani namespacu

        if needs_scala_plugin:
            self._add_scala_plugin(root, ns)

        dependencies = root.find('m:dependencies', ns)
        if dependencies is None:
            dependencies = ET.SubElement(root, '{http://maven.apache.org/POM/4.0.0}dependencies')

        #put dependency for each additional jar in classpath
        for jar_path in additional_jars:
            dependency = ET.Element('{http://maven.apache.org/POM/4.0.0}dependency')
            gid = ET.SubElement(dependency, '{http://maven.apache.org/POM/4.0.0}groupId')
            gid.text = "generated.groupId"
            aid = ET.SubElement(dependency, '{http://maven.apache.org/POM/4.0.0}artifactId')
            aid.text = "generated.artifactId." + os.path.basename(jar_path).replace(".", "_")
            ver = ET.SubElement(dependency, '{http://maven.apache.org/POM/4.0.0}version')
            ver.text = "1.0"
            scope = ET.SubElement(dependency, '{http://maven.apache.org/POM/4.0.0}scope')
            scope.text = 'system'
            system_path = ET.SubElement(dependency, '{http://maven.apache.org/POM/4.0.0}systemPath')
            system_path.text = self.tool_dir + os.sep + "lib" + os.sep + os.path.basename(jar_path)
            dependencies.append(dependency)

        tree.write(self.tool_dir +os.sep + "pom.xml", encoding='utf-8', xml_declaration=True)


    def _add_scala_plugin(self, root, ns):
        build = root.find("m:build", ns)
        if build is None:
            build = ET.SubElement(root, "build")

        plugins = build.find("m:plugins", ns)
        if plugins is None:
            plugins = ET.SubElement(build, "plugins")

        exists = any(
            (p.find("m:artifactId", ns) is not None and p.find("m:artifactId", ns).text == "scala-maven-plugin")
            for p in plugins.findall("m:plugin", ns)
        )

        if not exists:
            plugin = ET.SubElement(plugins, "plugin")

            gid = ET.SubElement(plugin, "groupId")
            gid.text = "net.alchim31.maven"

            aid = ET.SubElement(plugin, "artifactId")
            aid.text = "scala-maven-plugin"

            ver = ET.SubElement(plugin, "version")
            ver.text = "4.8.1"

            executions = ET.SubElement(plugin, 'executions')
            execution = ET.SubElement(executions, 'execution')

            goals = ET.SubElement(execution, 'goals')
            goal = ET.SubElement(goals, 'goal')
            goal.text = "testCompile"

            configuration = ET.SubElement(execution, 'configuration')

            jvm_args = ET.SubElement(configuration, 'jvmArgs')
            jvm_arg = ET.SubElement(jvm_args, 'jvmArg')
            jvm_arg.text = "-Xss100M"

            args = ET.SubElement(configuration, 'args')
            for arg_value in ["-deprecation", "-feature", "-unchecked", "-language:implicitConversions", "-language:postfixOps"]:
                arg = ET.SubElement(args, 'arg')
                arg.text = arg_value


    def build_mvn_launcher(self, new_name, simulation_class, log_folder):
        self.tool_path = new_name

        if is_windows():
            # Windows .bat
            run_line = (
                f"call mvnw.cmd gatling:test "
                f"-Dgatling.simulationClass={simulation_class} "
                f"-Dgatling.resultsFolder={log_folder} %JAVA_OPTS%"
            )
            content = (
                "@echo off\n"
                f"cd {self.tool_dir}\n"
                f"{run_line}\n"
            )
        else:
            # Unix .sh
            run_line = (
                f"eval ./mvnw gatling:test "
                f"-Dgatling.simulationClass={simulation_class} "
                f"-Dgatling.resultsFolder={log_folder} $JAVA_OPTS"
            )
            content = (
                "#!/bin/bash\n"
                f"cd {self.tool_dir}\n"
                f"{run_line}\n"
            )

            with open(self.tool_path, 'w') as modified:
                modified.write(content)

            if not is_windows():
                os.chmod(self.tool_path, 0o755)


    def build_launcher_sh(self, new_name):  # legacy, for v2 only
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
                    elif line.startswith('set CLASSPATH='):
                        mod_success = True
                        line = line.rstrip()[:-1] + '${JAVA_CLASSPATH}"\n'  # add from env
                    elif line.startswith('JAVA_OPTS'):
                        mod_success = True
                        line = re.sub(r'\$\{.*?\}', '', line)[:-2] + ' ${JAVA_OPTS}"\n'
                    elif line.startswith('%JAVA%'):
                        line = line.rstrip() + ' -rm local -rd Taurus\n'  # add mandatory parameters
                else:
                    if line.startswith('COMPILER_CLASSPATH='):
                        mod_success = True
                        line = line.rstrip()[:-1] + '${COMPILATION_CLASSPATH}"\n'  # add from env
                    elif line.startswith('GATLING_CLASSPATH='):
                        mod_success = True
                        line = line.rstrip()[:-1] + '${JAVA_CLASSPATH}"\n'  # add from env
                    elif line.startswith('CLASSPATH='):
                        mod_success = True
                        line = line.rstrip()[:-1] + ':${JAVA_CLASSPATH}"\n'  # add from env
                    elif line.startswith('JAVA_OPTS'):
                        mod_success = True
                        line = re.sub(r'\$\{.*?\}', '', line)[:-2] + ' ${JAVA_OPTS}"\n'
                    elif line.startswith('"$JAVA"'):
                        line = line.rstrip() + ' -rm local -rd Taurus \n'  # add mandatory parameters
                        line = 'eval ' + line
                modified_lines.append(line)

        if not mod_success:
            raise ToolError("Can't modify gatling launcher for jar usage, ability isn't supported")

        self.tool_path = new_name

        with open(self.tool_path, 'w') as modified:
            modified.writelines(modified_lines)

        if not is_windows():
            os.chmod(self.tool_path, 0o755)


class BinaryLogReader(object):


    def __init__(self):
        self.scenarios = []
        self.test = True
        self.string_cache = {}
        self.run_start_time = None

    def read_int(self, file):
        return struct.unpack('>i', file.read(4))[0]

    def read_long(self, file):
        return struct.unpack('>q', file.read(8))[0]

    def read_boolean(self, file):
        return struct.unpack(">?", file.read(1))[0]

    def read_string(self, file):
        length = self.read_int(file)
        if length == 0:
            return ""
        data = file.read(length)
        coder = struct.unpack("B", file.read(1))[0]

        if coder == 0:
            # Java "LATIN1" / Gatling use UTF-8 safe
            return data.decode("latin-1")
        elif coder == 1:
            # Java "UTF16"
            return data.decode("utf-16-be")
        else:
            raise ValueError(f"Unknown coder value: {coder}")

    def read_cached_string(self, file):
        idx = self.read_int(file)
        if idx >= 0:
            s = self.read_string(file)
            self.string_cache[idx] = s
            return s
        else:
            return self.string_cache[-idx]

    def read_byte_buffer(self, file):
        length = self.read_int(file)
        return file.read(length)  # not parsing content (it's Pickle)

    def read_groups(self, file):
        count = self.read_int(file)
        return [self.read_cached_string(file) for _ in range(count)]

    @staticmethod
    def with_bytes_read(parse_func):
        @wraps(parse_func)
        def wrapper(self, f, *args, **kwargs):
            start = f.tell()
            obj = parse_func(self, f, *args, **kwargs)
            end = f.tell()
            return obj, (end - start + 1)  # +1 for the record type byte
        return wrapper


    @with_bytes_read
    def parse_run_message(self, f):
        run = {"gatlingVersion": self.read_string(f),
               "simulationClassName": self.read_string(f),
               "start": self.read_long(f),
               "runDescription": self.read_string(f)}
        self.run_start_time = run["start"]

        # scenario
        scenario_count = self.read_int(f)
        scenarios = []
        for _ in range(scenario_count):
            scenarios.append(self.read_string(f))
        run["scenarios"] = scenarios
        self.scenarios = scenarios

        # assertions
        assertion_count = self.read_int(f)
        assertions = []
        for _ in range(assertion_count):
            assertions.append(self.read_byte_buffer(f))
        run["assertions_raw"] = assertions
        return run

    @with_bytes_read
    def parse_user_message(self, f):

        scenario = self.scenarios[self.read_int(f)]
        start = self.read_boolean(f)  # start
        timestamp = self.read_int(f) + self.run_start_time  #offset + run_start

        result = {
            'scenario': scenario,
            'start': start,  # boolean
            'timestamp': timestamp
        }

        return result

    @with_bytes_read
    def parse_request_message(self, f) -> dict[str, Any]:
        group_hierarchy = self.read_groups(f)
        name = self.read_cached_string(f)
        start_ts = self.read_int(f) + self.run_start_time
        end_ts = self.read_int(f) + self.run_start_time
        status = self.read_boolean(f)
        message = self.read_cached_string(f)

        return {
            "type": "Response",
            "groupHierarchy": group_hierarchy,
            "name": name,
            "start": start_ts,
            "end": end_ts,
            "status": "OK" if status else "KO",
            "message": message,
        }

    @with_bytes_read
    def parse_group_message(self, f):
        group_hierarchy = self.read_groups(f)
        start_ts = self.read_int(f) + self.run_start_time
        end_ts = self.read_int(f) + self.run_start_time
        cumulated_response_time = self.read_int(f)
        status = self.read_boolean(f)

        return {
            "type": "Group",
            "groupHierarchy": group_hierarchy,
            "start": start_ts,
            "end": end_ts,
            "cumulatedResponseTime": cumulated_response_time,
            "status": "OK" if status else "KO",
            "duration": end_ts - start_ts,
        }

    @with_bytes_read
    def parse_error_message(self, f):
        message = self.read_cached_string(f)
        timestamp = self.read_int(f) + self.run_start_time

        return {
            # update based on gatling sources
            "message": message,
            "timestamp": timestamp
        }

    def read_log_object(self, file, buffer):
        """
        Read binary log file
        :param file: file object
        :param buffer: int, max number of records to read
        :return: string, int
        """
        counter = 0
        while True:
            if 0 < buffer < (counter := counter + 1):
                return None
            header = file.read(1)
            if not header:
                return None # EOF
            record_type = header[0]
            if record_type == 0:
                data, size = self.parse_run_message(file)
                yield f"Run\t {data['simulationClassName']} \t {data['runDescription']} \t\t {data['gatlingVersion']} \n", size
            elif record_type == 1:
                data, size = self.parse_request_message(file)
                gh = ",".join(data["groupHierarchy"])
                yield f"REQUEST\t{gh}\t{data['name']}\t{data['start']}\t{data['end']}\t{data['status']}\t{data['message']}\n", size
            elif record_type == 2:
                data, size = self.parse_user_message(file)
                status = "START" if data['start'] else "END"
                yield f"USER\t {data['scenario']} \t {status} \t {data['timestamp']} \t {data['timestamp']} \n", size
            elif record_type == 3:
                data, size =  self.parse_group_message(file)
                yield f"GROUP\t{','.join(data['groupHierarchy'])}\t{data['start']}\t{data['end']}\t{data['cumulatedResponseTime']}\t{data['status']}\n", size
            elif record_type == 4:
                data, size = self.parse_error_message(file)
                yield f"Error\t{data['message']}\t{data['timestamp']}\n", size
            else:
                raise ValueError(f"Unknown record header: {record_type}")


