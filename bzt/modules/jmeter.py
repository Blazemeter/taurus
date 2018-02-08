"""
Module holds all stuff regarding JMeter tool usage

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
import copy
import csv
import fnmatch
import os
import re
import socket
import subprocess
import tempfile
import time
import traceback
from collections import Counter, namedtuple
from distutils.version import LooseVersion
from itertools import dropwhile

from cssselect import GenericTranslator

from bzt import TaurusConfigError, ToolError, TaurusInternalException, TaurusNetworkError
from bzt.engine import ScenarioExecutor, Scenario, FileLister, HavingInstallableTools
from bzt.engine import SelfDiagnosable, Provisioning, SETTINGS
from bzt.jmx import JMX, JMeterScenarioBuilder, LoadSettingsProcessor
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader, DataPoint, KPISet
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.modules.functional import FunctionalAggregator, FunctionalResultsReader, FunctionalSample
from bzt.modules.provisioning import Local
from bzt.modules.soapui import SoapUIScriptConverter
from bzt.requests_model import ResourceFilesCollector, has_variable_pattern
from bzt.six import communicate, PY2
from bzt.six import iteritems, string_types, StringIO, etree, unicode_decode, numeric_types
from bzt.utils import get_full_path, EXE_SUFFIX, MirrorsManager, ExceptionalDownloader, get_uniq_name
from bzt.utils import shell_exec, BetterDict, guess_csv_dialect, ensure_is_dict, dehumanize_time, FileReader
from bzt.utils import unzip, RequiredTool, JavaVM, shutdown_process, ProgressBarContext, TclLibrary


class JMeterExecutor(ScenarioExecutor, WidgetProvider, FileLister, HavingInstallableTools, SelfDiagnosable):
    """
    JMeter executor module

    :type modified_jmx: str
    :type jmeter_log: str
    :type properties_file: str
    :type sys_properties_file: str
    """
    MIRRORS_SOURCE = "https://jmeter.apache.org/download_jmeter.cgi"
    JMETER_DOWNLOAD_LINK = "https://archive.apache.org/dist/jmeter/binaries/apache-jmeter-{version}.zip"
    PLUGINS_MANAGER_VERSION = "0.18"
    PLUGINS_MANAGER = 'https://search.maven.org/remotecontent?filepath=kg/apc/jmeter-plugins-manager/' \
                      '{ver}/jmeter-plugins-manager-{ver}.jar'.format(ver=PLUGINS_MANAGER_VERSION)
    CMDRUNNER = 'https://search.maven.org/remotecontent?filepath=kg/apc/cmdrunner/2.0/cmdrunner-2.0.jar'
    JMETER_VER = "3.3"
    UDP_PORT_NUMBER = None

    def __init__(self):
        super(JMeterExecutor, self).__init__()
        self.original_jmx = None
        self.modified_jmx = None
        self.jmeter_log = None
        self.properties_file = None
        self.sys_properties_file = None
        self.kpi_jtl = None
        self.log_jtl = None
        self.process = None
        self.end_time = None
        self.retcode = None
        self.distributed_servers = []
        self.management_port = None
        self.resource_files_collector = None
        self.stdout_file = None
        self.stderr_file = None
        self.tool = None

    def get_load(self):
        """
        Helper method to read load specification
        """
        load = self.get_specific_load()

        throughput = load.throughput
        concurrency = load.concurrency
        iterations = load.iterations
        steps = load.steps
        hold = load.hold
        ramp_up = load.ramp_up

        hold = self._try_convert(hold, dehumanize_time, 0)
        duration = hold

        if ramp_up is not None:
            ramp_up = self._try_convert(ramp_up, dehumanize_time, 0)
            duration += ramp_up

        msg = ''
        if not isinstance(concurrency, numeric_types + (type(None),)):
            msg += "\nNon-integer concurrency value [%s]: %s " % (type(concurrency).__name__, concurrency)
        if not isinstance(throughput, numeric_types + (type(None),)):
            msg += "\nNon-integer throughput value [%s]: %s " % (type(throughput).__name__, throughput)
        if not isinstance(steps, numeric_types + (type(None),)):
            msg += "\nNon-integer steps value [%s]: %s " % (type(steps).__name__, steps)
        if not isinstance(iterations, numeric_types + (type(None),)):
            msg += "\nNon-integer iterations value [%s]: %s " % (type(iterations).__name__, iterations)

        if msg:
            self.log.warning(msg)

        throughput = self._try_convert(throughput, float, 0)
        concurrency = self._try_convert(concurrency, int, 0)
        iterations = self._try_convert(iterations, int, 0)
        steps = self._try_convert(steps, int, 0)

        if duration and not iterations:
            iterations = 0  # which means infinite

        return self.LOAD_FMT(concurrency=concurrency, ramp_up=ramp_up, throughput=throughput, hold=hold,
                             iterations=iterations, duration=duration, steps=steps)

    @staticmethod
    def _get_prop_default(val):
        comma_ind = val.find(",")
        comma_found = comma_ind > -1
        is_expression = val.startswith("${") and val.endswith("}")
        is_property = val.startswith("${__property(") or val.startswith("${__P(")
        if is_expression and is_property and comma_found:
            return val[comma_ind + 1: -2]
        else:
            return None

    @staticmethod
    def _try_convert(val, func, default=None):
        if val is None:
            res = val
        elif isinstance(val, string_types) and val.startswith('$'):  # it's property...
            if default is not None:
                val = JMeterExecutor._get_prop_default(val) or default
                res = func(val)
            else:
                res = val
        else:
            res = func(val)

        return res

    def get_specific_load(self):
        """
        Helper method to read load specification
        """
        prov_type = self.engine.config.get(Provisioning.PROV)

        ensure_is_dict(self.execution, ScenarioExecutor.THRPT, prov_type)
        throughput = self.execution[ScenarioExecutor.THRPT].get(prov_type, 0)

        ensure_is_dict(self.execution, ScenarioExecutor.CONCURR, prov_type)
        concurrency = self.execution[ScenarioExecutor.CONCURR].get(prov_type, 0)

        iterations = self.execution.get("iterations", None)

        steps = self.execution.get(ScenarioExecutor.STEPS, None)

        hold = self.execution.get(ScenarioExecutor.HOLD_FOR, 0)
        hold = self._try_convert(hold, dehumanize_time)

        ramp_up = self.execution.get(ScenarioExecutor.RAMP_UP, None)
        ramp_up = self._try_convert(ramp_up, dehumanize_time)

        if not hold:
            duration = ramp_up
        elif not ramp_up:
            duration = hold
        elif isinstance(ramp_up, numeric_types) and isinstance(hold, numeric_types):
            duration = hold + ramp_up
        else:
            duration = 1  # dehumanize_time(<sum_of_props>) can be unpredictable so we use default there

        throughput = self._try_convert(throughput, float)
        concurrency = self._try_convert(concurrency, int)
        iterations = self._try_convert(iterations, int)
        steps = self._try_convert(steps, int)

        if duration and not iterations:
            iterations = 0  # which means infinite

        return self.LOAD_FMT(concurrency=concurrency, ramp_up=ramp_up, throughput=throughput, hold=hold,
                             iterations=iterations, duration=duration, steps=steps)

    def get_scenario(self, name=None, cache_scenario=True):
        scenario_obj = super(JMeterExecutor, self).get_scenario(name=name, cache_scenario=False)

        if not isinstance(self.engine.provisioning, Local):
            return scenario_obj

        if Scenario.SCRIPT in scenario_obj and scenario_obj[Scenario.SCRIPT] is not None:
            script_path = self.engine.find_file(scenario_obj[Scenario.SCRIPT])
            with open(script_path) as fds:
                script_content = fds.read()
            if "con:soapui-project" in script_content:
                self.log.info("SoapUI project detected")
                scenario_name, merged_scenario = self._extract_scenario_from_soapui(scenario_obj, script_path)
                self.engine.config["scenarios"].merge({scenario_name: merged_scenario})
                self.execution[Scenario.SCRIPT] = scenario_name
                return super(JMeterExecutor, self).get_scenario(name=scenario_name)

        return scenario_obj

    def _extract_scenario_from_soapui(self, base_scenario, script_path):
        test_case = base_scenario.get("test-case", None)
        converter = SoapUIScriptConverter(self.log)
        conv_config = converter.convert_script(script_path)
        conv_scenarios = conv_config["scenarios"]
        scenario_name, conv_scenario = converter.find_soapui_test_case(test_case, conv_scenarios)

        new_name = scenario_name
        counter = 1
        while new_name in self.engine.config["scenarios"]:
            new_name = scenario_name + ("-%s" % counter)
            counter += 1

        if new_name != scenario_name:
            self.log.info("Scenario name '%s' is already taken, renaming to '%s'", scenario_name, new_name)
            scenario_name = new_name

        merged_scenario = BetterDict()
        merged_scenario.merge(conv_scenario)
        merged_scenario.merge(base_scenario.data)
        for field in [Scenario.SCRIPT, "test-case"]:
            if field in merged_scenario:
                merged_scenario.pop(field)

        return scenario_name, merged_scenario

    @staticmethod
    def _get_tool_version(jmx_file):
        jmx = JMX(jmx_file)
        selector = 'jmeterTestPlan'
        test_plan = jmx.get(selector)[0]
        ver = test_plan.get('jmeter')
        if isinstance(ver, string_types):
            index = ver.find(" ")
            if index != -1:
                return ver[:index]

        return JMeterExecutor.JMETER_VER

    def prepare(self):
        """
        Preparation for JMeter involves either getting existing JMX
        and modifying it, or generating new JMX from input data. Then,
        original JMX is modified to contain JTL writing classes with
        required settings and have workload as suggested by Provisioning

        :raise TaurusConfigError:
        """
        scenario = self.get_scenario()

        self.jmeter_log = self.engine.create_artifact("jmeter", ".log")
        self._set_remote_port()
        self.distributed_servers = self.execution.get('distributed', self.distributed_servers)

        is_jmx_generated = False

        self.original_jmx = self.get_script_path()
        if self.settings.get("version", self.JMETER_VER) == "auto":
            self.settings["version"] = self._get_tool_version(self.original_jmx)
        self.install_required_tools()

        if not self.original_jmx:
            if scenario.get("requests"):
                self.original_jmx = self.__jmx_from_requests()
                is_jmx_generated = True
            else:
                raise TaurusConfigError("You must specify either a JMX file or list of requests to run JMeter")

        if self.engine.aggregator.is_functional:
            flags = {"connectTime": True}
            version = LooseVersion(str(self.settings.get("version", self.JMETER_VER)))
            major = version.version[0]
            if major == 2:
                flags["bytes"] = True
            else:
                flags["sentBytes"] = True
            self.settings.merge({"xml-jtl-flags": flags})

        modified = self.__get_modified_jmx(self.original_jmx, is_jmx_generated)
        self.modified_jmx = self.__save_modified_jmx(modified, self.original_jmx, is_jmx_generated)

        self.__set_jmeter_properties(scenario)
        self.__set_system_properties()
        self.__set_jvm_properties()

        # check for necessary plugins and install them if needed
        if self.settings.get("detect-plugins", True):
            self.tool.install_for_jmx(self.modified_jmx)

        out = self.engine.create_artifact("jmeter", ".out")
        err = self.engine.create_artifact("jmeter", ".err")
        self.stdout_file = open(out, "w")
        self.stderr_file = open(err, "w")

        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.reader = JTLReader(self.kpi_jtl, self.log, self.log_jtl)
            self.reader.is_distributed = len(self.distributed_servers) > 0
            assert isinstance(self.reader, JTLReader)
            self.engine.aggregator.add_underling(self.reader)
        elif isinstance(self.engine.aggregator, FunctionalAggregator):
            self.reader = FuncJTLReader(self.log_jtl, self.engine, self.log)
            self.reader.is_distributed = len(self.distributed_servers) > 0
            self.reader.executor_label = self.label
            self.engine.aggregator.add_underling(self.reader)

    def __set_system_properties(self):
        sys_props = self.settings.get("system-properties")
        if sys_props:
            self.log.debug("Additional system properties %s", sys_props)
            sys_props_file = self.engine.create_artifact("system", ".properties")
            JMeterExecutor.__write_props_to_file(sys_props_file, sys_props)
            self.sys_properties_file = sys_props_file

    def __set_jvm_properties(self):
        heap_size = self.settings.get("memory-xmx", None)
        if heap_size is not None:
            self.log.debug("Setting JVM heap size to %s", heap_size)
            self.env.add_java_param({"JVM_ARGS": "-Xmx%s" % heap_size})

    def __set_jmeter_properties(self, scenario):
        props = copy.deepcopy(self.settings.get("properties"))
        props_local = copy.deepcopy(scenario.get("properties"))
        if self.distributed_servers and self.settings.get("gui", False):
            props_local.merge({"remote_hosts": ",".join(self.distributed_servers)})
        props_local.update({"jmeterengine.nongui.port": self.management_port})
        props_local.update({"jmeterengine.nongui.maxport": self.management_port})
        props_local.update({"jmeter.save.saveservice.timestamp_format": "ms"})
        props_local.update({"sampleresult.default.encoding": "UTF-8"})
        props.merge(props_local)

        user_cp = [self.engine.artifacts_dir]
        user_cp.append(get_full_path(self.original_jmx, step_up=1))

        for _file in self.execution.get('files', []):
            full_path = get_full_path(_file)
            if os.path.isdir(full_path):
                user_cp.append(full_path)
            elif full_path.lower().endswith('.jar'):
                user_cp.append((get_full_path(_file, step_up=1)))

        if 'user.classpath' in props:
            user_cp.append(props['user.classpath'])

        props['user.classpath'] = os.pathsep.join(user_cp).replace(os.path.sep, "/")  # replace to avoid Windows issue

        if props:
            self.log.debug("Additional properties: %s", props)
            props_file = self.engine.create_artifact("jmeter-bzt", ".properties")
            JMeterExecutor.__write_props_to_file(props_file, props)
            self.properties_file = props_file

    def startup(self):
        """
        Should start JMeter as fast as possible.
        """
        cmdline = [self.settings.get("path")]  # default is set when prepared
        if not self.settings.get("gui", False):
            cmdline += ["-n"]
        cmdline += ["-t", os.path.abspath(self.modified_jmx)]
        if self.jmeter_log:
            cmdline += ["-j", os.path.abspath(self.jmeter_log)]

        if self.properties_file:
            cmdline += ["-q", os.path.abspath(self.properties_file)]
            if self.distributed_servers:
                cmdline += ["-G", os.path.abspath(self.properties_file)]

        if self.sys_properties_file:
            cmdline += ["-S", os.path.abspath(self.sys_properties_file)]
        if self.distributed_servers and not self.settings.get("gui", False):
            cmdline += ['-R%s' % ','.join(self.distributed_servers)]

        self.start_time = time.time()
        try:
            self.process = self.execute(cmdline, stdout=self.stdout_file, stderr=self.stderr_file)
        except KeyboardInterrupt:
            raise
        except BaseException as exc:
            raise ToolError("%s\nFailed to start JMeter: %s" % (cmdline, exc))

    def check(self):
        """
        Checks if JMeter is still running. Also checks if resulting JTL contains
        any data and throws exception otherwise.

        :return: bool
        :raise ToolError:
        """
        self.retcode = self.process.poll()
        if self.retcode is not None:
            if self.retcode != 0:
                raise ToolError("JMeter exited with non-zero code: %s" % self.retcode, self.get_error_diagnostics())

            return True
        return False

    def shutdown(self):
        """
        If JMeter is still running - let's stop it.
        """
        max_attempts = self.settings.get("shutdown-wait", 5)
        if self._process_stopped(1):
            return

        try:
            if not self.settings.get("gui", False):
                udp_sock = socket.socket(type=socket.SOCK_DGRAM)

                self.log.info("Sending Shutdown command to JMeter on port %d...", self.management_port)
                udp_sock.sendto(b"Shutdown", ("localhost", self.management_port))
                if self._process_stopped(max_attempts):
                    self.log.debug("JMeter stopped on Shutdown command")
                    return

                self.log.info("Sending StopTestNow command to JMeter on port %d...", self.management_port)
                udp_sock.sendto(b"StopTestNow", ("localhost", self.management_port))
                if self._process_stopped(max_attempts):
                    self.log.debug("JMeter stopped on StopTestNow command")
                    return
        finally:
            if not self._process_stopped(1):
                self.log.warning("JMeter process is still alive, killing it")
                shutdown_process(self.process, self.log)

        if self.start_time:
            self.end_time = time.time()
            self.log.debug("JMeter worked for %s seconds", self.end_time - self.start_time)

    def post_process(self):
        self.engine.existing_artifact(self.modified_jmx, True)
        if self.stdout_file:
            self.stdout_file.close()
        if self.stderr_file:
            self.stderr_file.close()

    def has_results(self):
        if self.reader and self.reader.read_records:
            return True
        else:
            return False

    def _process_stopped(self, cycles):
        while cycles > 0:
            cycles -= 1
            if self.process and self.process.poll() is None:
                time.sleep(self.engine.check_interval)
            else:
                return True
        return False

    def _set_remote_port(self):
        """
        set management udp port
        :return:
        """

        if not JMeterExecutor.UDP_PORT_NUMBER:
            JMeterExecutor.UDP_PORT_NUMBER = self.settings.get("shutdown-port", 4445)
        else:
            JMeterExecutor.UDP_PORT_NUMBER += 1

        while not self.__port_is_free(JMeterExecutor.UDP_PORT_NUMBER):
            self.log.debug("Port %d is busy, trying next one", JMeterExecutor.UDP_PORT_NUMBER)
            if JMeterExecutor.UDP_PORT_NUMBER == 65535:
                TaurusInternalException("JMeter: no free ports for management interface")
            else:
                JMeterExecutor.UDP_PORT_NUMBER += 1

        self.management_port = JMeterExecutor.UDP_PORT_NUMBER
        self.log.debug("Using port %d for management", self.management_port)

    def __port_is_free(self, port_num):
        """
        :return: Bool
        """
        udp_sock = socket.socket(type=socket.SOCK_DGRAM)
        try:
            self.log.debug("Checking if port %d is free", port_num)
            udp_sock.bind(("localhost", port_num))
            udp_sock.close()
            self.log.debug("Port %d is free", port_num)
            return True
        except socket.error:
            self.log.debug("Port %d is busy", port_num)
            return False

    @staticmethod
    def __disable_listeners(jmx):
        """
        Set ResultCollector to disabled
        :param jmx: JMX
        :return:
        """
        sel = 'stringProp[name=filename]'
        xpath = GenericTranslator().css_to_xpath(sel)

        listeners = jmx.get('ResultCollector')
        for listener in listeners:
            file_setting = listener.xpath(xpath)
            if not file_setting or not file_setting[0].text:
                listener.set("enabled", "false")

    def __apply_test_mode(self, jmx):
        func_mode = self.engine.is_functional_mode()
        test_plan_selector = "jmeterTestPlan>hashTree>TestPlan"
        plans = jmx.get(test_plan_selector)
        if not plans:
            self.log.warning("No test plans, can't set test mode")
            return
        test_plan = plans[0]
        props = test_plan.xpath('boolProp[@name="TestPlan.functional_mode"]')
        if props:
            prop = props[0]
            prop.text = "true" if func_mode else "false"
        else:
            element = jmx._get_functional_mode_prop(func_mode)
            jmx.append(test_plan_selector, element)

    @staticmethod
    def __fill_empty_delimiters(jmx):
        delimiters = jmx.get("CSVDataSet>stringProp[name='delimiter']")
        for delimiter in delimiters:
            if not delimiter.text:
                delimiter.text = ','

    @staticmethod
    def __add_listener(lst, jmx):
        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, lst)
        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree.Element("hashTree"))

    def __add_result_listeners(self, jmx):
        if self.engine.is_functional_mode():
            self.__add_trace_writer(jmx)
        else:
            self.__add_result_writers(jmx)

    def __add_trace_writer(self, jmx):
        self.log_jtl = self.engine.create_artifact("trace", ".jtl")
        flags = self.settings.get('xml-jtl-flags')
        log_lst = jmx.new_xml_listener(self.log_jtl, True, flags)
        self.__add_listener(log_lst, jmx)

    def __add_result_writers(self, jmx):
        self.kpi_jtl = self.engine.create_artifact("kpi", ".jtl")
        kpi_lst = jmx.new_kpi_listener(self.kpi_jtl)
        self.__add_listener(kpi_lst, jmx)

        verbose = self.engine.config.get(SETTINGS).get("verbose", False)
        jtl_log_level = self.execution.get('write-xml-jtl', "full" if verbose else 'error')

        flags = self.settings.get('xml-jtl-flags')

        if jtl_log_level == 'error':
            self.log_jtl = self.engine.create_artifact("error", ".jtl")
            log_lst = jmx.new_xml_listener(self.log_jtl, False, flags)
            self.__add_listener(log_lst, jmx)
        elif jtl_log_level == 'full':
            self.log_jtl = self.engine.create_artifact("trace", ".jtl")
            log_lst = jmx.new_xml_listener(self.log_jtl, True, flags)
            self.__add_listener(log_lst, jmx)

    def __force_tran_parent_sample(self, jmx):
        scenario = self.get_scenario()
        if scenario.get("force-parent-sample", True):
            self.log.debug("Enforcing parent sample for transaction controller")
            jmx.set_text('TransactionController > boolProp[name="TransactionController.parent"]', 'true')

    def __get_modified_jmx(self, original, is_jmx_generated):
        """
        add two listeners to test plan:
            - to collect basic stats for KPIs
            - to collect detailed errors/trace info
        :return: path to artifact
        """
        self.log.debug("Load: %s", self.get_specific_load())
        jmx = JMX(original)

        if self.get_scenario().get("disable-listeners", not self.settings.get("gui", False)):
            JMeterExecutor.__disable_listeners(jmx)

        user_def_vars = self.get_scenario().get("variables")
        if user_def_vars:
            jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, jmx.add_user_def_vars_elements(user_def_vars))
            jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree.Element("hashTree"))

        headers = self.get_scenario().get_headers()
        if headers:
            jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, JMX._get_header_mgr(headers))
            jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree.Element("hashTree"))

        self.__apply_test_mode(jmx)
        self.__add_result_listeners(jmx)
        if not is_jmx_generated:
            self.__force_tran_parent_sample(jmx)
            version = LooseVersion(str(self.settings.get('version', self.JMETER_VER)))
            if version >= LooseVersion("3.2"):
                self.__force_hc4_cookie_handler(jmx)
        self.__fill_empty_delimiters(jmx)

        self.__apply_modifications(jmx)
        LoadSettingsProcessor(self).modify(jmx)

        return jmx

    def __force_hc4_cookie_handler(self, jmx):
        selector = "[testclass=CookieManager]"
        fix_counter = 0
        for node in jmx.get(selector):
            name = "CookieManager.implementation"
            if not node.get(name):
                val = "org.apache.jmeter.protocol.http.control.HC4CookieHandler"
                node.append(JMX._string_prop(name, val))
                fix_counter += 1
        if fix_counter:
            self.log.info('%s obsolete CookieManagers are found and fixed' % fix_counter)

    def __save_modified_jmx(self, jmx, original_jmx_path, is_jmx_generated):
        script_name, _ = os.path.splitext(os.path.basename(original_jmx_path))
        modified_script_name = "modified_" + script_name
        if is_jmx_generated:
            filename = self.engine.create_artifact(modified_script_name, ".jmx")
        else:
            script_dir = get_full_path(original_jmx_path, step_up=1)
            filename = get_uniq_name(script_dir, modified_script_name, ".jmx")
        jmx.save(filename)
        return filename

    def __jmx_from_requests(self):
        """
        Generate jmx file from requests
        :return:
        """
        filename = self.engine.create_artifact("requests", ".jmx")
        jmx = JMeterScenarioBuilder(self)
        jmx.save(filename)
        self.settings.merge(jmx.system_props)
        return filename

    @staticmethod
    def __write_props_to_file(file_path, params):
        """
        Write properties to file
        :param file_path:
        :param params:
        :return:
        """
        with open(file_path, 'w') as fds:
            for key, val in iteritems(params):
                fds.write("%s=%s\n" % (key, val))

    def get_widget(self):
        """
        Add progress widget to console screen sidebar

        :return:
        """
        if not self.widget:
            label = "%s" % self
            self.widget = ExecutorWidget(self, "JMeter: " + label.split('/')[1])
        return self.widget

    def __modify_resources_paths_in_jmx(self, jmx, file_list):
        """
        Modify resource files paths in jmx etree

        :param jmx: JMX
        :param file_list: list
        :return:
        """
        file_set = set(file_list)
        missed_files = []
        while file_set:
            filename = file_set.pop()
            file_path_elements = jmx.xpath('//stringProp[text()="%s"]' % filename)
            if not file_path_elements:
                missed_files.append(filename)
            for file_path_element in file_path_elements:
                basename = os.path.basename(filename)
                self.log.debug("Replacing JMX path %s with %s", file_path_element.text, basename)
                file_path_element.text = basename

        if missed_files:
            self.log.warning("Files not found in JMX: %s", missed_files)

    def _resolve_jmx_relpaths(self, resource_files_from_jmx):
        """
        Attempt to paths relative to JMX script itself.

        :param resource_files_from_jmx:
        :return:
        """
        resource_files = []
        script_basedir = os.path.dirname(get_full_path(self.original_jmx))
        for res_file in resource_files_from_jmx:
            if not os.path.exists(res_file):
                path_relative_to_jmx = os.path.join(script_basedir, res_file)
                if os.path.exists(path_relative_to_jmx):
                    self.log.info("Resolved resource file with path relative to JMX: %s", path_relative_to_jmx)
                    resource_files.append(path_relative_to_jmx)
                    continue
            resource_files.append(res_file)
        return resource_files

    def resource_files(self):
        """
        Get list of resource files, modify jmx file paths if necessary
        """
        # get all resource files from requests
        scenario = self.get_scenario()
        resource_files = self.res_files_from_scenario(scenario)

        self.original_jmx = self.get_script_path()
        if self.original_jmx:
            jmx = JMX(self.original_jmx)
            resource_files_from_jmx = JMeterExecutor.__get_resource_files_from_jmx(jmx)
            if resource_files_from_jmx:
                execution_files = self.execution.get('files', [])
                execution_files.extend(self._resolve_jmx_relpaths(resource_files_from_jmx))
                self.__modify_resources_paths_in_jmx(jmx.tree, resource_files_from_jmx)
                script_name, script_ext = os.path.splitext(os.path.basename(self.original_jmx))
                self.original_jmx = self.engine.create_artifact(script_name, script_ext)
                jmx.save(self.original_jmx)
                scenario[Scenario.SCRIPT] = self.original_jmx

        script = self.get_scenario().get(Scenario.SCRIPT, None)
        if script:
            resource_files.append(script)

        return resource_files

    @staticmethod
    def __get_resource_files_from_jmx(jmx):
        """
        Get list of resource files paths from jmx scenario
        :return: (file list)
        """
        resource_files = []
        exclude_elements = ['kg.apc.jmeter.jmxmon.JMXMonCollector', 'JSR223Listener',
                            'kg.apc.jmeter.vizualizers.CorrectedResultCollector',
                            'kg.apc.jmeter.reporters.FlexibleFileWriter', 'BSFListener',
                            'kg.apc.jmeter.dbmon.DbMonCollector', 'BeanShellListener', 'MailerResultCollector',
                            'kg.apc.jmeter.perfmon.PerfMonCollector', 'ResultCollector',
                            'kg.apc.jmeter.vizualizers.CompositeResultCollector',
                            'kg.apc.jmeter.reporters.LoadosophiaUploader']
        search_patterns = ["File.path", "filename", "BeanShellSampler.filename"]
        for pattern in search_patterns:
            resource_elements = jmx.tree.findall(".//stringProp[@name='%s']" % pattern)
            for resource_element in resource_elements:
                # check if none of parents are disabled
                parent = resource_element.getparent()
                parent_disabled = False
                while parent is not None:  # ?
                    if parent.get('enabled') == 'false' or parent.tag in exclude_elements:
                        parent_disabled = True
                        break
                    parent = parent.getparent()

                if resource_element.text and not parent_disabled and not has_variable_pattern(resource_element.text):
                    resource_files.append(resource_element.text)
        return resource_files

    def res_files_from_scenario(self, scenario):
        files = []
        data_sources = scenario.data.get('data-sources')
        if data_sources:
            for data_source in data_sources:
                if isinstance(data_source, string_types):
                    files.append(data_source)
                elif isinstance(data_source, dict):
                    files.append(data_source['path'])
        requests = scenario.get_requests()
        for req in requests:
            files.extend(self.res_files_from_request(req))
            self.resource_files_collector.clear_path_cache()
        return files

    def res_files_from_request(self, request):
        if self.resource_files_collector is None:
            self.resource_files_collector = ResourceFilesCollector(self)
        return self.resource_files_collector.visit(request)

    def __apply_modifications(self, jmx):
        """
        :type jmx: JMX
        """
        modifs = self.get_scenario().get("modifications")

        if 'disable' in modifs:
            self.__apply_enable_disable(modifs, 'disable', jmx)

        if 'enable' in modifs:
            self.__apply_enable_disable(modifs, 'enable', jmx)

        if 'set-prop' in modifs:
            items = modifs['set-prop']
            for path, text in iteritems(items):
                parts = path.split('>')
                if len(parts) < 2:
                    raise TaurusConfigError("JMeter: property selector must have at least 2 levels")
                sel_parts = ["[testname='%s']" % parts[0]]  # TODO: support wildcards in element names

                for add in parts[1:]:
                    sel_parts.append("[name='%s']" % add)
                selector = '>'.join(sel_parts)
                if not jmx.set_text(selector, text):
                    selector = '>'.join(sel_parts[:-1])
                    if jmx.get(selector):
                        jmx.append(selector, JMX._string_prop(parts[-1], text))
                    else:
                        self.log.warning("No elements matched for set-prop: %s", path)

    def __apply_enable_disable(self, modifs, action, jmx):
        items = modifs[action]
        if not isinstance(items, list):
            modifs[action] = [items]
            items = modifs[action]
        for name in items:
            candidates = jmx.get("[testname]")
            for candidate in candidates:
                if fnmatch.fnmatch(candidate.get('testname'), name):
                    jmx.set_enabled("[testname='%s']" % candidate.get('testname'),
                                    True if action == 'enable' else False)

    def install_required_tools(self):
        """
        check tools
        """
        required_tools = [JavaVM(self.log), TclLibrary(self.log)]
        for tool in required_tools:
            if not tool.check_if_installed():
                tool.install()

        jmeter_version = self.settings.get("version", JMeterExecutor.JMETER_VER)
        jmeter_path = self.settings.get("path", "~/.bzt/jmeter-taurus/{version}/")
        jmeter_path = get_full_path(jmeter_path)
        download_link = self.settings.get("download-link", None)
        plugins = self.settings.get("plugins", [])
        proxy = self.engine.config.get('settings').get('proxy')
        self.tool = JMeter(jmeter_path, self.log, jmeter_version, download_link, plugins, proxy)

        if self._need_to_install(self.tool):
            self.tool.install()

        self.settings['path'] = self.tool.tool_path

    @staticmethod
    def _need_to_install(tool):
        end_str_l = os.path.join('bin', 'jmeter' + EXE_SUFFIX)
        end_str_s = os.path.join('bin', 'jmeter')

        if os.path.isfile(tool.tool_path):
            if tool.check_if_installed():  # all ok, it's really tool path
                return False
            else:  # probably it's path to other tool)
                raise TaurusConfigError('JMeter: wrong tool path: %s' % tool.tool_path)

        if os.path.isdir(tool.tool_path):  # it's dir: fix tool path and install if needed
            tool.tool_path = os.path.join(tool.tool_path, end_str_l)
            if tool.check_if_installed():
                return False
            else:
                return True

        # similar to future jmeter directory
        if not (tool.tool_path.endswith(end_str_l) or tool.tool_path.endswith(end_str_s)):
            tool.tool_path = os.path.join(tool.tool_path, end_str_l)

        return True

    @staticmethod
    def __trim_jmeter_log(log_contents):
        lines = [line for line in log_contents.split("\n") if line]
        relevant_lines = list(dropwhile(lambda line: "ERROR" not in line, lines))
        if relevant_lines:
            return "\n".join(relevant_lines)
        else:
            return log_contents

    def get_error_diagnostics(self):
        diagnostics = []
        if self.stdout_file is not None:
            with open(self.stdout_file.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("JMeter STDOUT:\n" + contents)
        if self.stderr_file is not None:
            with open(self.stderr_file.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("JMeter STDERR:\n" + contents)
        if self.jmeter_log is not None and os.path.exists(self.jmeter_log):
            with open(self.jmeter_log) as fds:
                log_contents = fds.read().strip()
                trimmed_log = self.__trim_jmeter_log(log_contents)
                if trimmed_log:
                    diagnostics.append("JMeter log:\n" + trimmed_log)
        return diagnostics


class JTLReader(ResultsReader):
    """
    Class to read KPI JTL
    :type errors_reader: JTLErrorsReader
    """

    def __init__(self, filename, parent_logger, errors_filename=None):
        super(JTLReader, self).__init__()
        self.is_distributed = False
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.csvreader = IncrementalCSVReader(self.log, filename)
        self.read_records = 0
        if errors_filename:
            self.errors_reader = JTLErrorsReader(errors_filename, parent_logger)
        else:
            self.errors_reader = None

    def _read(self, last_pass=False):
        """
        Generator method that returns next portion of data

        :type last_pass: bool
        """
        if self.errors_reader:
            self.errors_reader.read_file(last_pass)

        for row in self.csvreader.read(last_pass):
            label = unicode_decode(row["label"])
            if self.is_distributed:
                concur = int(row["grpThreads"])
                trname = row["Hostname"] + row["threadName"][:row["threadName"].rfind('-')]
            else:
                concur = int(row["allThreads"])
                trname = ''

            rtm = int(row["elapsed"]) / 1000.0
            ltc = int(row["Latency"]) / 1000.0
            if "Connect" in row:
                cnn = int(row["Connect"]) / 1000.0
                if cnn < ltc:  # this is generally bad idea...
                    ltc -= cnn  # fixing latency included into connect time
            else:
                cnn = None

            rcd = row["responseCode"]
            if rcd.endswith('Exception'):
                rcd = rcd.split('.')[-1]

            if row["success"] != "true":
                error = row["responseMessage"]
            else:
                error = None

            byte_count = int(row.get("bytes", 0))

            tstmp = int(int(row["timeStamp"]) / 1000)
            self.read_records += 1
            yield tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname, byte_count

    def _calculate_datapoints(self, final_pass=False):
        for point in super(JTLReader, self)._calculate_datapoints(final_pass):
            if self.errors_reader:
                data = self.errors_reader.get_data(point[DataPoint.TIMESTAMP])
                for label, label_data in iteritems(point[DataPoint.CURRENT]):
                    if label in data:
                        label_data[KPISet.ERRORS] = data[label]
                    else:
                        label_data[KPISet.ERRORS] = {}

            yield point


class FuncJTLReader(FunctionalResultsReader):
    """
    Class to read trace.jtl
    :type filename: str
    :type parent_logger: logging.Logger
    """

    FILE_EXTRACTED_FIELDS = ["requestBody", "responseBody", "requestCookiesRaw"]

    def __init__(self, filename, engine, parent_logger):
        super(FuncJTLReader, self).__init__()
        self.executor_label = "JMeter"
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.parser = etree.XMLPullParser(events=('end',), recover=True)
        self.engine = engine
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self.failed_processing = False
        self.read_records = 0

    def read(self, last_pass=True):
        """
        Read the next part of the file
        """
        if self.failed_processing:
            return

        self.__read_next_chunk(last_pass)

        for _, elem in self.parser.read_events():
            if elem.getparent() is not None and elem.getparent().tag == 'testResults':
                sample = self._extract_sample(elem)
                self.read_records += 1

                elem.clear()
                while elem.getprevious() is not None:
                    del elem.getparent()[0]

                yield sample

    def __read_next_chunk(self, last_pass):
        while not self.failed_processing:
            read = self.file.get_bytes(size=1024 * 1024)
            if not read or not read.strip():
                break

            try:
                self.parser.feed(read)
            except etree.XMLSyntaxError as exc:
                self.failed_processing = True
                self.log.debug("Error reading trace.jtl: %s", traceback.format_exc())
                self.log.warning("Failed to parse errors XML: %s", exc)

            if not last_pass:
                break

    def _write_sample_data(self, filename, contents):
        artifact = self.engine.create_artifact(filename, ".bin")
        with open(artifact, 'wb') as fds:
            fds.write(contents.encode('utf-8'))
        return artifact

    @staticmethod
    def _extract_sample_assertions(sample_elem):
        assertions = []
        for result in sample_elem.findall("assertionResult"):
            name = result.findtext("name")
            failed = result.findtext("failure") == "true" or result.findtext("error") == "true"
            error_message = ""
            if failed:
                error_message = result.findtext("failureMessage")
            assertions.append({"name": name, "isFailed": failed, "errorMessage": error_message})
        return assertions

    def _parse_http_headers(self, header_str):
        headers = {}
        for line in header_str.split("\n"):
            clean_line = line.strip()
            if ":" in clean_line:
                key, value = clean_line.split(":", 1)
                headers[key] = value
        return headers

    def _parse_http_cookies(self, cookie_str):
        cookies = {}
        clean_line = cookie_str.strip()
        if "; " in clean_line:
            for item in clean_line.split("; "):
                key, value = item.split("=", 1)
                cookies[key] = value
        return cookies

    def _extract_sample_extras(self, sample_elem):
        method = sample_elem.findtext("method")
        uri = sample_elem.findtext("java.net.URL")  # smells like Java automarshalling
        req_headers = sample_elem.findtext("requestHeader") or ""
        resp_headers = sample_elem.findtext("responseHeader") or ""
        req_cookies = sample_elem.findtext("cookies") or ""

        thread_id = sample_elem.get("tn")
        split = thread_id.split("-")
        thread_group = "-".join(split[:-1])

        sample_extras = {
            "responseCode": sample_elem.get("rc"),
            "responseMessage": sample_elem.get("rm"),
            "responseTime": int(sample_elem.get("t") or 0),
            "connectTime": int(sample_elem.get("ct") or 0),
            "latency": int(sample_elem.get("lt") or 0),
            "responseSize": int(sample_elem.get("by") or 0),
            "requestSize": int(sample_elem.get("sby") or 0),
            "requestMethod": method,
            "requestURI": uri,

            "threadId": thread_id,
            "threadGroup": thread_group,

            "assertions": self._extract_sample_assertions(sample_elem),
            "requestHeaders": self._parse_http_headers(req_headers),
            "responseHeaders": self._parse_http_headers(resp_headers),
            "requestCookies": self._parse_http_cookies(req_cookies),

            "requestBody": sample_elem.findtext("queryString") or "",
            "responseBody": sample_elem.findtext("responseData") or "",
            "requestCookiesRaw": req_cookies,
        }

        sample_extras["requestBodySize"] = len(sample_extras["requestBody"])
        sample_extras["responseBodySize"] = len(sample_extras["responseBody"])
        sample_extras["requestCookiesSize"] = len(sample_extras["requestCookiesRaw"])

        return sample_extras

    def __write_sample_data_to_artifacts(self, sample_extras):
        for file_field in self.FILE_EXTRACTED_FIELDS:
            contents = sample_extras.pop(file_field)
            if contents:
                filename = "sample-%s" % file_field
                artifact = self._write_sample_data(filename, contents)
                sample_extras[file_field] = artifact

    def _extract_sample(self, sample_elem):
        tstmp = int(float(sample_elem.get("ts")) / 1000)
        label = sample_elem.get("lb")
        duration = float(sample_elem.get("t")) / 1000.0
        success = sample_elem.get("s") == "true"

        if success:
            status = "PASSED"
            error_msg = ""
            error_trace = ""
        else:
            assertion = self.__get_failed_assertion(sample_elem)
            if assertion is not None:
                status = "FAILED"
                error_msg = assertion.find("failureMessage").text
                error_trace = ""
            else:
                status = "BROKEN"
                error_msg, error_trace = self.get_failure(sample_elem)

        if error_msg.startswith("The operation lasted too long"):
            error_msg = "The operation lasted too long"

        sample_extras = self._extract_sample_extras(sample_elem)
        self.__write_sample_data_to_artifacts(sample_extras)

        return FunctionalSample(test_case=label, test_suite=self.executor_label, status=status,
                                start_time=tstmp, duration=duration,
                                error_msg=error_msg, error_trace=error_trace,
                                extras=sample_extras, subsamples=[])

    def get_failure(self, element):
        """
        Returns failure message and a stack trace
        """
        r_code = element.get('rc')
        if r_code and r_code.startswith("2") and element.get('s') == "false":
            children = [elem for elem in element.iterchildren() if elem.tag == "httpSample"]
            for child in children:
                child_failure = self.get_failure(child)
                if child_failure:
                    return child_failure
        else:
            message = element.get('rm')
            response_data = element.find("responseData")
            if response_data is not None:
                trace = response_data.text
            else:
                trace = ""
            return message, trace

    @staticmethod
    def __get_failed_assertion(element):
        """
        Returns first failed assertion, or None

        :rtype lxml.etree.Element
        """
        assertions = [elem for elem in element.iterchildren() if elem.tag == "assertionResult"]
        for assertion in assertions:
            failed = assertion.find("failure")
            error = assertion.find("error")
            if failed.text == "true" or error.text == "true":
                return assertion
        return None


class IncrementalCSVReader(object):
    """
    JTL csv reader
    """

    def __init__(self, parent_logger, filename):
        self.buffer = StringIO()
        self.csv_reader = None
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.indexes = {}
        self.partial_buffer = ""
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self.read_speed = 1024 * 1024

    def read(self, last_pass=False):
        """
        read data from jtl
        yield csv row
        :type last_pass: bool
        """
        lines = self.file.get_lines(size=self.read_speed, last_pass=last_pass)

        lines_read = 0
        bytes_read = 0

        for line in lines:
            if not line.endswith("\n"):
                self.partial_buffer += line
                continue

            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""

            lines_read += 1
            bytes_read += len(line)

            if self.csv_reader is None:
                dialect = guess_csv_dialect(line, force_doublequote=True)  # TODO: configurable doublequoting?
                self.csv_reader = csv.DictReader(self.buffer, [], dialect=dialect)
                self.csv_reader.fieldnames += line.strip().split(self.csv_reader.dialect.delimiter)
                self.log.debug("Analyzed header line: %s", self.csv_reader.fieldnames)
                continue

            if PY2:  # todo: fix csv parsing of unicode strings on PY2
                line = line.encode('utf-8')

            self.buffer.write(line)

        if lines_read:
            self.log.debug("Read: %s lines / %s bytes (at speed %s)", lines_read, bytes_read, self.read_speed)
            self._tune_speed(bytes_read)

            self.buffer.seek(0)
            for row in self.csv_reader:
                yield row

            self.buffer.seek(0)
            self.buffer.truncate(0)

    def _tune_speed(self, bytes_read):
        if bytes_read >= self.read_speed:
            self.read_speed = min(8 * 1024 * 1024, self.read_speed * 2)
        elif bytes_read < self.read_speed / 2:
            self.read_speed = max(self.read_speed / 2, 1024 * 1024)


class JTLErrorsReader(object):
    """
    Reader for errors.jtl, which is in XML max-verbose format

    :type filename: str
    :type parent_logger: logging.Logger
    """
    assertionMessage = GenericTranslator().css_to_xpath("assertionResult>failureMessage")
    url_xpath = GenericTranslator().css_to_xpath("java\\.net\\.URL")

    def __init__(self, filename, parent_logger):
        # http://stackoverflow.com/questions/9809469/python-sax-to-lxml-for-80gb-xml/9814580#9814580
        super(JTLErrorsReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.parser = etree.XMLPullParser(events=('end',))
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self.buffer = BetterDict()
        self.failed_processing = False

    def read_file(self, final_pass=False):
        """
        Read the next part of the file
        """
        while not self.failed_processing:
            read = self.file.get_bytes(size=1024 * 1024)
            if not read or not read.strip():
                break

            try:
                self.parser.feed(read)  # "Huge input lookup" error without capping :)
            except etree.XMLSyntaxError as exc:
                self.failed_processing = True
                self.log.debug("Error reading errors.jtl: %s", traceback.format_exc())
                self.log.warning("Failed to parse errors XML: %s", exc)

            for _, elem in self.parser.read_events():
                if elem.getparent() is not None and elem.getparent().tag == 'testResults':
                    self._parse_element(elem)
                    elem.clear()  # cleanup processed from the memory
                    while elem.getprevious() is not None:
                        del elem.getparent()[0]

            if not final_pass:
                break

    def _parse_element(self, elem):
        if elem.get('s'):
            result = elem.get('s')
        else:
            result = elem.xpath('success')[0].text
        if result == 'false':
            if elem.items():
                self._extract_standard(elem)
            else:
                self._extract_nonstandard(elem)

    def get_data(self, max_ts):
        """
        Get accumulated errors data up to specified timestamp
        """
        result = BetterDict()
        for t_stamp in sorted(self.buffer.keys()):
            if t_stamp > max_ts:
                break
            labels = self.buffer.pop(t_stamp)
            for label, label_data in iteritems(labels):
                res = result.get(label, [])
                for err_item in label_data:
                    KPISet.inc_list(res, ('msg', err_item['msg']), err_item)

        return result

    def _extract_standard(self, elem):
        t_stamp = int(elem.get("ts")) / 1000
        label = elem.get("lb")
        r_code = elem.get("rc")
        urls = elem.xpath(self.url_xpath)
        if urls:
            url = Counter({urls[0].text: 1})
        else:
            url = Counter()
        errtype = KPISet.ERRTYPE_ERROR

        failed_assertion = self.__get_failed_assertion(elem)
        if failed_assertion is not None:
            errtype = KPISet.ERRTYPE_ASSERT

        message = self.get_failure_message(elem)
        if message is None:
            message = elem.get('rm')
        err_item = KPISet.error_item_skel(message, r_code, 1, errtype, url)
        KPISet.inc_list(self.buffer.get(t_stamp).get(label, []), ("msg", message), err_item)
        KPISet.inc_list(self.buffer.get(t_stamp).get('', []), ("msg", message), err_item)

    def _extract_nonstandard(self, elem):
        t_stamp = int(self.__get_child(elem, 'timeStamp')) / 1000  # NOTE: will it be sometimes EndTime?
        label = self.__get_child(elem, "label")
        message = self.__get_child(elem, "responseMessage")
        r_code = self.__get_child(elem, "responseCode")

        urls = elem.xpath(self.url_xpath)
        if urls:
            url = Counter({urls[0].text: 1})
        else:
            url = Counter()
        errtype = KPISet.ERRTYPE_ERROR
        massert = elem.xpath(self.assertionMessage)
        if massert:
            errtype = KPISet.ERRTYPE_ASSERT
            message = massert[0].text
        err_item = KPISet.error_item_skel(message, r_code, 1, errtype, url)
        KPISet.inc_list(self.buffer.get(t_stamp).get(label, []), ("msg", message), err_item)
        KPISet.inc_list(self.buffer.get(t_stamp).get('', []), ("msg", message), err_item)

    def get_failure_message(self, element):
        """
        Returns failure message
        """

        failed_assertion = self.__get_failed_assertion(element)
        if failed_assertion is not None:
            assertion_message = self.__get_assertion_message(failed_assertion)
            if assertion_message:
                return assertion_message
            else:
                return element.get('rm')
        r_code = element.get('rc')
        if r_code and r_code.startswith("2"):
            if element.get('s') == "false":
                children = [elem for elem in element.iterchildren() if elem.tag == "httpSample"]
                for child in children:
                    child_message = self.get_failure_message(child)
                    if child_message:
                        return child_message
        else:
            return element.get('rm')

    def __get_assertion_message(self, assertion_element):
        """
        Returns assertion failureMessage if "failureMessage" element exists
        """
        failure_message_elem = assertion_element.find("failureMessage")
        if failure_message_elem is not None:
            msg = failure_message_elem.text
            if msg.startswith("The operation lasted too long"):
                msg = "The operation lasted too long"

            return msg

    def __get_failed_assertion(self, element):
        """
        Returns first failed assertion, or None

        :rtype lxml.etree.Element
        """
        assertions = [elem for elem in element.iterchildren() if elem.tag == "assertionResult"]
        for assertion in assertions:
            if self.__assertion_is_failed(assertion):
                return assertion

    def __assertion_is_failed(self, assertion_element):
        """
        returns True if assertion failed
        """
        failed = assertion_element.find("failure")
        error = assertion_element.find("error")
        if failed.text == "true" or error.text == "true":
            return True
        return False

    def __get_child(self, elem, tag):
        for child in elem:
            if child.tag == tag:
                return child.text


class JMeter(RequiredTool):
    """
    JMeter tool
    """

    def __init__(self, tool_path, parent_logger, jmeter_version, jmeter_download_link, plugins, proxy):
        super(JMeter, self).__init__("JMeter", tool_path, jmeter_download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.version = jmeter_version
        self.mirror_manager = JMeterMirrorsManager(self.log, self.version)
        self.plugins = plugins
        self.proxy_settings = proxy
        self.tool_path = self.tool_path.format(version=self.version)

    def check_if_installed(self):
        self.log.debug("Trying jmeter: %s", self.tool_path)
        try:
            with tempfile.NamedTemporaryFile(prefix="jmeter", suffix="log", delete=False) as jmlog:
                jm_proc = shell_exec([self.tool_path, '-j', jmlog.name, '--version'], stderr=subprocess.STDOUT)
                jmout, jmerr = communicate(jm_proc)
                self.log.debug("JMeter check: %s / %s", jmout, jmerr)

            os.remove(jmlog.name)

            if "is too low to run JMeter" in jmout:
                raise ToolError("Java version is too low to run JMeter")

            return True

        except OSError:
            self.log.debug("JMeter check failed.")
            return False

    def _pmgr_call(self, params):
        cmd = [self._pmgr_path()] + params
        proc = shell_exec(cmd)
        return communicate(proc)

    def install_for_jmx(self, jmx_file):
        if not os.path.isfile(jmx_file):
            self.log.warning("Script %s not found" % jmx_file)
            return

        try:
            out, err = self._pmgr_call(["install-for-jmx", jmx_file])
            self.log.debug("Try to detect plugins for %s\n%s\n%s", jmx_file, out, err)
        except KeyboardInterrupt:
            raise
        except BaseException as exc:
            self.log.warning("Failed to detect plugins for %s: %s", jmx_file, exc)
            return

        if err and "Wrong command: install-for-jmx" in err:  # old manager
            self.log.debug("pmgr can't discover jmx for plugins")

        if out and "Restarting JMeter" in out:
            time.sleep(5)  # allow for modifications to complete

    def __install_jmeter(self, dest):
        if self.download_link:
            jmeter_dist = self._download(use_link=True)
        else:
            jmeter_dist = self._download()

        try:
            self.log.info("Unzipping %s to %s", jmeter_dist, dest)
            unzip(jmeter_dist, dest, 'apache-jmeter-%s' % self.version)
        finally:
            os.remove(jmeter_dist)

        # set exec permissions
        os.chmod(os.path.join(dest, 'bin', 'jmeter'), 0o755)
        os.chmod(os.path.join(dest, 'bin', 'jmeter' + EXE_SUFFIX), 0o755)

        if not self.check_if_installed():
            raise ToolError("Unable to run %s after installation!" % self.tool_name)

    def __download_additions(self, tools):
        downloader = ExceptionalDownloader()
        with ProgressBarContext() as pbar:
            for tool in tools:
                url = tool[0]
                _file = os.path.basename(url)
                self.log.info("Downloading %s from %s", _file, url)
                try:
                    downloader.get(url, tool[1], reporthook=pbar.download_callback)
                except KeyboardInterrupt:
                    raise
                except BaseException as exc:
                    raise TaurusNetworkError("Error while downloading %s: %s" % (_file, exc))

    def __install_plugins_manager(self, plugins_manager_path):
        installer = "org.jmeterplugins.repository.PluginManagerCMDInstaller"
        cmd = ["java", "-cp", plugins_manager_path, installer]
        self.log.debug("Trying: %s", cmd)
        try:
            proc = shell_exec(cmd)
            out, err = communicate(proc)
            self.log.debug("Install PluginsManager: %s / %s", out, err)
        except KeyboardInterrupt:
            raise
        except BaseException as exc:
            raise ToolError("Failed to install PluginsManager: %s" % exc)

    def __install_plugins(self, plugins_manager_cmd):
        plugin_str = ",".join(self.plugins)
        self.log.info("Installing JMeter plugins: %s", plugin_str)
        cmd = [plugins_manager_cmd, 'install', plugin_str]
        self.log.debug("Trying: %s", cmd)
        try:
            proc = shell_exec(cmd)
            out, err = communicate(proc)
            self.log.debug("Install plugins: %s / %s", out, err)
        except KeyboardInterrupt:
            raise
        except BaseException as exc:
            raise ToolError("Failed to install plugins %s: %s" % (plugin_str, exc))

        if out and "Restarting JMeter" in out:
            time.sleep(5)  # allow for modifications to complete

    def _pmgr_path(self):
        dest = get_full_path(self.tool_path, step_up=2)
        return os.path.join(dest, 'bin', 'PluginsManagerCMD' + EXE_SUFFIX)

    def install(self):
        dest = get_full_path(self.tool_path, step_up=2)
        self.log.info("Will install %s into %s", self.tool_name, dest)
        plugins_manager_name = os.path.basename(JMeterExecutor.PLUGINS_MANAGER)
        cmdrunner_name = os.path.basename(JMeterExecutor.CMDRUNNER)
        plugins_manager_path = os.path.join(dest, 'lib', 'ext', plugins_manager_name)
        cmdrunner_path = os.path.join(dest, 'lib', cmdrunner_name)
        direct_install_tools = [  # source link and destination
            [JMeterExecutor.PLUGINS_MANAGER, plugins_manager_path],
            [JMeterExecutor.CMDRUNNER, cmdrunner_path]]
        plugins_manager_cmd = self._pmgr_path()

        self.__install_jmeter(dest)
        self.__download_additions(direct_install_tools)
        self.__install_plugins_manager(plugins_manager_path)
        self.__install_plugins(plugins_manager_cmd)

        cleaner = JarCleaner(self.log)
        cleaner.clean(os.path.join(dest, 'lib'))

    def ctg_plugin_installed(self):
        """
        Simple check if ConcurrentThreadGroup is available
        :return:
        """
        ext_dir = os.path.join(get_full_path(self.tool_path, step_up=2), 'lib', 'ext')
        if os.path.isdir(ext_dir):
            list_of_jars = [file_name for file_name in os.listdir(ext_dir) if file_name.endswith('.jar')]
            if any([file_name.startswith('jmeter-plugins-casutg') for file_name in list_of_jars]):
                return True

        return False


class JarCleaner(object):
    def __init__(self, parent_logger):
        self.log = parent_logger.getChild(self.__class__.__name__)

    @staticmethod
    def __extract_version(jar):
        version_str = jar.split('-')[-1]
        return version_str.replace('.jar', '')

    def clean(self, path):
        """
        Remove old jars
        :param path: str
        """
        self.log.debug("Removing old jars from %s", path)
        jarlib = namedtuple("jarlib", ("file_name", "lib_name", "version"))
        jars = [fname for fname in os.listdir(path) if '-' in fname and os.path.isfile(os.path.join(path, fname))]
        jar_libs = [jarlib(file_name=jar,
                           lib_name='-'.join(jar.split('-')[:-1]),
                           version=JarCleaner.__extract_version(jar))
                    for jar in jars]

        duplicated_libraries = set()
        for jar_lib_obj in jar_libs:
            similar_packages = [lib for lib in jar_libs if lib.lib_name == jar_lib_obj.lib_name]
            if len(similar_packages) > 1:
                right_version = max(similar_packages, key=lambda l: LooseVersion(l.version))
                similar_packages.remove(right_version)
                duplicated_libraries.update(similar_packages)

        for old_lib in duplicated_libraries:
            os.remove(os.path.join(path, old_lib.file_name))
            self.log.debug("Old jar removed %s", old_lib.file_name)


class JMeterMirrorsManager(MirrorsManager):
    def __init__(self, parent_logger, jmeter_version):
        self.jmeter_version = str(jmeter_version)
        super(JMeterMirrorsManager, self).__init__(JMeterExecutor.MIRRORS_SOURCE, parent_logger)

    def _parse_mirrors(self):
        links = []
        if self.page_source is not None:
            self.log.debug('Parsing mirrors...')
            select_search_pattern = re.compile(r'<select name="Preferred">.*?</select>', re.MULTILINE | re.DOTALL)
            option_search_pattern = re.compile(r'<option value=".*?">')
            select_element = select_search_pattern.findall(self.page_source)
            if select_element:
                option_elements = option_search_pattern.findall(select_element[0])
                link_tail = "/jmeter/binaries/apache-jmeter-{version}.zip".format(version=self.jmeter_version)
                links = [link.strip('<option value="').strip('">') + link_tail for link in option_elements]
        links.append(JMeterExecutor.JMETER_DOWNLOAD_LINK.format(version=self.jmeter_version))
        self.log.debug('Total mirrors: %d', len(links))
        # place HTTPS links first, preserving the order of HTTP links
        sorted_links = sorted(links, key=lambda l: l.startswith("https"), reverse=True)
        return sorted_links
