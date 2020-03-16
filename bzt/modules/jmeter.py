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
import codecs
import copy
import csv
import fnmatch
import os
import re
import socket
import tempfile
import time
import traceback
from collections import Counter, namedtuple
from distutils.version import LooseVersion
from itertools import dropwhile
from io import StringIO

from cssselect import GenericTranslator
from lxml import etree

from bzt import TaurusConfigError, ToolError, TaurusInternalException, TaurusNetworkError
from bzt.engine import Scenario, FileLister, HavingInstallableTools, ScenarioExecutor
from bzt.engine import SelfDiagnosable, SETTINGS
from bzt.jmx import JMX, JMeterScenarioBuilder, LoadSettingsProcessor, try_convert
from bzt.modules.aggregator import ResultsReader, DataPoint, KPISet
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.modules.functional import FunctionalResultsReader, FunctionalSample
from bzt.requests_model import ResourceFilesCollector, has_variable_pattern, HierarchicRequestParser
from bzt.utils import iteritems, numeric_types, unicode_decode
from bzt.utils import get_full_path, EXE_SUFFIX, MirrorsManager, ExceptionalDownloader, get_uniq_name, is_windows
from bzt.utils import BetterDict, guess_csv_dialect, dehumanize_time, CALL_PROBLEMS
from bzt.utils import unzip, RequiredTool, JavaVM, shutdown_process, ProgressBarContext, TclLibrary, FileReader


def get_child_assertion(element):
    """
    Returns first failed assertion, or None

    :rtype lxml.etree.Element
    """
    for child in element.iterchildren():
        msg, name = parse_assertion(child)
        if msg:
            return msg, name

    return "", None


def parse_assertion(element, default=""):
    assertion = element.tag == "assertionResult"
    name = element.findtext("name")
    failure = element.findtext("failure") == "true"
    error = element.findtext("error") == "true"
    failure_message = element.findtext("failureMessage", default=default)
    if failure_message and failure_message.startswith("The operation lasted too long"):
        failure_message = "The operation lasted too long"

    wrong_message = "One or more sub-samples failed"

    failed_assertion = assertion and (failure or error) and (failure_message != wrong_message)

    if not failed_assertion:
        failure_message = ""

    return failure_message, name


class JMeterExecutor(ScenarioExecutor, WidgetProvider, FileLister, HavingInstallableTools, SelfDiagnosable):
    """
    JMeter executor module

    :type modified_jmx: str
    :type jmeter_log: str
    :type properties_file: str
    :type sys_properties_file: str
    """
    UDP_PORT_NUMBER = None

    def __init__(self):
        super(JMeterExecutor, self).__init__()
        self.original_jmx = None
        self.modified_jmx = None
        self.jmeter_log = None
        self.properties = BetterDict()
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

        duration = 0

        if ramp_up is not None:
            ramp_up = try_convert(ramp_up, dehumanize_time, 0)
            duration += ramp_up

        if hold is not None:
            hold = try_convert(hold, dehumanize_time, 0)
            duration += hold

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

        throughput = try_convert(throughput, float, default=0)
        concurrency = try_convert(concurrency, default=0)
        iterations = try_convert(iterations, default=0)
        steps = try_convert(steps, default=0)

        return self.LOAD_FMT(concurrency=concurrency, ramp_up=ramp_up, throughput=throughput, hold=hold,
                             iterations=iterations, duration=duration, steps=steps)

    def get_specific_load(self):
        """
        Helper method to read load specification
        """
        # throughput, concurrency, iterations, steps, hold, ramp_up
        raw_load = self.get_raw_load()

        hold = try_convert(raw_load.hold or 0, dehumanize_time)

        ramp_up = try_convert(raw_load.ramp_up, dehumanize_time)

        if not hold:
            duration = ramp_up
        elif not ramp_up:
            duration = hold
        elif isinstance(ramp_up, numeric_types) and isinstance(hold, numeric_types):
            duration = hold + ramp_up
        else:
            duration = 1  # dehumanize_time(<sum_of_props>) can be unpredictable so we use default there

        throughput = try_convert(raw_load.throughput, float)
        concurrency = try_convert(raw_load.concurrency)
        iterations = try_convert(raw_load.iterations)
        steps = try_convert(raw_load.steps)

        if not iterations:
            if duration:
                iterations = 0  # which means infinite
            else:
                iterations = 1

        return self.LOAD_FMT(concurrency=concurrency, ramp_up=ramp_up, throughput=throughput, hold=hold,
                             iterations=iterations, duration=duration, steps=steps)

    @staticmethod
    def _get_tool_version(jmx_file):
        jmx = JMX(jmx_file)
        selector = 'jmeterTestPlan'
        test_plan = jmx.get(selector)[0]
        ver = test_plan.get('jmeter')
        if isinstance(ver, str):
            index = ver.find(" ")
            if index != -1:
                return ver[:index]

        return JMeter.VERSION

    def prepare(self):
        """
        Preparation for JMeter involves either getting existing JMX
        and modifying it, or generating new JMX from input data. Then,
        original JMX is modified to contain JTL writing classes with
        required settings and have workload as suggested by Provisioning

        :raise TaurusConfigError:
        """
        super(JMeterExecutor, self).prepare()
        self.jmeter_log = self.engine.create_artifact("jmeter", ".log")
        self._set_remote_port()
        self.distributed_servers = self.execution.get('distributed', self.distributed_servers)

        is_jmx_generated = False

        self.original_jmx = self.get_script_path()
        if self.settings.get("version", JMeter.VERSION, force_set=True) == "auto":
            self.settings["version"] = self._get_tool_version(self.original_jmx)

        if not self.original_jmx:
            if self.get_scenario().get("requests"):
                self.original_jmx = self.__jmx_from_requests()
                is_jmx_generated = True
            else:
                raise TaurusConfigError("You must specify either a JMX file or list of requests to run JMeter")

        self.__set_jvm_properties()
        self.__set_system_properties()
        self.__set_jmeter_properties()

        self.install_required_tools()

        if self.engine.aggregator.is_functional:
            flags = {"connectTime": True}
            version = LooseVersion(self.tool.version)
            major = version.version[0]
            if major == 2:
                flags["bytes"] = True
            else:
                flags["sentBytes"] = True
            self.settings.merge({"xml-jtl-flags": flags})

        modified = self.__get_modified_jmx(self.original_jmx, is_jmx_generated)
        self.modified_jmx = self.__save_modified_jmx(modified, self.original_jmx, is_jmx_generated)

        # check for necessary plugins and install them if needed
        if self.settings.get("detect-plugins", True):
            self.tool.install_for_jmx(self.modified_jmx)

        self.stdout = open(self.engine.create_artifact("jmeter", ".out"), "w")
        self.stderr = open(self.engine.create_artifact("jmeter", ".err"), "w")

        if self.engine.is_functional_mode():
            self.reader = FuncJTLReader(self.log_jtl, self.engine, self.log)
            self.reader.is_distributed = len(self.distributed_servers) > 0
            self.reader.executor_label = self.label
            self.engine.aggregator.add_underling(self.reader)
        else:
            err_msg_separator = self.settings.get("error-message-separator")
            self.reader = JTLReader(self.kpi_jtl, self.log, self.log_jtl, err_msg_separator)
            self.reader.is_distributed = len(self.distributed_servers) > 0
            assert isinstance(self.reader, JTLReader)
            self.engine.aggregator.add_underling(self.reader)

    def __set_system_properties(self):
        sys_props = self.settings.get("system-properties")
        if sys_props:
            self.log.debug("Additional system properties %s", sys_props)
            self.properties.merge(sys_props)
            sys_props_file = self.engine.create_artifact("system", ".properties")
            JMeterExecutor.__write_props_to_file(sys_props_file, sys_props)
            self.sys_properties_file = sys_props_file

    def __set_jvm_properties(self):
        heap_size = self.settings.get("memory-xmx", None)
        if heap_size is not None:
            self.log.debug("Setting JVM heap size to %s", heap_size)
            self.env.add_java_param({"JVM_ARGS": "-Xmx%s" % heap_size})

    def __set_jmeter_properties(self):
        props = copy.deepcopy(self.settings.get("properties"))
        props_local = copy.deepcopy(self.get_scenario().get("properties"))
        if self.distributed_servers and self.settings.get("gui", False):
            props_local.merge({"remote_hosts": ",".join(self.distributed_servers)})
        props_local.update({"jmeterengine.nongui.port": self.management_port})
        props_local.update({"jmeterengine.nongui.maxport": self.management_port})
        props_local.update({"jmeter.save.saveservice.timestamp_format": "ms"})
        props_local.update({"sampleresult.default.encoding": "UTF-8"})
        props.merge(props_local)

        user_cp = [self.engine.artifacts_dir, get_full_path(self.original_jmx, step_up=1)]

        for _file in self.execution.get('files', []):
            full_path = get_full_path(_file)
            if os.path.isdir(full_path):
                user_cp.append(full_path)
            elif full_path.lower().endswith('.jar'):
                user_cp.append((get_full_path(_file, step_up=1)))

        if 'user.classpath' in props:
            user_cp.append(props['user.classpath'])

        props['user.classpath'] = os.pathsep.join(user_cp).replace(os.path.sep, "/")  # replace to avoid Windows issue

        self.log.debug("Additional properties: %s", props)
        self.properties.merge(props)
        self.properties_file = self.engine.create_artifact("jmeter-bzt", ".properties")
        JMeterExecutor.__write_props_to_file(self.properties_file, props)

    def startup(self):
        """
        Should start JMeter as fast as possible.
        """
        cmdline = [self.tool.tool_path, "-t", self.modified_jmx, "-j", self.jmeter_log, "-q", self.properties_file]
        if not self.settings.get("gui", False):
            cmdline += ["-n"]

        if self.distributed_servers:
            cmdline += ["-G", self.properties_file]

        if self.sys_properties_file:
            cmdline += ["-S", self.sys_properties_file]
        if self.distributed_servers and not self.settings.get("gui", False):
            cmdline += ['-R%s' % ','.join(self.distributed_servers)]

        # fix for JMeter 4.0 bug where jmeter.bat requires JMETER_HOME to be set
        if is_windows() and self.tool.version == "4.0" and not self.env.get("JMETER_HOME"):
            tool_dir = self.tool.tool_path
            if os.path.isfile(self.tool.tool_path):
                tool_dir = get_full_path(self.tool.tool_path, step_up=2)
            self.env.set({"JMETER_HOME": tool_dir})

        self.process = self._execute(cmdline)

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
        distr_multiplier = len(self.execution.get('distributed', [None]))  # 1 for regular, N of servers for distributed

        max_attempts = self.settings.get("shutdown-wait", 5) * distr_multiplier
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
        super(JMeterExecutor, self).post_process()

    def has_results(self):
        if self.reader and self.reader.read_records:
            return True
        else:
            return False

    def _process_stopped(self, cycles):
        while cycles > 0:
            if not (self.process and self.process.poll() is None):
                return True
            cycles -= 1
            time.sleep(self.engine.check_interval)
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
        version = LooseVersion(self.tool.version)
        csv_flags = self.settings.get('csv-jtl-flags')
        if version < LooseVersion("2.13"):
            csv_flags['^connectTime'] = False

        self.kpi_jtl = self.engine.create_artifact("kpi", ".jtl")
        kpi_lst = jmx.new_kpi_listener(self.kpi_jtl, csv_flags)
        self.__add_listener(kpi_lst, jmx)

        verbose = self.engine.config.get(SETTINGS).get("verbose", False)
        jtl_log_level = self.execution.get('write-xml-jtl', "full" if verbose else 'error')

        xml_flags = self.settings.get('xml-jtl-flags')

        if jtl_log_level == 'error':
            self.log_jtl = self.engine.create_artifact("error", ".jtl")
            log_lst = jmx.new_xml_listener(self.log_jtl, False, xml_flags)
            self.__add_listener(log_lst, jmx)
        elif jtl_log_level == 'full':
            self.log_jtl = self.engine.create_artifact("trace", ".jtl")
            log_lst = jmx.new_xml_listener(self.log_jtl, True, xml_flags)
            self.__add_listener(log_lst, jmx)

    def __force_tran_parent_sample(self, jmx):
        scenario = self.get_scenario()
        if scenario.get("force-parent-sample", False):
            self.log.debug("Enforcing parent sample for transaction controller")
            jmx.set_text('TransactionController > boolProp[name="TransactionController.parent"]', 'true')

    def __get_modified_jmx(self, original, is_jmx_generated):
        """
        add two listeners to test plan:
            - to collect basic stats for KPIs
            - to collect detailed errors/trace info
        :return: path to artifact
        """
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
            version = LooseVersion(self.tool.version)
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
                execution_files = self.execution.get('files', [], force_set=True)
                execution_files.extend(self._resolve_jmx_relpaths(resource_files_from_jmx))
                self.__modify_resources_paths_in_jmx(jmx.tree, resource_files_from_jmx)
                script_name, script_ext = os.path.splitext(os.path.basename(self.original_jmx))
                self.original_jmx = self.engine.create_artifact(script_name, script_ext)
                jmx.save(self.original_jmx)
                scenario[Scenario.SCRIPT] = self.original_jmx

        script = self.get_script_path()
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
        for source in scenario.get_data_sources():
            files.append(source['path'])
        requests = scenario.get_requests(parser=HierarchicRequestParser)
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
        self.tool = self._get_tool(JMeter, config=self.settings, props=self.properties)

        required_tools = [self._get_tool(JavaVM), self._get_tool(TclLibrary), self.tool]
        for tool in required_tools:
            if not tool.check_if_installed():
                tool.install()

        self.settings['path'] = self.tool.tool_path

    @staticmethod
    def __trim_jmeter_log(log_contents):
        lines = [line for line in log_contents.split("\n") if line]
        relevant_lines = list(dropwhile(lambda lin: "ERROR" not in lin, lines))
        if relevant_lines:
            return "\n".join(relevant_lines)
        else:
            return log_contents

    def get_error_diagnostics(self):
        diagnostics = []
        if self.stdout is not None:
            with codecs.open(self.stdout.name, encoding='utf-8') as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("JMeter STDOUT:\n" + contents)
        if self.stderr is not None:
            with codecs.open(self.stderr.name, encoding='utf-8') as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("JMeter STDERR:\n" + contents)
        if self.jmeter_log is not None and os.path.exists(self.jmeter_log):
            with codecs.open(self.jmeter_log, encoding='utf-8') as fds:
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

    def __init__(self, filename, parent_logger, errors_filename=None, err_msg_separator=None):
        super(JTLReader, self).__init__()
        self.is_distributed = False
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.csvreader = IncrementalCSVReader(self.log, filename)
        self.read_records = 0
        if errors_filename:
            self.errors_reader = JTLErrorsReader(errors_filename, parent_logger, err_msg_separator)
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

            tstmp = int(int(row["timeStamp"]) / 1000.0)
            self.read_records += 1
            yield tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname, byte_count

    def _calculate_datapoints(self, final_pass=False):
        for point in super(JTLReader, self)._calculate_datapoints(final_pass):
            if self.errors_reader:
                self.errors_reader.read_file()
                err_details = self.errors_reader.get_data(point[DataPoint.TIMESTAMP])  # get only for labels we have
                for label in err_details:
                    if label in point[DataPoint.CURRENT]:
                        point[DataPoint.CURRENT][label][KPISet.ERRORS] = err_details[label]
                    else:
                        self.log.warning("Had error data but no KPISet %s: %s", label, err_details[label])

                for label, label_data in iteritems(point[DataPoint.CURRENT]):
                    if label in err_details:
                        pass
                    elif label_data[KPISet.ERRORS]:
                        self.log.warning("No details for errors of %s, dropped info: %s", label,
                                         label_data[KPISet.ERRORS])

            point[DataPoint.SOURCE_ID] = self.csvreader.file.name + "@" + str(id(self))
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
            read = self.file.get_bytes(size=1024 * 1024, decode=False)
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
        tstmp = int(float(sample_elem.get("ts")) / 1000.0)
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
    url_xpath = GenericTranslator().css_to_xpath("java\\.net\\.URL")

    def __init__(self, filename, parent_logger, err_msg_separator=None):
        # http://stackoverflow.com/questions/9809469/python-sax-to-lxml-for-80gb-xml/9814580#9814580
        super(JTLErrorsReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.parser = etree.XMLPullParser(events=('end',))
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self.buffer = BetterDict()
        self.failed_processing = False
        self.err_msg_separator = err_msg_separator

    def read_file(self, final_pass=False):
        """
        Read the next part of the file
        """
        start_size = os.path.getsize(self.file.name) if self.file.is_ready() else 0
        while not self.failed_processing:
            # we need to feed bytes, not a unicode string, into the parser
            read = self.file.get_bytes(size=1024 * 1024, decode=False)  # "Huge input lookup" error without capping :)
            if not read or not read.strip():
                break

            self.log.debug("Read bytes from error file: %s", len(read))

            try:
                self.parser.feed(read)
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
            elif self.file.is_ready() and os.path.getsize(self.file.name) != start_size:
                self.log.debug("Error file size has changed %d=>%d while reading offset %d",
                               start_size, os.path.getsize(self.file.name), self.file.offset)
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
            if t_stamp >= max_ts + 1:
                break
            labels = self.buffer.pop(t_stamp)
            for label, label_data in iteritems(labels):
                res = result.get(label, [], force_set=True)
                for err_item in label_data:
                    KPISet.inc_list(res, ('msg', err_item['msg']), err_item)

        if result:
            self.log.debug("Got error info for %s, labels: %s", max_ts, result.keys())
        return result

    def _extract_standard(self, elem):
        t_stamp = int(elem.get("ts")) / 1000.0
        label = elem.get("lb")
        message = elem.get('rm')
        r_code = elem.get("rc")

        self._extract_common(elem, label, r_code, t_stamp, message)

    def _extract_common(self, elem, label, r_code, t_stamp, r_msg):
        f_msg, f_url, f_rc, f_tag, f_type = self.find_failure(elem, r_msg, r_code)

        if f_type == KPISet.ERRTYPE_SUBSAMPLE:
            url_counts = Counter({f_url: 1})
        else:
            urls = elem.xpath(self.url_xpath)
            if urls:
                url_counts = Counter({urls[0].text: 1})
            else:
                url_counts = Counter()

        err_item = KPISet.error_item_skel(f_msg, f_rc, 1, f_type, url_counts, f_tag)
        buf = self.buffer.get(t_stamp, force_set=True)
        KPISet.inc_list(buf.get(label, [], force_set=True), ("msg", f_msg), err_item)
        KPISet.inc_list(buf.get('', [], force_set=True), ("msg", f_msg), err_item)

    def _extract_nonstandard(self, elem):
        t_stamp = int(elem.findtext("timeStamp")) / 1000.0  # NOTE: will it be sometimes EndTime?
        label = elem.findtext("label")
        message = elem.findtext("responseMessage")
        r_code = elem.findtext("responseCode")

        self._extract_common(elem, label, r_code, t_stamp, message)

    def find_failure(self, element, def_msg="", def_rc=None):
        """ returns (message, url, rc, tag, err_type) """
        rc = element.get("rc", default="")

        e_msg = ""
        url = None
        err_type = KPISet.ERRTYPE_ERROR

        a_msg, name = get_child_assertion(element)

        if not rc.startswith("2"):  # this sample is failed
            e_msg = element.get("rm", default="")
            url = element.xpath(self.url_xpath)
            url = url[0].text if url else element.get("lb")
        elif a_msg:
            err_type = KPISet.ERRTYPE_ASSERT
        elif element.get("s") == "false":  # has failed sub element, we should look deeper...
            for child in element.iterchildren():
                if child.tag in ("httpSample", "sample"):  # let's check sub samples..
                    e_msg, url, rc, name, err_type = self.find_failure(child)
                    if e_msg:
                        if err_type == KPISet.ERRTYPE_ERROR:  # replace subsample error
                            err_type = KPISet.ERRTYPE_SUBSAMPLE
                        break

        if not (err_type == KPISet.ERRTYPE_SUBSAMPLE) and self.err_msg_separator and (a_msg or e_msg):
            msg = self.err_msg_separator.join((a_msg, e_msg))
        elif e_msg:
            msg = e_msg
        else:
            msg = a_msg

        if not msg and def_msg:  # top level, empty result
            msg = def_msg
            if self.err_msg_separator:  # add appropriate separator to default msg
                msg = self.err_msg_separator + msg

        return msg, url, rc or def_rc, name, err_type


class XMLJTLReader(JTLErrorsReader, ResultsReader):
    def __init__(self, filename, parent_logger):
        super(XMLJTLReader, self).__init__(filename, parent_logger)
        self.items = []

    def _read(self, final_pass=False):
        self.read_file()
        while self.items:
            yield self.items.pop(0)

    def _parse_element(self, elem):
        tstmp = int(int(elem.get("ts")) / 1000.0)
        label = elem.get("lb")
        rtm = int(elem.get("t")) / 1000.0
        ltc = int(elem.get("lt")) / 1000.0 if "lt" in elem.attrib else 0
        cnn = int(elem.get("ct")) / 1000.0 if "ct" in elem.attrib else 0
        byte_count = int(elem.get("by")) if "by" in elem.attrib else 0
        concur = int(elem.get("na")) if "na" in elem.attrib else 0
        trname = ''

        rcd = elem.get("rc")
        message = self.find_failure(elem, def_msg=elem.get("rm"))[0]

        error = message if elem.get("s") == "false" else None
        self.items.append((tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname, byte_count))


class JMeter(RequiredTool):
    """
    JMeter tool
    """
    PLUGINS_MANAGER_VERSION = "1.3"
    PLUGINS_MANAGER = 'https://search.maven.org/remotecontent?filepath=kg/apc/jmeter-plugins-manager/' \
                      '{ver}/jmeter-plugins-manager-{ver}.jar'.format(ver=PLUGINS_MANAGER_VERSION)
    CMDRUNNER = 'https://search.maven.org/remotecontent?filepath=kg/apc/cmdrunner/2.2/cmdrunner-2.2.jar'
    VERSION = "5.2.1"

    def __init__(self, config=None, props=None, **kwargs):
        settings = config or {}
        props = props or {}

        version = settings.get("version", JMeter.VERSION)
        jmeter_path = settings.get("path", "~/.bzt/jmeter-taurus/{version}/")
        jmeter_path = get_full_path(jmeter_path).format(version=version)

        download_link = settings.get("download-link", None)
        if download_link is not None:
            download_link = download_link.format(version=version)

        self.plugins = settings.get("plugins", [])

        super(JMeter, self).__init__(tool_path=jmeter_path, download_link=download_link, version=version, **kwargs)

        self.mirror_manager = JMeterMirrorsManager(self.http_client, self.log, self.version)

        additional_jvm_props = self._get_jvm_props(props)
        for key in additional_jvm_props:
            self.env.add_java_param({"JVM_ARGS": "-D%s=%s" % (key, additional_jvm_props[key])})

    def _get_jvm_props(self, settings):
        props = {key: settings[key] for key in settings if key.startswith("jpgc.")}

        if self.http_client:
            props.update(self.http_client.get_proxy_props())

        return props

    def check_if_installed(self):
        end_str_l = os.path.join('bin', 'jmeter' + EXE_SUFFIX)
        end_str_s = os.path.join('bin', 'jmeter')

        if os.path.isfile(self.tool_path):
            if self.run_and_check():  # all ok, it's really tool path
                return True
            else:  # probably it's path to other tool)
                raise TaurusConfigError('JMeter: wrong tool path: %s' % self.tool_path)

        if os.path.isdir(self.tool_path):  # it's dir: fix tool path and install if needed
            self.tool_path = os.path.join(self.tool_path, end_str_l)
            return self.run_and_check()

        # similar to future jmeter directory
        if not (self.tool_path.endswith(end_str_l) or self.tool_path.endswith(end_str_s)):
            self.tool_path = os.path.join(self.tool_path, end_str_l)

        return False

    def run_and_check(self):
        self.log.debug("Trying JMeter..")
        jmlog = tempfile.NamedTemporaryFile(prefix="jmeter", suffix="log", delete=False)

        cmd_line = [self.tool_path, '-j', jmlog.name, '--version']
        try:
            out, err = self.call(cmd_line)
        except CALL_PROBLEMS as exc:
            msg = "JMeter check failed: %s" % exc
            if os.path.exists(self.tool_path):
                raise ToolError(msg)

            self.log.debug(msg)
            return False
        finally:
            jmlog.close()

        self.log.debug("JMeter check: %s / %s", out, err)
        return True

    def _pmgr_call(self, params):
        cmd = [self._pmgr_path()] + params
        return self.call(cmd)

    def install_for_jmx(self, jmx_file):
        if not os.path.isfile(jmx_file):
            self.log.warning("Script %s not found" % jmx_file)
            return

        params = ["install-for-jmx", jmx_file]

        try:
            out, err = self._pmgr_call(params)
        except CALL_PROBLEMS as exc:
            self.log.warning("Failed to detect plugins for %s: %s", jmx_file, exc)
            return

        self.log.debug("Try to detect plugins for %s\n%s\n%s", jmx_file, out, err)

        if err and "Wrong command: install-for-jmx" in err:  # old manager
            self.log.debug("pmgr can't discover jmx for plugins")

        if out and "Plugins manager will apply some modifications" in out:
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
        downloader = ExceptionalDownloader(self.http_client)
        with ProgressBarContext() as pbar:
            for url, path in tools:
                _file = os.path.basename(url)
                self.log.info("Downloading %s from %s", _file, url)
                try:
                    downloader.get(url, path, reporthook=pbar.download_callback)
                except KeyboardInterrupt:
                    raise
                except BaseException as exc:
                    self.log.debug("Error details: %s", traceback.format_exc())
                    raise TaurusNetworkError("Error while downloading %s: %s" % (_file, exc))

    def __install_plugins_manager(self, plugins_manager_path):
        installer = "org.jmeterplugins.repository.PluginManagerCMDInstaller"
        cmd_line = ["java", "-cp", plugins_manager_path, installer]
        self.log.debug("Trying: %s", cmd_line)
        try:
            out, err = self.call(cmd_line)
        except CALL_PROBLEMS as exc:
            raise ToolError("Failed to install PluginsManager: %s" % exc)

        self.log.debug("Install PluginsManager: %s / %s", out, err)

    def __install_plugins(self, plugins_manager_cmd):
        plugin_str = ",".join(self.plugins)
        self.log.info("Installing JMeter plugins: %s", plugin_str)
        cmd_line = [plugins_manager_cmd, 'install', plugin_str]
        self.log.debug("Trying: %s", cmd_line)

        try:
            out, err = self.call(cmd_line)
        except CALL_PROBLEMS as exc:
            raise ToolError("Failed to install plugins %s: %s" % (plugin_str, exc))

        self.log.debug("Install plugins: %s / %s", out, err)

        if out and "Plugins manager will apply some modifications" in out:
            time.sleep(5)  # allow for modifications to complete

    def _pmgr_path(self):
        dest = get_full_path(self.tool_path, step_up=2)
        return os.path.join(dest, 'bin', 'PluginsManagerCMD' + EXE_SUFFIX)

    def install(self):
        dest = get_full_path(self.tool_path, step_up=2)
        self.log.info("Will install %s into %s", self.tool_name, dest)
        plugins_manager_name = os.path.basename(self.PLUGINS_MANAGER)
        cmdrunner_name = os.path.basename(self.CMDRUNNER)
        plugins_manager_path = os.path.join(dest, 'lib', 'ext', plugins_manager_name)
        cmdrunner_path = os.path.join(dest, 'lib', cmdrunner_name)
        direct_install_tools = [  # source link and destination
            [self.PLUGINS_MANAGER, plugins_manager_path],
            [self.CMDRUNNER, cmdrunner_path]]
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
    MIRRORS_SOURCE = "https://jmeter.apache.org/download_jmeter.cgi"
    DOWNLOAD_LINK = "https://archive.apache.org/dist/jmeter/binaries/apache-jmeter-{version}.zip"

    def __init__(self, http_client, parent_logger, jmeter_version):
        self.jmeter_version = jmeter_version
        super(JMeterMirrorsManager, self).__init__(http_client, self.MIRRORS_SOURCE, parent_logger)

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
        links.append(self.DOWNLOAD_LINK.format(version=self.jmeter_version))
        self.log.debug('Total mirrors: %d', len(links))
        # place HTTPS links first, preserving the order of HTTP links
        sorted_links = sorted(links, key=lambda l: l.startswith("https"), reverse=True)
        return sorted_links
