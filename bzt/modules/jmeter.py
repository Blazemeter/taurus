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
import csv
import fnmatch
import json
import mimetypes
import os
import re
import socket
import subprocess
import tempfile
import time
import traceback
from collections import Counter, namedtuple
from distutils.version import LooseVersion
from math import ceil

from cssselect import GenericTranslator

from bzt import TaurusConfigError, ToolError, TaurusInternalException, TaurusNetworkError
from bzt.engine import ScenarioExecutor, Scenario, FileLister, Request, HTTPRequest
from bzt.jmx import JMX
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader, DataPoint, KPISet
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.modules.functional import FunctionalAggregator, FunctionalResultsReader, FunctionalSample
from bzt.modules.services import HavingInstallableTools
from bzt.six import iteritems, string_types, StringIO, etree, binary_type, parse, unicode_decode
from bzt.utils import get_full_path, EXE_SUFFIX, MirrorsManager, ExceptionalDownloader, get_uniq_name
from bzt.utils import shell_exec, ensure_is_dict, dehumanize_time, BetterDict, guess_csv_dialect
from bzt.utils import unzip, RequiredTool, JavaVM, shutdown_process, ProgressBarContext, TclLibrary


class JMeterExecutor(ScenarioExecutor, WidgetProvider, FileLister, HavingInstallableTools):
    """
    JMeter executor module

    :type modified_jmx: str
    :type jmeter_log: str
    :type properties_file: str
    :type sys_properties_file: str
    """
    MIRRORS_SOURCE = "https://jmeter.apache.org/download_jmeter.cgi"
    JMETER_DOWNLOAD_LINK = "https://archive.apache.org/dist/jmeter/binaries/apache-jmeter-{version}.zip"
    PLUGINS_MANAGER = 'https://search.maven.org/remotecontent?filepath=' \
                      'kg/apc/jmeter-plugins-manager/0.11/jmeter-plugins-manager-0.11.jar'
    CMDRUNNER = 'https://search.maven.org/remotecontent?filepath=kg/apc/cmdrunner/2.0/cmdrunner-2.0.jar'
    JMETER_VER = "3.1"
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
        self._env = {}
        self.resource_files_collector = None

    def prepare(self):
        """
        Preparation for JMeter involves either getting existing JMX
        and modifying it, or generating new JMX from input data. Then,
        original JMX is modified to contain JTL writing classes with
        required settings and have workload as suggested by Provisioning

        :raise TaurusConfigError:
        """
        self.jmeter_log = self.engine.create_artifact("jmeter", ".log")
        self._set_remote_port()
        self.install_required_tools()
        self.distributed_servers = self.execution.get('distributed', self.distributed_servers)
        scenario = self.get_scenario()

        is_jmx_generated = False

        if Scenario.SCRIPT in scenario and scenario[Scenario.SCRIPT]:
            self.original_jmx = self.get_script_path()
        elif scenario.get("requests"):
            self.original_jmx = self.__jmx_from_requests()
            is_jmx_generated = True
        else:
            raise TaurusConfigError("You must specify either a JMX file or list of requests to run JMeter")

        load = self.get_load()

        modified = self.__get_modified_jmx(self.original_jmx, load)
        self.modified_jmx = self.__save_modified_jmx(modified, self.original_jmx, is_jmx_generated)

        self.__set_jmeter_properties(scenario)
        self.__set_system_properties()
        self.__set_jvm_properties()

        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.reader = JTLReader(self.kpi_jtl, self.log, self.log_jtl)
            self.reader.is_distributed = len(self.distributed_servers) > 0
            self.engine.aggregator.add_underling(self.reader)
        elif isinstance(self.engine.aggregator, FunctionalAggregator):
            self.reader = FuncJTLReader(self.log_jtl, self.log)
            self.reader.is_distributed = len(self.distributed_servers) > 0
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
            jvm_args = os.environ.get("JVM_ARGS", "")
            if jvm_args:
                jvm_args += ' '
            self._env["JVM_ARGS"] = jvm_args + "-Xmx%s" % heap_size

    def __set_jmeter_properties(self, scenario):
        props = self.settings.get("properties")
        props_local = scenario.get("properties")
        if self.distributed_servers and self.settings.get("gui", False):
            props_local.merge({"remote_hosts": ",".join(self.distributed_servers)})
        props_local.update({"jmeterengine.nongui.port": self.management_port})
        props_local.update({"jmeterengine.nongui.maxport": self.management_port})
        props_local.update({"jmeter.save.saveservice.timestamp_format": "ms"})
        props_local.update({"sampleresult.default.encoding": "UTF-8"})
        props.merge(props_local)
        user_cp = self.engine.artifacts_dir
        if 'user.classpath' in props:
            user_cp += os.pathsep + props['user.classpath']

        props['user.classpath'] = user_cp.replace(os.path.sep, "/")  # replace to avoid Windows issue
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

        if self.sys_properties_file:
            cmdline += ["-S", os.path.abspath(self.sys_properties_file)]
        if self.distributed_servers and not self.settings.get("gui", False):
            cmdline += ['-R%s' % ','.join(self.distributed_servers)]

        self.start_time = time.time()
        try:
            # FIXME: muting stderr and stdout is bad
            self.process = self.execute(cmdline, stderr=None, env=self._env)
        except BaseException as exc:
            ToolError("%s\nFailed to start JMeter: %s" % (cmdline, exc))

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
                raise ToolError("JMeter exited with non-zero code: %s" % self.retcode)

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
    def __apply_ramp_up(jmx, ramp_up):
        """
        Apply ramp up period in seconds to ThreadGroup.ramp_time
        :param jmx: JMX
        :param ramp_up: int ramp_up period
        :return:
        """
        rampup_sel = ".//*[@name='ThreadGroup.ramp_time']"

        for group in jmx.enabled_thread_groups():
            prop = group.find(rampup_sel)
            prop.text = str(ramp_up)

    @staticmethod
    def __apply_stepping_ramp_up(jmx, load):
        """
        Change all thread groups to step groups, use ramp-up/steps
        :param jmx: JMX
        :param load: load
        :return:
        """
        step_time = int(load.ramp_up / load.steps)
        thread_groups = jmx.tree.findall(".//ThreadGroup")
        for thread_group in thread_groups:
            thread_cnc = int(thread_group.find(".//*[@name='ThreadGroup.num_threads']").text)
            tg_name = thread_group.attrib["testname"]
            thread_step = int(ceil(float(thread_cnc) / load.steps))
            step_group = JMX.get_stepping_thread_group(thread_cnc, thread_step, step_time, load.hold + step_time,
                                                       tg_name)
            thread_group.getparent().replace(thread_group, step_group)

    @staticmethod
    def __apply_duration(jmx, duration):
        """
        Apply duration to ThreadGroup.duration
        :param jmx: JMX
        :param duration: int
        :return:
        """
        sched_sel = "[name='ThreadGroup.scheduler']"
        sched_xpath = GenericTranslator().css_to_xpath(sched_sel)
        dur_sel = "[name='ThreadGroup.duration']"
        dur_xpath = GenericTranslator().css_to_xpath(dur_sel)

        for group in jmx.enabled_thread_groups():
            group.xpath(sched_xpath)[0].text = 'true'
            group.xpath(dur_xpath)[0].text = str(int(duration))
            loops_element = group.find(".//elementProp[@name='ThreadGroup.main_controller']")
            loops_loop_count = loops_element.find("*[@name='LoopController.loops']")
            loops_loop_count.getparent().replace(loops_loop_count, JMX.int_prop("LoopController.loops", -1))

    @staticmethod
    def __apply_iterations(jmx, iterations):
        """
        Apply iterations to LoopController.loops
        :param jmx: JMX
        :param iterations: int
        :return:
        """
        sel = "elementProp>[name='LoopController.loops']"
        xpath = GenericTranslator().css_to_xpath(sel)

        flag_sel = "elementProp>[name='LoopController.continue_forever']"
        flag_xpath = GenericTranslator().css_to_xpath(flag_sel)

        for group in jmx.enabled_thread_groups():
            sprop = group.xpath(xpath)
            bprop = group.xpath(flag_xpath)
            if iterations:
                bprop[0].text = 'false'
                sprop[0].text = str(iterations)

    def __apply_concurrency(self, jmx, concurrency):
        """
        Apply concurrency to ThreadGroup.num_threads
        :param jmx: JMX
        :param concurrency: int
        :return:
        """
        # TODO: what to do when they used non-standard thread groups?
        tnum_sel = ".//*[@name='ThreadGroup.num_threads']"

        orig_sum = 0.0
        for group in jmx.enabled_thread_groups():
            othread = group.find(tnum_sel)
            orig_sum += int(othread.text)
        self.log.debug("Original threads: %s", orig_sum)
        leftover = concurrency
        for group in jmx.enabled_thread_groups():
            othread = group.find(tnum_sel)
            orig = int(othread.text)
            new = int(round(concurrency * orig / orig_sum))
            leftover -= new
            othread.text = str(new)
        if leftover < 0:
            msg = "Had to add %s more threads to maintain thread group proportion"
            self.log.warning(msg, -leftover)
        elif leftover > 0:
            msg = "%s threads left undistributed due to thread group proportion"
            self.log.warning(msg, leftover)

    def __convert_to_normal_tg(self, jmx, load):
        """
        Convert all TGs to simple ThreadGroup
        :param jmx: JMX
        :param load:
        :return:
        """
        if load.iterations or load.concurrency or load.duration:
            for group in jmx.enabled_thread_groups(all_types=True):
                if group.tag != 'ThreadGroup':
                    testname = group.get('testname')
                    self.log.warning("Converting %s (%s) to normal ThreadGroup", group.tag, testname)
                    group_concurrency = JMeterExecutor.__get_concurrency_from_tg(group)
                    on_error = JMeterExecutor.__get_tg_action_on_error(group)
                    if group_concurrency:
                        new_group = JMX.get_thread_group(group_concurrency, 0, -1, testname, on_error)
                    else:
                        new_group = JMX.get_thread_group(1, 0, -1, testname, on_error)
                    group.getparent().replace(group, new_group)

    @staticmethod
    def __get_concurrency_from_tg(thread_group):
        """
        :param thread_group: etree.Element
        :return:
        """
        concurrency_element = thread_group.find(".//stringProp[@name='ThreadGroup.num_threads']")
        if concurrency_element is not None:
            return int(concurrency_element.text)

    @staticmethod
    def __get_tg_action_on_error(thread_group):
        action = thread_group.find(".//stringProp[@name='ThreadGroup.on_sample_error']")
        if action is not None:
            return action.text

    def __add_shaper(self, jmx, load):
        """
        Add shaper
        :param jmx: JMX
        :param load: namedtuple("LoadSpec",
                         ('concurrency', "throughput", 'ramp_up', 'hold', 'iterations', 'duration'))
        :return:
        """
        if not load.duration:
            self.log.warning("You must set 'ramp-up' and/or 'hold-for' when using 'throughput' option")
            return

        etree_shaper = jmx.get_rps_shaper()
        if load.ramp_up:
            jmx.add_rps_shaper_schedule(etree_shaper, 1, load.throughput, load.ramp_up)

        if load.hold:
            jmx.add_rps_shaper_schedule(etree_shaper, load.throughput, load.throughput, load.hold)

        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree_shaper)
        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree.Element("hashTree"))

    def __add_stepping_shaper(self, jmx, load):
        """
        adds stepping shaper
        1) warning if any ThroughputTimer found
        2) add VariableThroughputTimer to test plan
        :param jmx: JMX
        :param load: load
        :return:
        """
        if not load.ramp_up:
            self.log.warning("You should set up 'ramp-up' for usage of 'steps'")
            return

        timers_patterns = ["ConstantThroughputTimer", "kg.apc.jmeter.timers.VariableThroughputTimer"]

        for timer_pattern in timers_patterns:
            for timer in jmx.tree.findall(".//%s" % timer_pattern):
                self.log.warning("Test plan already use %s", timer.attrib['testname'])

        step_rps = int(round(float(load.throughput) / load.steps))
        step_time = int(round(float(load.ramp_up) / load.steps))
        step_shaper = jmx.get_rps_shaper()

        for step in range(1, int(load.steps + 1)):
            step_load = step * step_rps
            if step != load.steps:
                jmx.add_rps_shaper_schedule(step_shaper, step_load, step_load, step_time)
            else:
                if load.hold:
                    jmx.add_rps_shaper_schedule(step_shaper, step_load, step_load, step_time + load.hold)

        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, step_shaper)
        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree.Element("hashTree"))

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

    def __apply_load_settings(self, jmx, load):
        self.__convert_to_normal_tg(jmx, load)
        if load.concurrency:
            self.__apply_concurrency(jmx, load.concurrency)
        if load.hold or (load.ramp_up and not load.iterations):
            JMeterExecutor.__apply_duration(jmx, int(load.duration))
        if load.iterations:
            JMeterExecutor.__apply_iterations(jmx, int(load.iterations))
        if load.ramp_up:
            JMeterExecutor.__apply_ramp_up(jmx, int(load.ramp_up))
            if load.steps:
                JMeterExecutor.__apply_stepping_ramp_up(jmx, load)
        if load.throughput:
            if load.steps:
                self.__add_stepping_shaper(jmx, load)
            else:
                self.__add_shaper(jmx, load)


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

        jtl_log_level = self.execution.get('write-xml-jtl', 'error')

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

    def __get_modified_jmx(self, original, load):
        """
        add two listeners to test plan:
            - to collect basic stats for KPIs
            - to collect detailed errors/trace info
        :return: path to artifact
        """
        self.log.debug("Load: %s", load)
        jmx = JMX(original)

        if self.get_scenario().get("disable-listeners", True):
            JMeterExecutor.__disable_listeners(jmx)

        user_def_vars = self.get_scenario().get("variables")
        if user_def_vars:
            jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, jmx.add_user_def_vars_elements(user_def_vars))
            jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree.Element("hashTree"))

        self.__apply_modifications(jmx)

        self.__apply_test_mode(jmx)
        self.__apply_load_settings(jmx, load)
        self.__add_result_listeners(jmx)
        self.__force_tran_parent_sample(jmx)
        self.__fill_empty_delimiters(jmx)

        return jmx

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
                self.execution.get('files', []).extend(resource_files_from_jmx)
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

                if resource_element.text and not parent_disabled:
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
        requests_parser = RequestsParser(self.engine)
        requests = requests_parser.extract_requests(scenario)
        for req in requests:
            files.extend(self.res_files_from_request(req))
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
        required_tools = [JavaVM("", "", self.log), TclLibrary(self.log)]
        for tool in required_tools:
            if not tool.check_if_installed():
                tool.install()

        jmeter_path = self.settings.get("path", "~/.bzt/jmeter-taurus/")
        jmeter_path = get_full_path(jmeter_path)
        jmeter_version = self.settings.get("version", JMeterExecutor.JMETER_VER)
        download_link = self.settings.get("download-link", None)
        plugins = self.settings.get("plugins", [])
        proxy = self.engine.config.get('settings').get('proxy')
        tool = JMeter(jmeter_path, self.log, jmeter_version, download_link, plugins, proxy)

        if self._need_to_install(tool):
            tool.install()

        self.settings['path'] = tool.tool_path

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


class JTLReader(ResultsReader):
    """
    Class to read KPI JTL
    :type errors_reader: JTLErrorsReader
    """

    def __init__(self, filename, parent_logger, errors_filename):
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
            self.errors_reader.read_file()

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

    def __init__(self, filename, parent_logger):
        super(FuncJTLReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.parser = etree.XMLPullParser(events=('end',))
        self.offset = 0
        self.filename = filename
        self.fds = None
        self.failed_processing = False
        self.read_records = 0

    def __del__(self):
        if self.fds:
            self.fds.close()

    def read(self, last_pass=True):
        """
        Read the next part of the file
        """

        if self.failed_processing:
            return

        if not self.fds:
            if os.path.exists(self.filename) and os.path.getsize(self.filename):
                self.log.debug("Opening %s", self.filename)
                self.fds = open(self.filename, 'rb')
            else:
                self.log.debug("File not exists: %s", self.filename)
                return

        self.fds.seek(self.offset)
        while True:
            read = self.fds.read(1024 * 1024)
            if read.strip():
                try:
                    self.parser.feed(read)
                except etree.XMLSyntaxError as exc:
                    self.failed_processing = True
                    self.log.debug("Error reading trace.jtl: %s", traceback.format_exc())
                    self.log.warning("Failed to parse errors XML: %s", exc)
            else:
                break
            if not last_pass:
                continue
        self.offset = self.fds.tell()

        for _, elem in self.parser.read_events():
            if elem.getparent() is not None and elem.getparent().tag == 'testResults':
                sample = self.__extract_sample(elem)
                self.read_records += 1

                elem.clear()
                while elem.getprevious() is not None:
                    del elem.getparent()[0]

                yield sample

    def __extract_sample(self, elem):
        tstmp = int(float(elem.get("ts")) / 1000)
        label = elem.get("lb")
        suite_name = "JMeter"
        duration = float(elem.get("t")) / 1000.0
        success = elem.get("s") == "true"

        if success:
            status = "PASSED"
            error_msg = ""
            error_trace = ""
        else:
            assertion = self.__get_failed_assertion(elem)
            if assertion is not None:
                status = "FAILED"
                error_msg = assertion.find("failureMessage").text
                error_trace = ""
            else:
                status = "BROKEN"
                error_msg, error_trace = self.get_failure(elem)

        if error_msg.startswith("The operation lasted too long"):
            error_msg = "The operation lasted too long"

        return FunctionalSample(test_case=label, test_suite=suite_name, status=status,
                                start_time=tstmp, duration=duration,
                                error_msg=error_msg, error_trace=error_trace,
                                extras=None)

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
        self.csv_reader = csv.DictReader(self.buffer, [])
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.indexes = {}
        self.partial_buffer = ""
        self.offset = 0
        self.filename = filename
        self.fds = None
        self.read_speed = 1024 * 1024

    def read(self, last_pass=False):
        """
        read data from jtl
        yield csv row
        :type last_pass: bool
        """
        if not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            return

        self.log.debug("Reading JTL: %s", self.filename)
        self.fds.seek(self.offset)  # without this we have stuck reads on Mac

        if last_pass:
            lines = self.fds.readlines()  # unlimited
        else:
            lines = self.fds.readlines(int(self.read_speed))
        self.offset = self.fds.tell()
        bytes_read = sum(len(line) for line in lines)
        self.log.debug("Read lines: %s / %s bytes (at speed %s)", len(lines), bytes_read, self.read_speed)
        if bytes_read >= self.read_speed:
            self.read_speed = min(8 * 1024 * 1024, self.read_speed * 2)
        elif bytes_read < self.read_speed / 2:
            self.read_speed = max(self.read_speed / 2, 1024 * 1024)

        for line in lines:
            if not line.endswith("\n"):
                self.partial_buffer += line
                continue

            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""

            if not self.csv_reader.fieldnames:
                self.csv_reader.dialect = guess_csv_dialect(line)
                self.csv_reader.fieldnames += line.strip().split(self.csv_reader.dialect.delimiter)
                self.log.debug("Analyzed header line: %s", self.csv_reader.fieldnames)
                continue

            self.buffer.write(line)

        self.buffer.seek(0)
        for row in self.csv_reader:
            yield row
        self.buffer.seek(0)
        self.buffer.truncate(0)

    def __open_fds(self):
        """
        Opens JTL file for reading
        """
        if not os.path.isfile(self.filename):
            self.log.debug("File not appeared yet: %s", self.filename)
            return False

        fsize = os.path.getsize(self.filename)
        if not fsize:
            self.log.debug("File is empty: %s", self.filename)
            return False

        if fsize <= self.offset:
            self.log.debug("Waiting file to grow larget than %s, current: %s", self.offset, fsize)
            return False

        self.log.debug("Opening file: %s", self.filename)
        self.fds = open(self.filename)
        self.fds.seek(self.offset)
        return True

    def __del__(self):
        if self.fds:
            self.fds.close()


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
        # context = etree.iterparse(self.fds, events=('end',))
        self.offset = 0
        self.filename = filename
        self.fds = None
        self.buffer = BetterDict()
        self.failed_processing = False

    def __del__(self):
        if self.fds:
            self.fds.close()

    def read_file(self):
        """
        Read the next part of the file
        """

        if self.failed_processing:
            return

        if not self.fds:
            if os.path.exists(self.filename) and os.path.getsize(self.filename):  # getsize check to not stuck on mac
                self.log.debug("Opening %s", self.filename)
                self.fds = open(self.filename, 'rb')
            else:
                self.log.debug("File not exists: %s", self.filename)
                return

        self.fds.seek(self.offset)
        read = self.fds.read(1024 * 1024)
        if read.strip():
            try:
                self.parser.feed(read)  # "Huge input lookup" error without capping :)
            except etree.XMLSyntaxError as exc:
                self.failed_processing = True
                self.log.debug("Error reading errors.jtl: %s", traceback.format_exc())
                self.log.warning("Failed to parse errors XML: %s", exc)

        self.offset = self.fds.tell()
        for _action, elem in self.parser.read_events():
            del _action
            if elem.getparent() is not None and elem.getparent().tag == 'testResults':
                if elem.get('s'):
                    result = elem.get('s')
                else:
                    result = elem.xpath('success')[0].text
                if result == 'false':
                    if elem.items():
                        self.__extract_standard(elem)
                    else:
                        self.__extract_nonstandard(elem)

                # cleanup processed from the memory
                elem.clear()
                while elem.getprevious() is not None:
                    del elem.getparent()[0]

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

    def __extract_standard(self, elem):
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

    def __extract_nonstandard(self, elem):
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


class JMeterScenarioBuilder(JMX):
    """
    Helper to build JMeter test plan from Scenario

    :param executor: ScenarioExecutor
    :param original: inherited from JMX
    """

    def __init__(self, executor, original=None):
        super(JMeterScenarioBuilder, self).__init__(original)
        self.executor = executor
        self.scenario = executor.get_scenario()
        self.engine = executor.engine
        self.system_props = BetterDict()
        self.request_compiler = None

    def __gen_managers(self, scenario):
        elements = []
        headers = scenario.get_headers()
        if headers:
            elements.append(self._get_header_mgr(headers))
            elements.append(etree.Element("hashTree"))
        if scenario.get("store-cache", True):
            elements.append(self._get_cache_mgr())
            elements.append(etree.Element("hashTree"))
        if scenario.get("store-cookie", True):
            elements.append(self._get_cookie_mgr())
            elements.append(etree.Element("hashTree"))
        if scenario.get("use-dns-cache-mgr", True):
            elements.append(self.get_dns_cache_mgr())
            elements.append(etree.Element("hashTree"))
            self.system_props.merge({"system-properties": {"sun.net.inetaddr.ttl": 0}})
        return elements

    @staticmethod
    def smart_time(any_time):
        try:
            smart_time = int(1000 * dehumanize_time(any_time))
        except TaurusInternalException:
            smart_time = any_time

        return smart_time

    def __gen_defaults(self, scenario):
        default_address = scenario.get("default-address", None)
        retrieve_resources = scenario.get("retrieve-resources", True)
        resources_regex = scenario.get("retrieve-resources-regex", None)
        concurrent_pool_size = scenario.get("concurrent-pool-size", 4)
        content_encoding = scenario.get("content-encoding", None)

        timeout = scenario.get("timeout", None)
        timeout = self.smart_time(timeout)
        elements = [self._get_http_defaults(default_address, timeout, retrieve_resources,
                                            concurrent_pool_size, content_encoding, resources_regex),
                    etree.Element("hashTree")]
        return elements

    def __add_think_time(self, children, req):
        global_ttime = self.scenario.get("think-time", None)
        if req.think_time is not None:
            ttime = self.smart_time(req.think_time)
        elif global_ttime is not None:
            ttime = self.smart_time(global_ttime)
        else:
            ttime = None
        if ttime is not None:
            children.append(JMX._get_constant_timer(ttime))
            children.append(etree.Element("hashTree"))

    def __add_extractors(self, children, req):
        extractors = req.config.get("extract-regexp", BetterDict())
        for varname in extractors:
            cfg = ensure_is_dict(extractors, varname, "regexp")
            extractor = JMX._get_extractor(varname, cfg.get('subject', 'body'), cfg['regexp'], cfg.get('template', 1),
                                           cfg.get('match-no', 1), cfg.get('default', 'NOT_FOUND'))
            children.append(extractor)
            children.append(etree.Element("hashTree"))

        jextractors = req.config.get("extract-jsonpath", BetterDict())
        for varname in jextractors:
            cfg = ensure_is_dict(jextractors, varname, "jsonpath")
            children.append(JMX._get_json_extractor(varname, cfg['jsonpath'], cfg.get('default', 'NOT_FOUND')))
            children.append(etree.Element("hashTree"))

        css_jquery_extors = req.config.get("extract-css-jquery", BetterDict())
        for varname in css_jquery_extors:
            cfg = ensure_is_dict(css_jquery_extors, varname, "expression")
            extractor = self._get_jquerycss_extractor(varname, cfg['expression'], cfg.get('attribute', ""),
                                                      cfg.get('match-no', 0), cfg.get('default', 'NOT_FOUND'))
            children.append(extractor)
            children.append(etree.Element("hashTree"))

        xpath_extractors = req.config.get("extract-xpath", BetterDict())
        for varname in xpath_extractors:
            cfg = ensure_is_dict(xpath_extractors, varname, "xpath")
            children.append(JMX._get_xpath_extractor(varname,
                                                     cfg['xpath'],
                                                     cfg.get('default', 'NOT_FOUND'),
                                                     cfg.get('validate-xml', False),
                                                     cfg.get('ignore-whitespace', True),
                                                     cfg.get('use-tolerant-parser', False)))
            children.append(etree.Element("hashTree"))

    def __add_assertions(self, children, req):
        assertions = req.config.get("assert", [])
        for idx, assertion in enumerate(assertions):
            assertion = ensure_is_dict(assertions, idx, "contains")
            if not isinstance(assertion['contains'], list):
                assertion['contains'] = [assertion['contains']]
            children.append(JMX._get_resp_assertion(assertion.get("subject", Scenario.FIELD_BODY),
                                                    assertion['contains'],
                                                    assertion.get('regexp', True),
                                                    assertion.get('not', False),
                                                    assertion.get('assume-success', False)), )
            children.append(etree.Element("hashTree"))

        jpath_assertions = req.config.get("assert-jsonpath", [])
        for idx, assertion in enumerate(jpath_assertions):
            assertion = ensure_is_dict(jpath_assertions, idx, "jsonpath")

            component = JMX._get_json_path_assertion(assertion['jsonpath'], assertion.get('expected-value', ''),
                                                     assertion.get('validate', False),
                                                     assertion.get('expect-null', False),
                                                     assertion.get('invert', False), )
            children.append(component)
            children.append(etree.Element("hashTree"))

        xpath_assertions = req.config.get("assert-xpath", [])
        for idx, assertion in enumerate(xpath_assertions):
            assertion = ensure_is_dict(xpath_assertions, idx, "xpath")

            component = JMX._get_xpath_assertion(assertion['xpath'],
                                                 assertion.get('validate-xml', False),
                                                 assertion.get('ignore-whitespace', True),
                                                 assertion.get('use-tolerant-parser', False),
                                                 assertion.get('invert', False))
            children.append(component)
            children.append(etree.Element("hashTree"))

    def __add_jsr_elements(self, children, req):
        jsrs = req.config.get("jsr223", None)
        if not jsrs:
            return
        if isinstance(jsrs, dict):
            jsrs = [jsrs]
        for jsr in jsrs:
            lang = jsr.get("language", TaurusConfigError("jsr223 element should specify 'language'"))
            script = jsr.get("script-file", TaurusConfigError("jsr223 element should specify 'script-file'"))
            parameters = jsr.get("parameters", "")
            execute = jsr.get("execute", "after")
            children.append(JMX._get_jsr223_element(lang, script, parameters, execute))
            children.append(etree.Element("hashTree"))

    def _get_merged_ci_headers(self, req, header):
        def dic_lower(dic):
            return {k.lower(): dic[k].lower() for k in dic}

        ci_scenario_headers = dic_lower(self.scenario.get_headers())
        ci_request_headers = dic_lower(req.headers)
        headers = BetterDict()
        headers.merge(ci_scenario_headers)
        headers.merge(ci_request_headers)
        if header.lower() in headers:
            return headers[header]
        else:
            return None

    def __gen_requests(self, scenario):
        requests_parser = RequestsParser(self.engine)
        requests = list(requests_parser.extract_requests(scenario))
        elements = []
        for compiled in self.compile_requests(requests):
            elements.extend(compiled)
        return elements

    def compile_scenario(self, scenario):
        elements = []
        elements.extend(self.__gen_managers(scenario))
        elements.extend(self.__gen_defaults(scenario))
        elements.extend(self.__gen_datasources(scenario))
        elements.extend(self.__gen_requests(scenario))
        return elements

    def compile_http_request(self, request):
        """

        :type request: HierarchicHTTPRequest
        :return:
        """
        global_timeout = self.scenario.get("timeout", None)
        global_keepalive = self.scenario.get("keepalive", True)
        global_follow_redirects = self.scenario.get("follow-redirects", True)

        if request.timeout is not None:
            timeout = self.smart_time(request.timeout)
        elif global_timeout is not None:
            timeout = self.smart_time(global_timeout)
        else:
            timeout = None

        follow_redirects = request.follow_redirects
        if follow_redirects is None:
            follow_redirects = global_follow_redirects

        content_type = self._get_merged_ci_headers(request, 'content-type')
        if content_type == 'application/json' and isinstance(request.body, dict):
            body = json.dumps(request.body)
        else:
            body = request.body

        http = JMX._get_http_request(request.url, request.label, request.method, timeout, body, global_keepalive,
                                     request.upload_files, request.content_encoding, follow_redirects)

        children = etree.Element("hashTree")

        if request.headers:
            children.append(JMX._get_header_mgr(request.headers))
            children.append(etree.Element("hashTree"))

        self.__add_think_time(children, request)

        self.__add_assertions(children, request)

        if timeout is not None:
            children.append(JMX._get_dur_assertion(timeout))
            children.append(etree.Element("hashTree"))

        self.__add_extractors(children, request)

        self.__add_jsr_elements(children, request)

        return [http, children]

    def compile_if_block(self, block):
        elements = []

        # TODO: pass jmeter IfController options
        if_controller = JMX._get_if_controller(block.condition)
        then_children = etree.Element("hashTree")
        for compiled in self.compile_requests(block.then_clause):
            for element in compiled:
                then_children.append(element)
        elements.extend([if_controller, then_children])

        if block.else_clause:
            inverted_condition = "!(" + block.condition + ")"
            else_controller = JMX._get_if_controller(inverted_condition)
            else_children = etree.Element("hashTree")
            for compiled in self.compile_requests(block.else_clause):
                for element in compiled:
                    else_children.append(element)
            elements.extend([else_controller, else_children])

        return elements

    def compile_loop_block(self, block):
        elements = []

        loop_controller = JMX._get_loop_controller(block.loops)
        children = etree.Element("hashTree")
        for compiled in self.compile_requests(block.requests):
            for element in compiled:
                children.append(element)
        elements.extend([loop_controller, children])

        return elements

    def compile_while_block(self, block):
        elements = []

        controller = JMX._get_while_controller(block.condition)
        children = etree.Element("hashTree")
        for compiled in self.compile_requests(block.requests):
            for element in compiled:
                children.append(element)
        elements.extend([controller, children])

        return elements

    def compile_foreach_block(self, block):
        """
        :type block: ForEachBlock
        """

        elements = []

        controller = JMX._get_foreach_controller(block.input_var, block.loop_var)
        children = etree.Element("hashTree")
        for compiled in self.compile_requests(block.requests):
            for element in compiled:
                children.append(element)
        elements.extend([controller, children])

        return elements

    def compile_transaction_block(self, block):
        elements = []
        controller = JMX._get_transaction_controller(block.name)
        children = etree.Element("hashTree")
        for compiled in self.compile_requests(block.requests):
            for element in compiled:
                children.append(element)
        elements.extend([controller, children])
        return elements

    def compile_include_scenario_block(self, block):
        elements = []
        controller = JMX._get_simple_controller(block.scenario_name)
        children = etree.Element("hashTree")
        scenario = self.executor.get_scenario(name=block.scenario_name)
        for element in self.compile_scenario(scenario):
            children.append(element)
        elements.extend([controller, children])
        return elements

    def compile_action_block(self, block):
        """
        :type block: ActionBlock
        :return:
        """
        actions = {
            'stop': 0,
            'pause': 1,
            'stop-now': 2,
            'continue': 3,
        }
        targets = {'current-thread': 0, 'all-threads': 2}
        action = actions[block.action]
        target = targets[block.target]
        duration = 0
        if block.duration is not None:
            duration = int(block.duration * 1000)
        test_action = JMX._get_action_block(action, target, duration)
        return [test_action, etree.Element("hashTree")]

    def compile_requests(self, requests):
        if self.request_compiler is None:
            self.request_compiler = RequestCompiler(self)
        return [self.request_compiler.visit(request) for request in requests]

    def __generate(self):
        """
        Generate the test plan
        """

        thread_group = self.get_thread_group(concurrency=1, rampup=0, iterations=-1)
        thread_group_ht = etree.Element("hashTree", type="tg")

        # NOTE: set realistic dns-cache and JVM prop by default?
        self.request_compiler = RequestCompiler(self)
        for element in self.compile_scenario(self.scenario):
            thread_group_ht.append(element)

        results_tree = self._get_results_tree()
        results_tree_ht = etree.Element("hashTree")

        self.append(self.TEST_PLAN_SEL, thread_group)
        self.append(self.TEST_PLAN_SEL, thread_group_ht)
        self.append(self.TEST_PLAN_SEL, results_tree)
        self.append(self.TEST_PLAN_SEL, results_tree_ht)

    def save(self, filename):
        """
        Generate test plan and save

        :type filename: str
        """
        # NOTE: bad design, as repetitive save will duplicate stuff
        self.__generate()
        super(JMeterScenarioBuilder, self).save(filename)

    def __gen_datasources(self, scenario):
        sources = scenario.get("data-sources", [])
        if not sources:
            return []
        if not isinstance(sources, list):
            raise TaurusConfigError("data-sources '%s' is not a list" % sources)
        elements = []
        for idx, source in enumerate(sources):
            source = ensure_is_dict(sources, idx, "path")
            source_path = source["path"]

            jmeter_var_pattern = re.compile("^\$\{.*\}$")
            delimiter = source.get('delimiter', None)

            if jmeter_var_pattern.match(source_path):
                self.log.warning('JMeter variable "%s" found, check of file existence is impossible', source_path)
                if not delimiter:
                    self.log.warning('CSV dialect detection impossible, default delimiter selected (",")')
                    delimiter = ','
            else:
                modified_path = self.executor.engine.find_file(source_path)
                if not os.path.isfile(modified_path):
                    raise TaurusConfigError("data-sources path not found: %s" % modified_path)
                if not delimiter:
                    delimiter = self.__guess_delimiter(modified_path)
                source_path = get_full_path(modified_path)

            config = JMX._get_csv_config(source_path, delimiter, source.get("quoted", False), source.get("loop", True))
            elements.append(config)
            elements.append(etree.Element("hashTree"))
        return elements

    def __guess_delimiter(self, path):
        with open(path) as fhd:
            header = fhd.read(4096)  # 4KB is enough for header
            try:
                delimiter = guess_csv_dialect(header).delimiter
            except BaseException as exc:
                self.log.debug(traceback.format_exc())
                self.log.warning('CSV dialect detection failed (%s), default delimiter selected (",")', exc)
                delimiter = ","  # default value

        return delimiter


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

    def check_if_installed(self):
        self.log.debug("Trying jmeter: %s", self.tool_path)
        try:
            with tempfile.NamedTemporaryFile(prefix="jmeter", suffix="log", delete=False) as jmlog:
                jm_proc = shell_exec([self.tool_path, '-j', jmlog.name, '--version'], stderr=subprocess.STDOUT)
                jmout, jmerr = jm_proc.communicate()
                self.log.debug("JMeter check: %s / %s", jmout, jmerr)

            os.remove(jmlog.name)

            if isinstance(jmout, binary_type):
                jmout = jmout.decode()

            if "is too low to run JMeter" in jmout:
                raise ToolError("Java version is too low to run JMeter")

            return True

        except OSError:
            self.log.debug("JMeter check failed.")
            return False

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
                except BaseException as exc:
                    raise TaurusNetworkError("Error while downloading %s: %s" % (_file, exc))

    def __install_plugins_manager(self, plugins_manager_path):
        installer = "org.jmeterplugins.repository.PluginManagerCMDInstaller"
        cmd = ["java", "-cp", plugins_manager_path, installer]
        self.log.debug("Trying: %s", cmd)
        try:
            proc = shell_exec(cmd)
            out, err = proc.communicate()
            self.log.debug("Install PluginsManager: %s / %s", out, err)
        except BaseException as exc:
            raise ToolError("Failed to install PluginsManager: %s" % exc)

    def __install_plugins(self, plugins_manager_cmd):
        plugin_str = ",".join(self.plugins)
        self.log.info("Installing JMeter plugins: %s", plugin_str)
        cmd = [plugins_manager_cmd, 'install', plugin_str]
        self.log.debug("Trying: %s", cmd)
        try:
            # prepare proxy settings
            if self.proxy_settings and self.proxy_settings.get('address'):
                env = BetterDict()
                env.merge(dict(os.environ))
                jvm_args = env.get('JVM_ARGS', '')

                proxy_url = parse.urlsplit(self.proxy_settings.get("address"))
                self.log.debug("Using proxy settings: %s", proxy_url)
                host = proxy_url.hostname
                port = proxy_url.port
                if not port:
                    port = 80

                jvm_args += ' -Dhttp.proxyHost=%s -Dhttp.proxyPort=%s' % (host, port)  # TODO: remove it after pmgr 0.9
                jvm_args += ' -Dhttps.proxyHost=%s -Dhttps.proxyPort=%s' % (host, port)

                username = self.proxy_settings.get('username')
                password = self.proxy_settings.get('password')

                if username and password:
                    # property names correspond to
                    # https://github.com/apache/jmeter/blob/trunk/src/core/org/apache/jmeter/JMeter.java#L110
                    jvm_args += ' -Dhttp.proxyUser="%s" -Dhttp.proxyPass="%s"' % (username, password)

                env['JVM_ARGS'] = jvm_args

            proc = shell_exec(cmd)
            out, err = proc.communicate()
            self.log.debug("Install plugins: %s / %s", out, err)
        except BaseException as exc:
            raise ToolError("Failed to install plugins %s: %s" % (plugin_str, exc))

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
        plugins_manager_cmd = os.path.join(dest, 'bin', 'PluginsManagerCMD' + EXE_SUFFIX)

        self.__install_jmeter(dest)
        self.__download_additions(direct_install_tools)
        self.__install_plugins_manager(plugins_manager_path)
        self.__install_plugins(plugins_manager_cmd)

        cleaner = JarCleaner(self.log)
        cleaner.clean(os.path.join(dest, 'lib'))


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


class IfBlock(Request):
    def __init__(self, condition, then_clause, else_clause, config):
        super(IfBlock, self).__init__(config)
        self.condition = condition
        self.then_clause = then_clause
        self.else_clause = else_clause

    def __repr__(self):
        then_clause = [repr(req) for req in self.then_clause]
        else_clause = [repr(req) for req in self.else_clause]
        return "IfBlock(condition=%s, then=%s, else=%s)" % (self.condition, then_clause, else_clause)


class LoopBlock(Request):
    def __init__(self, loops, requests, config):
        super(LoopBlock, self).__init__(config)
        self.loops = loops
        self.requests = requests

    def __repr__(self):
        requests = [repr(req) for req in self.requests]
        return "LoopBlock(loops=%s, requests=%s)" % (self.loops, requests)


class WhileBlock(Request):
    def __init__(self, condition, requests, config):
        super(WhileBlock, self).__init__(config)
        self.condition = condition
        self.requests = requests

    def __repr__(self):
        requests = [repr(req) for req in self.requests]
        return "WhileBlock(condition=%s, requests=%s)" % (self.condition, requests)


class ForEachBlock(Request):
    def __init__(self, input_var, loop_var, requests, config):
        super(ForEachBlock, self).__init__(config)
        self.input_var = input_var
        self.loop_var = loop_var
        self.requests = requests

    def __repr__(self):
        requests = [repr(req) for req in self.requests]
        fmt = "ForEachBlock(input=%s, loop_var=%s, requests=%s)"
        return fmt % (self.input_var, self.loop_var, requests)


class TransactionBlock(Request):
    def __init__(self, name, requests, config):
        super(TransactionBlock, self).__init__(config)
        self.name = name
        self.requests = requests

    def __repr__(self):
        requests = [repr(req) for req in self.requests]
        fmt = "TransactionBlock(name=%s, requests=%s)"
        return fmt % (self.name, requests)


class IncludeScenarioBlock(Request):
    def __init__(self, scenario_name, config):
        super(IncludeScenarioBlock, self).__init__(config)
        self.scenario_name = scenario_name


class RequestsParser(object):
    def __init__(self, engine):
        self.engine = engine

    def __parse_request(self, req):
        if 'if' in req:
            condition = req.get("if")
            # TODO: apply some checks to `condition`?
            then_clause = req.get("then", TaurusConfigError("'then' clause is mandatory for 'if' blocks"))
            then_requests = self.__parse_requests(then_clause)
            else_clause = req.get("else", [])
            else_requests = self.__parse_requests(else_clause)
            return IfBlock(condition, then_requests, else_requests, req)
        elif 'loop' in req:
            loops = req.get("loop")
            do_block = req.get("do", TaurusConfigError("'do' option is mandatory for 'loop' blocks"))
            do_requests = self.__parse_requests(do_block)
            return LoopBlock(loops, do_requests, req)
        elif 'while' in req:
            condition = req.get("while")
            do_block = req.get("do", TaurusConfigError("'do' option is mandatory for 'while' blocks"))
            do_requests = self.__parse_requests(do_block)
            return WhileBlock(condition, do_requests, req)
        elif 'foreach' in req:
            iteration_str = req.get("foreach")
            match = re.match(r'(.+) in (.+)', iteration_str)
            if not match:
                msg = "'foreach' value should be in format '<elementName> in <collection>' but '%s' found"
                raise TaurusConfigError(msg % iteration_str)
            loop_var, input_var = match.groups()
            do_block = req.get("do", TaurusConfigError("'do' field is mandatory for 'foreach' blocks"))
            do_requests = self.__parse_requests(do_block)
            return ForEachBlock(input_var, loop_var, do_requests, req)
        elif 'transaction' in req:
            name = req.get('transaction')
            do_block = req.get('do', TaurusConfigError("'do' field is mandatory for transaction blocks"))
            do_requests = self.__parse_requests(do_block)
            return TransactionBlock(name, do_requests, req)
        elif 'include-scenario' in req:
            name = req.get('include-scenario')
            return IncludeScenarioBlock(name, req)
        elif 'action' in req:
            action = req.get('action')
            if action not in ('pause', 'stop', 'stop-now', 'continue'):
                raise TaurusConfigError("Action should be either 'pause', 'stop', 'stop-now' or 'continue'")
            target = req.get('target', 'current-thread')
            if target not in ('current-thread', 'all-threads'):
                msg = "Target for action should be either 'current-thread' or 'all-threads' but '%s' found"
                raise TaurusConfigError(msg % target)
            duration = req.get('pause-duration', None)
            if duration is not None:
                duration = dehumanize_time(duration)
            return ActionBlock(action, target, duration, req)
        else:
            return HierarchicHTTPRequest(req, self.engine)

    def __parse_requests(self, raw_requests):
        requests = []
        for key in range(len(raw_requests)):  # pylint: disable=consider-using-enumerate
            if not isinstance(raw_requests[key], dict):
                req = ensure_is_dict(raw_requests, key, "url")
            else:
                req = raw_requests[key]
            requests.append(self.__parse_request(req))
        return requests

    def extract_requests(self, scenario):
        requests = scenario.get("requests", [])
        return list(self.__parse_requests(requests))


class HierarchicHTTPRequest(HTTPRequest):
    def __init__(self, config, engine):
        super(HierarchicHTTPRequest, self).__init__(config, engine)
        self.upload_files = config.get("upload-files", [])
        for file_dict in self.upload_files:
            file_dict.get("param", TaurusConfigError("Items from upload-files must specify parameter name"))
            path = file_dict.get('path', TaurusConfigError("Items from upload-files must specify path to file"))
            mime = mimetypes.guess_type(path)[0] or "application/octet-stream"
            file_dict.get('mime-type', mime)
        self.content_encoding = config.get('content-encoding', None)
        self.follow_redirects = config.get('follow-redirects', None)


class ActionBlock(Request):
    def __init__(self, action, target, duration, config):
        super(ActionBlock, self).__init__(config)
        self.action = action
        self.target = target
        self.duration = duration


class RequestVisitor(object):
    def __init__(self):
        self.path = []

    def visit(self, node):
        class_name = node.__class__.__name__.lower()
        visitor = getattr(self, 'visit_' + class_name, None)
        if visitor is not None:
            return visitor(node)
        raise TaurusInternalException("Visitor for class %s not found" % class_name)


class ResourceFilesCollector(RequestVisitor):
    def __init__(self, executor):
        """
        :param executor: JMeterExecutor
        """
        super(ResourceFilesCollector, self).__init__()
        self.executor = executor

    def visit_hierarchichttprequest(self, request):
        files = []
        body_file = request.config.get('body-file')
        if body_file:
            files.append(body_file)
        if 'jsr223' in request.config:
            jsrs = request.config.get('jsr223')
            if isinstance(jsrs, dict):
                jsrs = [jsrs]
            for jsr in jsrs:
                if 'script-file' in jsr:
                    files.append(jsr.get('script-file'))
        return files

    def visit_ifblock(self, block):
        files = []
        for request in block.then_clause:
            files.extend(self.visit(request))
        for request in block.else_clause:
            files.extend(self.visit(request))
        return files

    def visit_loopblock(self, block):
        files = []
        for request in block.requests:
            files.extend(self.visit(request))
        return files

    def visit_whileblock(self, block):
        files = []
        for request in block.requests:
            files.extend(self.visit(request))
        return files

    def visit_foreachblock(self, block):
        files = []
        for request in block.requests:
            files.extend(self.visit(request))
        return files

    def visit_transactionblock(self, block):
        files = []
        for request in block.requests:
            files.extend(self.visit(request))
        return files

    def visit_includescenarioblock(self, block):
        scenario_name = block.scenario_name
        if scenario_name in self.path:
            msg = "Mutual recursion detected in include-scenario blocks (scenario %s)"
            raise TaurusConfigError(msg % scenario_name)
        self.path.append(scenario_name)
        scenario = self.executor.get_scenario(name=block.scenario_name)
        return self.executor.res_files_from_scenario(scenario)

    def visit_actionblock(self, _):
        return []


class RequestCompiler(RequestVisitor):
    def __init__(self, jmx_builder):
        super(RequestCompiler, self).__init__()
        self.jmx_builder = jmx_builder

    def visit_hierarchichttprequest(self, request):
        return self.jmx_builder.compile_http_request(request)

    def visit_ifblock(self, block):
        return self.jmx_builder.compile_if_block(block)

    def visit_loopblock(self, block):
        return self.jmx_builder.compile_loop_block(block)

    def visit_whileblock(self, block):
        return self.jmx_builder.compile_while_block(block)

    def visit_foreachblock(self, block):
        return self.jmx_builder.compile_foreach_block(block)

    def visit_transactionblock(self, block):
        return self.jmx_builder.compile_transaction_block(block)

    def visit_includescenarioblock(self, block):
        scenario_name = block.scenario_name
        if scenario_name in self.path:
            msg = "Mutual recursion detected in include-scenario blocks (scenario %s)"
            raise TaurusConfigError(msg % scenario_name)
        self.path.append(scenario_name)
        return self.jmx_builder.compile_include_scenario_block(block)

    def visit_actionblock(self, block):
        return self.jmx_builder.compile_action_block(block)
