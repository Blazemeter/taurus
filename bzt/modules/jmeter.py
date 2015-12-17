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
import logging
import os
import re
import shutil
import socket
import subprocess
import tempfile
import time
import traceback
from collections import Counter, namedtuple
from distutils.version import LooseVersion
from itertools import chain
from math import ceil

from cssselect import GenericTranslator

from bzt.engine import ScenarioExecutor, Scenario, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader, DataPoint, KPISet
from bzt.modules.console import WidgetProvider, SidebarWidget
from bzt.six import iteritems, text_type, string_types, StringIO, parse, request, etree, binary_type
from bzt.utils import shell_exec, ensure_is_dict, dehumanize_time, BetterDict, \
    guess_csv_dialect, unzip, RequiredTool, JavaVM, shutdown_process, ProgressBarContext, TclLibrary, MirrorsManager, \
    EXE_SUFFIX


class JMeterExecutor(ScenarioExecutor, WidgetProvider, FileLister):
    """
    JMeter executor module

    :type modified_jmx: str
    :type jmeter_log: str
    :type properties_file: str
    :type sys_properties_file: str
    """
    MIRRORS_SOURCE = "http://jmeter.apache.org/download_jmeter.cgi"
    JMETER_DOWNLOAD_LINK = "http://apache.claz.org/jmeter/binaries/apache-jmeter-{version}.zip"
    JMETER_VER = "2.13"
    PLUGINS_DOWNLOAD_TPL = "http://jmeter-plugins.org/files/JMeterPlugins-{plugin}-1.3.1.zip"
    UDP_PORT_NUMBER = None

    def __init__(self):
        super(JMeterExecutor, self).__init__()
        self.original_jmx = None
        self.modified_jmx = None
        self.jmeter_log = None
        self.properties_file = None
        self.sys_properties_file = None
        self.kpi_jtl = None
        self.errors_jtl = None
        self.process = None
        self.start_time = None
        self.end_time = None
        self.retcode = None
        self.widget = None
        self.distributed_servers = []
        self.management_port = None
        self.reader = None

    def prepare(self):
        """
        Preparation for JMeter involves either getting existing JMX
        and modifying it, or generating new JMX from input data. Then,
        original JMX is modified to contain JTL writing classes with
        required settings and have workload as suggested by Provisioning

        :raise ValueError:
        """
        self.jmeter_log = self.engine.create_artifact("jmeter", ".log")
        self._set_remote_port()
        self.run_checklist()
        self.distributed_servers = self.execution.get('distributed', self.distributed_servers)
        scenario = self.get_scenario()
        self.resource_files()
        if Scenario.SCRIPT in scenario and scenario[Scenario.SCRIPT]:
            self.engine.existing_artifact(self.original_jmx)
        elif "requests" in scenario:
            if scenario.get("requests"):
                self.original_jmx = self.__jmx_from_requests()
            else:
                raise RuntimeError("Nothing to test, no requests were provided in scenario")
        else:
            raise ValueError("There must be a JMX file to run JMeter")
        load = self.get_load()
        self.modified_jmx = self.__get_modified_jmx(self.original_jmx, load)

        self.__set_jmeter_properties(scenario)
        self.__set_system_properties()

        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.reader = JTLReader(self.kpi_jtl, self.log, self.errors_jtl)
            self.reader.is_distributed = len(self.distributed_servers) > 0
            self.engine.aggregator.add_underling(self.reader)

    def __set_system_properties(self):
        sys_props = self.settings.get("system-properties")
        if sys_props:
            self.log.debug("Additional system properties %s", sys_props)
            sys_props_file = self.engine.create_artifact("system", ".properties")
            JMeterExecutor.__write_props_to_file(sys_props_file, sys_props)
            self.sys_properties_file = sys_props_file

    def __set_jmeter_properties(self, scenario):
        props = self.settings.get("properties")
        props_local = scenario.get("properties")
        if self.distributed_servers and self.settings.get("gui", False):
            props_local.merge({"remote_hosts": ",".join(self.distributed_servers)})
        props_local.update({"jmeterengine.nongui.port": self.management_port})
        props_local.update({"jmeterengine.nongui.maxport": self.management_port})
        props_local.update({"jmeter.save.saveservice.timestamp_format": "ms"})
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
            self.process = shell_exec(cmdline, stderr=None, cwd=self.engine.artifacts_dir)
        except OSError as exc:
            self.log.error("Failed to start JMeter: %s", traceback.format_exc())
            self.log.error("Failed command: %s", cmdline)
            raise RuntimeError("Failed to start JMeter: %s" % exc)

    def check(self):
        """
        Checks if JMeter is still running. Also checks if resulting JTL contains
        any data and throws exception otherwise.

        :return: bool
        :raise RuntimeWarning:
        """
        if self.widget:
            self.widget.update()

        self.retcode = self.process.poll()
        if self.retcode is not None:
            if self.retcode != 0:
                self.log.info("JMeter exit code: %s", self.retcode)
                raise RuntimeError("JMeter exited with non-zero code")

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
        if self.reader and not self.reader.buffer:
            msg = "Empty results JTL, most likely JMeter failed: %s"
            raise RuntimeWarning(msg % self.kpi_jtl)

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
                self.log.error("No free ports for management interface")
                raise RuntimeError
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
        rampup_sel = "stringProp[name='ThreadGroup.ramp_time']"
        xpath = GenericTranslator().css_to_xpath(rampup_sel)

        for group in jmx.enabled_thread_groups():
            prop = group.xpath(xpath)
            prop[0].text = str(ramp_up)

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
        tnum_sel = "stringProp[name='ThreadGroup.num_threads']"
        tnum_xpath = GenericTranslator().css_to_xpath(tnum_sel)

        orig_sum = 0.0
        for group in jmx.enabled_thread_groups():
            othreads = group.xpath(tnum_xpath)
            orig_sum += int(othreads[0].text)
        self.log.debug("Original threads: %s", orig_sum)
        leftover = concurrency
        for group in jmx.enabled_thread_groups():
            othreads = group.xpath(tnum_xpath)
            orig = int(othreads[0].text)
            new = int(round(concurrency * orig / orig_sum))
            leftover -= new
            othreads[0].text = str(new)
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
                        new_group = JMX._get_thread_group(group_concurrency, 0, -1, testname, on_error)
                    else:
                        new_group = JMX._get_thread_group(1, 0, -1, testname, on_error)
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

    @staticmethod
    def __add_shaper(jmx, load):
        """
        Add shaper
        :param jmx: JMX
        :param load: namedtuple("LoadSpec",
                         ('concurrency', "throughput", 'ramp_up', 'hold', 'iterations', 'duration'))
        :return:
        """

        if load.throughput and load.duration:
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
                JMeterExecutor.__add_shaper(jmx, load)

    def __add_result_writers(self, jmx):
        self.kpi_jtl = self.engine.create_artifact("kpi", ".jtl")
        kpil = jmx.new_kpi_listener(self.kpi_jtl)
        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, kpil)
        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree.Element("hashTree"))
        # NOTE: maybe have option not to write it, since it consumes drive space
        # TODO: option to enable full trace JTL for all requests
        self.errors_jtl = self.engine.create_artifact("errors", ".jtl")
        errs = jmx.new_errors_listener(self.errors_jtl)
        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, errs)
        jmx.append(JMeterScenarioBuilder.TEST_PLAN_SEL, etree.Element("hashTree"))

    def __prepare_resources(self, jmx):
        resource_files_from_jmx = JMeterExecutor.__get_resource_files_from_jmx(jmx)
        resource_files_from_requests = self.__get_res_files_from_requests()
        self.__cp_res_files_to_artifacts_dir(resource_files_from_jmx)
        self.__cp_res_files_to_artifacts_dir(resource_files_from_requests)
        if resource_files_from_jmx and not self.distributed_servers:
            self.__modify_resources_paths_in_jmx(jmx.tree, resource_files_from_jmx)

    def __get_modified_jmx(self, original, load):
        """
        add two listeners to test plan:
            - to collect basic stats for KPIs
            - to collect detailed errors info
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

        rename_threads = self.settings.get("rename-distributed-threads", True)
        if self.distributed_servers and rename_threads:
            self.__rename_thread_groups(jmx)

        self.__apply_load_settings(jmx, load)
        self.__prepare_resources(jmx)
        self.__add_result_writers(jmx)

        prefix = "modified_" + os.path.basename(original)
        filename = self.engine.create_artifact(prefix, ".jmx")
        jmx.save(filename)
        return filename

    def __jmx_from_requests(self):
        """
        Generate jmx file from requests
        :return:
        """
        filename = self.engine.create_artifact("requests", ".jmx")
        jmx = JMeterScenarioBuilder()
        jmx.scenario = self.get_scenario()
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
            self.widget = SidebarWidget(self, "JMeter: " + label.split('/')[1])
        return self.widget

    def resource_files(self):
        """
        Get list of resource files, copy resource files to artifacts dir, modify jmx
        """
        resource_files = []
        # get all resource files from requests
        files_from_requests = self.__get_res_files_from_requests()
        if not self.original_jmx:
            self.original_jmx = self.__get_script()

        if self.original_jmx and os.path.exists(self.original_jmx):
            jmx = JMX(self.original_jmx)
            resource_files_from_jmx = JMeterExecutor.__get_resource_files_from_jmx(jmx)

            if resource_files_from_jmx:
                self.__modify_resources_paths_in_jmx(jmx.tree, resource_files_from_jmx)

                script_name, script_ext = os.path.splitext(self.original_jmx)
                script_name = os.path.basename(script_name)
                # create modified jmx script in artifacts dir
                modified_script = self.engine.create_artifact(script_name, script_ext)
                jmx.save(modified_script)
                resource_files.extend(resource_files_from_jmx)

        resource_files.extend(files_from_requests)
        # copy files to artifacts dir
        self.__cp_res_files_to_artifacts_dir(resource_files)
        if self.original_jmx:
            resource_files.append(self.original_jmx)
        return resource_files

    def __cp_res_files_to_artifacts_dir(self, resource_files_list):
        """

        :param resource_files_list:
        :return:
        """
        for resource_file in resource_files_list:
            if os.path.exists(resource_file):
                try:
                    shutil.copy(resource_file, self.engine.artifacts_dir)
                except BaseException:
                    self.log.warning("Cannot copy file: %s", resource_file)
            else:
                if '${' not in resource_file:
                    self.log.warning("File not found: %s", resource_file)

    def __modify_resources_paths_in_jmx(self, jmx, file_list):
        """
        Modify resource files paths in jmx etree

        :param jmx: JMX
        :param file_list: list
        :return:
        """
        for file_path in file_list:
            if os.path.exists(file_path):
                file_path_elements = jmx.xpath('//stringProp[text()="%s"]' % file_path)
                for file_path_element in file_path_elements:
                    file_path_element.text = os.path.basename(file_path)
            else:
                self.log.warning("File not found: %s", file_path)

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

                if resource_element.text and parent_disabled is False:
                    resource_files.append(resource_element.text)
        return resource_files

    def __get_res_files_from_requests(self):
        """
        Get post-body files from requests
        :return file list:
        """
        post_body_files = []
        scenario = self.get_scenario()
        data_sources = scenario.data.get('data-sources')
        if data_sources:
            for data_source in data_sources:
                if isinstance(data_source, text_type):
                    post_body_files.append(data_source)

        requests = scenario.data.get("requests")
        if requests:
            for req in requests:
                if isinstance(req, dict):
                    post_body_path = req.get('body-file')

                    if post_body_path:
                        post_body_files.append(post_body_path)
        return post_body_files

    def __rename_thread_groups(self, jmx):
        """
        In case of distributed test, rename thread groups
        :param jmx: JMX
        :return:
        """
        prepend_str = r"${__machineName()}"
        thread_groups = jmx.tree.findall(".//ThreadGroup")
        for thread_group in thread_groups:
            test_name = thread_group.attrib["testname"]
            if prepend_str not in test_name:
                thread_group.attrib["testname"] = prepend_str + test_name

        self.log.debug("ThreadGroups renamed: %d", len(thread_groups))

    def __get_script(self):
        """

        :return: script path
        """
        scenario = self.get_scenario()
        if Scenario.SCRIPT not in scenario:
            return None

        fname = scenario[Scenario.SCRIPT]
        if fname is not None:
            return self.engine.find_file(fname)
        else:
            return None

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
                    raise ValueError("Property selector must have at least 2 levels")
                sel = "[testname='%s']" % parts[0]  # TODO: support wildcards in element names
                for add in parts[1:]:
                    sel += ">[name='%s']" % add
                jmx.set_text(sel, text)

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

    def run_checklist(self):
        """
        check tools
        """
        required_tools = [JavaVM("", "", self.log), TclLibrary(self.log)]

        jmeter_path = self.settings.get("path", "~/.bzt/jmeter-taurus/bin/jmeter")
        if jmeter_path.lower().endswith(EXE_SUFFIX):
            jmeter_path += EXE_SUFFIX
        jmeter_path = os.path.abspath(os.path.expanduser(jmeter_path))
        self.settings["path"] = jmeter_path
        jmeter_version = self.settings.get("version", JMeterExecutor.JMETER_VER)

        plugin_download_link = self.settings.get("plugins-download-link", JMeterExecutor.PLUGINS_DOWNLOAD_TPL)

        required_tools.append(JMeter(jmeter_path, self.log, jmeter_version))
        required_tools.append(JMeterPlugins(jmeter_path, plugin_download_link, self.log))

        self.check_tools(required_tools)

    def check_tools(self, required_tools):
        for tool in required_tools:
            if not tool.check_if_installed():
                self.log.info("Installing %s", tool.tool_name)
                tool.install()


class JMeterJTLLoaderExecutor(ScenarioExecutor):
    """
    Executor type that just loads existing kpi.jtl and errors.jtl
    """

    def __init__(self):
        # TODO: document this executor
        super(JMeterJTLLoaderExecutor, self).__init__()
        self.kpi_jtl = None
        self.errors_jtl = None
        self.reader = None

    def prepare(self):
        self.kpi_jtl = self.execution.get("kpi-jtl", None)
        if self.kpi_jtl is None:
            raise ValueError("Option is required for executor: kpi-jtl")
        self.errors_jtl = self.execution.get("errors-jtl", None)

        self.reader = JTLReader(self.kpi_jtl, self.log, self.errors_jtl)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def check(self):
        return True


class JMX(object):
    """
    A class to manipulate and generate JMX test plans for JMeter

    :param original: path to existing JMX to load. If it is None, then creates
    empty test plan
    """

    TEST_PLAN_SEL = "jmeterTestPlan>hashTree>hashTree"
    THR_GROUP_SEL = TEST_PLAN_SEL + ">hashTree[type=tg]"
    FIELD_RESP_CODE = "http-code"
    FIELD_HEADERS = "headers"
    FIELD_BODY = "body"

    def __init__(self, original=None, test_plan_name="BZT Generated Test Plan"):
        self.log = logging.getLogger(self.__class__.__name__)
        if original:
            self.load(original)
        else:
            root = etree.Element("jmeterTestPlan")
            self.tree = etree.ElementTree(root)

            test_plan = etree.Element("TestPlan", guiclass="TestPlanGui",
                                      testname=test_plan_name,
                                      testclass="TestPlan")

            htree = etree.Element("hashTree")
            htree.append(test_plan)
            htree.append(etree.Element("hashTree"))
            self.append("jmeterTestPlan", htree)

            element_prop = self._get_arguments_panel("TestPlan.user_defined_variables")
            self.append("jmeterTestPlan>hashTree>TestPlan", element_prop)

    def load(self, original):
        """
        Load existing JMX file

        :param original: JMX file path
        :raise RuntimeError: in case of XML parsing error
        """
        try:
            self.tree = etree.ElementTree()
            self.tree.parse(original)
        except BaseException as exc:
            self.log.debug("XML parsing error: %s", traceback.format_exc())
            data = (original, exc)
            raise RuntimeError("XML parsing failed for file %s: %s" % data)

    def get(self, selector):
        """
        Returns tree elements by CSS selector

        :type selector: str
        :return:
        """
        expression = GenericTranslator().css_to_xpath(selector)
        nodes = self.tree.xpath(expression)
        return nodes

    def append(self, selector, node):
        """
        Add node to container specified by selector. If multiple nodes will
        match the selector, first of them will be used as container.

        :param selector: CSS selector for container
        :param node: Element instance to add
        :raise RuntimeError: if container was not found
        """
        container = self.get(selector)
        if not len(container):
            msg = "Failed to find TestPlan node in file: %s"
            raise RuntimeError(msg % selector)

        container[0].append(node)

    def save(self, filename):
        """
        Save JMX into file

        :param filename:
        """
        self.log.debug("Saving JMX to: %s", filename)
        with open(filename, "wb") as fhd:
            # self.log.debug("\n%s", etree.tostring(self.tree))
            self.tree.write(fhd, pretty_print=True, encoding="UTF-8", xml_declaration=True)

    def enabled_thread_groups(self, all_types=False):
        """
        Get thread groups that are enabled
        :type all_types: bool
        """
        if all_types:
            ultimate_tgroup = self.get('jmeterTestPlan>hashTree>hashTree>kg\.apc\.jmeter\.threads\.UltimateThreadGroup')
            stepping_tgroup = self.get('jmeterTestPlan>hashTree>hashTree>kg\.apc\.jmeter\.threads\.SteppingThreadGroup')
            tgroups = chain(ultimate_tgroup, stepping_tgroup)
        else:
            tgroups = self.get('jmeterTestPlan>hashTree>hashTree>ThreadGroup')

        for group in tgroups:
            if group.get("enabled") != 'false':
                yield group

    @staticmethod
    def _flag(flag_name, bool_value):
        """
        Generates element for JMX flag node

        :param flag_name:
        :param bool_value:
        :return:
        """
        elm = etree.Element(flag_name)
        elm.text = "true" if bool_value else "false"
        return elm

    @staticmethod
    def __jtl_writer(filename, label, flags):
        """
        Generates JTL writer

        :param filename:
        :return:
        """
        jtl = etree.Element("stringProp", {"name": "filename"})
        jtl.text = filename

        name = etree.Element("name")
        name.text = "saveConfig"
        value = etree.Element("value")
        value.set("class", "SampleSaveConfiguration")

        for key, val in iteritems(flags):
            value.append(JMX._flag(key, val))
        obj_prop = etree.Element("objProp")
        obj_prop.append(name)
        obj_prop.append(value)

        kpi_listener = etree.Element("ResultCollector",
                                     testname=label,
                                     testclass="ResultCollector",
                                     guiclass="SimpleDataWriter")
        kpi_listener.append(jtl)
        kpi_listener.append(obj_prop)
        return kpi_listener

    @staticmethod
    def new_kpi_listener(filename):
        """
        Generates listener for writing basic KPI data in CSV format

        :param filename:
        :return:
        """
        flags = {
            "xml": False,
            "fieldNames": True,
            "time": True,
            "timestamp": True,
            "latency": True,
            "success": True,
            "label": True,
            "code": True,
            "message": True,
            "threadName": True,
            "dataType": False,
            "encoding": False,
            "assertions": False,
            "subresults": False,
            "responseData": False,
            "samplerData": False,
            "responseHeaders": False,
            "requestHeaders": False,
            "responseDataOnError": False,
            "saveAssertionResultsFailureMessage": False,
            "bytes": False,
            "threadCounts": True,
            "url": False
        }

        return JMX.__jtl_writer(filename, "KPI Writer", flags)

    @staticmethod
    def new_errors_listener(filename):
        """

        :type filename: str
        :return:
        """
        flags = {
            "xml": True,
            "fieldNames": True,
            "time": True,
            "timestamp": True,
            "latency": True,
            "success": True,
            "label": True,
            "code": True,
            "message": True,
            "threadName": True,
            "dataType": True,
            "encoding": True,
            "assertions": True,
            "subresults": True,
            "responseData": True,
            "samplerData": True,
            "responseHeaders": True,
            "requestHeaders": True,
            "responseDataOnError": True,
            "saveAssertionResultsFailureMessage": True,
            "bytes": True,
            "threadCounts": True,
            "url": True
        }
        writer = JMX.__jtl_writer(filename, "Errors Writer", flags)
        writer.append(JMX._bool_prop("ResultCollector.error_logging", True))
        return writer

    @staticmethod
    def _get_arguments_panel(name):
        """
        Generates ArgumentsPanel node

        :param name:
        :return:
        """
        return etree.Element("elementProp", name=name, elementType="Arguments",
                             guiclass="ArgumentsPanel", testclass="Arguments")

    @staticmethod
    def _get_http_request(url, label, method, timeout, body, keepalive):
        """
        Generates HTTP request
        :type method: str
        :type label: str
        :type url: str
        :rtype: lxml.etree.Element
        """
        proxy = etree.Element("HTTPSamplerProxy", guiclass="HttpTestSampleGui", testclass="HTTPSamplerProxy")
        proxy.set("testname", label)

        args = JMX._get_arguments_panel("HTTPsampler.Arguments")

        if isinstance(body, string_types):
            proxy.append(JMX._bool_prop("HTTPSampler.postBodyRaw", True))
            coll_prop = JMX._collection_prop("Arguments.arguments")
            header = JMX._element_prop("elementProp", "HTTPArgument")
            header.append(JMX._string_prop("Argument.value", body))
            coll_prop.append(header)
            args.append(coll_prop)
            proxy.append(args)
        elif isinstance(body, dict):
            http_args_coll_prop = JMX._collection_prop("Arguments.arguments")
            for arg_name, arg_value in body.items():
                http_element_prop = JMX._element_prop(arg_name, "HTTPArgument")
                http_element_prop.append(JMX._bool_prop("HTTPArgument.always_encode", True))
                http_element_prop.append(JMX._bool_prop("HTTPArgument.use_equals", arg_value is not None))
                http_element_prop.append(JMX._string_prop("Argument.value", arg_value if arg_value is not None else ''))
                http_element_prop.append(JMX._string_prop("Argument.name", arg_name))
                http_element_prop.append(JMX._string_prop("Argument.metadata", '='))
                http_args_coll_prop.append(http_element_prop)
            args.append(http_args_coll_prop)
            proxy.append(args)
        elif body:
            raise ValueError("Cannot handle 'body' option of type %s: %s" % (type(body), body))

        proxy.append(JMX._string_prop("HTTPSampler.path", url))
        proxy.append(JMX._string_prop("HTTPSampler.method", method))
        proxy.append(JMX._bool_prop("HTTPSampler.use_keepalive", keepalive))
        proxy.append(JMX._bool_prop("HTTPSampler.follow_redirects", True))

        if timeout is not None:
            proxy.append(JMX._string_prop("HTTPSampler.connect_timeout", timeout))
            proxy.append(JMX._string_prop("HTTPSampler.response_timeout", timeout))
        return proxy

    @staticmethod
    def _element_prop(name, element_type):
        """
        Generates element property node

        :param name:
        :param element_type:
        :return:
        """
        res = etree.Element("elementProp", name=name, elementType=element_type)
        return res

    @staticmethod
    def _collection_prop(name):
        """
        Adds Collection prop
        :param name:
        :return:
        """
        res = etree.Element("collectionProp", name=name)
        return res

    @staticmethod
    def _string_prop(name, value):
        """
        Generates string property node

        :param name:
        :param value:
        :return:
        """
        res = etree.Element("stringProp", name=name)
        try:
            res.text = str(value)
        except ValueError:
            logging.warning("Failed to set string prop: %s=%s", name, value)
            res.text = 'BINARY'
        return res

    @staticmethod
    def _long_prop(name, value):
        """
        Generates long property node

        :param name:
        :param value:
        :return:
        """
        res = etree.Element("longProp", name=name)
        res.text = str(value)
        return res

    @staticmethod
    def _bool_prop(name, value):
        """
        Generates boolean property

        :param name:
        :param value:
        :return:
        """
        res = etree.Element("boolProp", name=name)
        res.text = 'true' if value else 'false'
        return res

    @staticmethod
    def int_prop(name, value):
        """
        JMX int property
        :param name:
        :param value:
        :return:
        """
        res = etree.Element("intProp", name=name)
        res.text = str(value)
        return res

    @staticmethod
    def _get_thread_group(concurrency=None, rampup=None, iterations=None, testname="ThreadGroup", on_error="continue"):
        """
        Generates ThreadGroup with 1 thread and 1 loop

        :param iterations:
        :param rampup:
        :param concurrency:
        :return:
        """
        trg = etree.Element("ThreadGroup", guiclass="ThreadGroupGui",
                            testclass="ThreadGroup", testname=testname)
        if on_error is not None:
            trg.append(JMX._string_prop("ThreadGroup.on_sample_error", on_error))
        loop = etree.Element("elementProp",
                             name="ThreadGroup.main_controller",
                             elementType="LoopController",
                             guiclass="LoopControlPanel",
                             testclass="LoopController")
        loop.append(JMX._bool_prop("LoopController.continue_forever", iterations < 0))
        if not iterations:
            iterations = 1
        loop.append(JMX._string_prop("LoopController.loops", iterations))

        trg.append(loop)

        if not concurrency:
            concurrency = 1
        trg.append(JMX._string_prop("ThreadGroup.num_threads", concurrency))

        if not rampup:
            rampup = ""
        trg.append(JMX._string_prop("ThreadGroup.ramp_time", rampup))

        trg.append(JMX._string_prop("ThreadGroup.start_time", ""))
        trg.append(JMX._string_prop("ThreadGroup.end_time", ""))
        trg.append(JMX._bool_prop("ThreadGroup.scheduler", False))
        trg.append(JMX._long_prop("ThreadGroup.duration", 0))

        return trg

    def get_rps_shaper(self):
        """

        :return: etree.Element
        """

        throughput_timer_element = etree.Element("kg.apc.jmeter.timers.VariableThroughputTimer",
                                                 guiclass="kg.apc.jmeter.timers.VariableThroughputTimerGui",
                                                 testclass="kg.apc.jmeter.timers.VariableThroughputTimer",
                                                 testname="jp@gc - Throughput Shaping Timer",
                                                 enabled="true")
        shaper_load_prof = self._collection_prop("load_profile")

        throughput_timer_element.append(shaper_load_prof)

        return throughput_timer_element

    def add_rps_shaper_schedule(self, shaper_etree, start_rps, end_rps, duration):
        """
        Adds schedule to rps shaper
        :param shaper_etree:
        :param start_rps:
        :param end_rps:
        :param duration:
        :return:
        """
        shaper_collection = shaper_etree.find(".//collectionProp[@name='load_profile']")
        coll_prop = self._collection_prop("1817389797")
        start_rps_prop = self._string_prop("49", int(start_rps))
        end_rps_prop = self._string_prop("1567", int(end_rps))
        duration_prop = self._string_prop("53", int(duration))
        coll_prop.append(start_rps_prop)
        coll_prop.append(end_rps_prop)
        coll_prop.append(duration_prop)
        shaper_collection.append(coll_prop)

    @staticmethod
    def add_user_def_vars_elements(udv_dict, testname="Variables from Taurus"):
        """
        :type testname: str
        :type udv_dict: dict[str,str]
        :rtype: etree.Element
        """

        udv_element = etree.Element("Arguments", guiclass="ArgumentsPanel", testclass="Arguments",
                                    testname=testname)
        udv_collection_prop = JMX._collection_prop("Arguments.arguments")

        for var_name in sorted(udv_dict.keys()):
            udv_element_prop = JMX._element_prop(var_name, "Argument")
            udv_arg_name_prop = JMX._string_prop("Argument.name", var_name)
            udv_arg_value_prop = JMX._string_prop("Argument.value", udv_dict[var_name])
            udv_arg_desc_prop = JMX._string_prop("Argument.desc", "")
            udv_arg_meta_prop = JMX._string_prop("Argument.metadata", "=")
            udv_element_prop.append(udv_arg_name_prop)
            udv_element_prop.append(udv_arg_value_prop)
            udv_element_prop.append(udv_arg_desc_prop)
            udv_element_prop.append(udv_arg_meta_prop)
            udv_collection_prop.append(udv_element_prop)

        udv_element.append(udv_collection_prop)
        return udv_element

    @staticmethod
    def get_stepping_thread_group(concurrency, step_threads, step_time, hold_for, tg_name):
        """
        :return: etree element, Stepping Thread Group
        """
        stepping_thread_group = etree.Element("kg.apc.jmeter.threads.SteppingThreadGroup",
                                              guiclass="kg.apc.jmeter.threads.SteppingThreadGroupGui",
                                              testclass="kg.apc.jmeter.threads.SteppingThreadGroup",
                                              testname=tg_name, enabled="true")
        stepping_thread_group.append(JMX._string_prop("ThreadGroup.on_sample_error", "continue"))
        stepping_thread_group.append(JMX._string_prop("ThreadGroup.num_threads", concurrency))
        stepping_thread_group.append(JMX._string_prop("Threads initial delay", 0))
        stepping_thread_group.append(JMX._string_prop("Start users count", step_threads))
        stepping_thread_group.append(JMX._string_prop("Start users count burst", 0))
        stepping_thread_group.append(JMX._string_prop("Start users period", step_time))
        stepping_thread_group.append(JMX._string_prop("Stop users count", ""))
        stepping_thread_group.append(JMX._string_prop("Stop users period", 1))
        stepping_thread_group.append(JMX._string_prop("flighttime", int(hold_for)))
        stepping_thread_group.append(JMX._string_prop("rampUp", 0))

        loop_controller = etree.Element("elementProp", name="ThreadGroup.main_controller", elementType="LoopController",
                                        guiclass="LoopControlPanel", testclass="LoopController",
                                        testname="Loop Controller", enabled="true")
        loop_controller.append(JMX._bool_prop("LoopController.continue_forever", False))
        loop_controller.append(JMX.int_prop("LoopController.loops", -1))

        stepping_thread_group.append(loop_controller)
        return stepping_thread_group

    @staticmethod
    def get_dns_cache_mgr():
        """
        Adds dns cache element with defaults parameters

        :return:
        """
        dns_element = etree.Element("DNSCacheManager", guiclass="DNSCachePanel", testclass="DNSCacheManager",
                                    testname="DNS Cache Manager")
        dns_element.append(JMX._collection_prop("DNSCacheManager.servers"))
        dns_element.append(JMX._bool_prop("DNSCacheManager.clearEachIteration", False))
        dns_element.append(JMX._bool_prop("DNSCacheManager.isCustomResolver", False))
        return dns_element

    @staticmethod
    def _get_header_mgr(hdict):
        """

        :type hdict: dict[str,str]
        :rtype: lxml.etree.Element
        """
        mgr = etree.Element("HeaderManager", guiclass="HeaderPanel", testclass="HeaderManager", testname="Headers")

        coll_prop = etree.Element("collectionProp", name="HeaderManager.headers")
        for hname, hval in iteritems(hdict):
            header = etree.Element("elementProp", name="", elementType="Header")
            header.append(JMX._string_prop("Header.name", hname))
            header.append(JMX._string_prop("Header.value", hval))
            coll_prop.append(header)
        mgr.append(coll_prop)
        return mgr

    @staticmethod
    def _get_cache_mgr():
        """
        :rtype: lxml.etree.Element
        """
        mgr = etree.Element("CacheManager", guiclass="CacheManagerGui", testclass="CacheManager", testname="Cache")
        mgr.append(JMX._bool_prop("clearEachIteration", True))
        mgr.append(JMX._bool_prop("useExpires", True))
        return mgr

    @staticmethod
    def _get_cookie_mgr():
        """
        :rtype: lxml.etree.Element
        """
        mgr = etree.Element("CookieManager", guiclass="CookiePanel", testclass="CookieManager", testname="Cookies")
        mgr.append(JMX._bool_prop("CookieManager.clearEachIteration", True))
        return mgr

    @staticmethod
    def _get_http_defaults(default_address=None, timeout=None, retrieve_resources=None, concurrent_pool_size=4):
        """
        :rtype: lxml.etree.Element
        """
        cfg = etree.Element("ConfigTestElement", guiclass="HttpDefaultsGui",
                            testclass="ConfigTestElement", testname="Defaults")

        if retrieve_resources:
            cfg.append(JMX._bool_prop("HTTPSampler.image_parser", True))
            cfg.append(JMX._bool_prop("HTTPSampler.concurrentDwn", True))
            if concurrent_pool_size:
                cfg.append(JMX._string_prop("HTTPSampler.concurrentPool", concurrent_pool_size))

        params = etree.Element("elementProp",
                               name="HTTPsampler.Arguments",
                               elementType="Arguments",
                               guiclass="HTTPArgumentsPanel",
                               testclass="Arguments", testname="user_defined")
        cfg.append(params)
        if default_address:
            parsed_url = parse.urlsplit(default_address)
            if parsed_url.scheme:
                cfg.append(JMX._string_prop("HTTPSampler.protocol", parsed_url.scheme))
            if parsed_url.hostname:
                cfg.append(JMX._string_prop("HTTPSampler.domain", parsed_url.hostname))
            if parsed_url.port:
                cfg.append(JMX._string_prop("HTTPSampler.port", parsed_url.port))

        if timeout:
            cfg.append(JMX._string_prop("HTTPSampler.connect_timeout", timeout))
            cfg.append(JMX._string_prop("HTTPSampler.response_timeout", timeout))
        return cfg

    @staticmethod
    def _get_dur_assertion(timeout):
        """

        :type timeout: int
        :return:
        """
        element = etree.Element("DurationAssertion", guiclass="DurationAssertionGui",
                                testclass="DurationAssertion", testname="Timeout Check")
        element.append(JMX._string_prop("DurationAssertion.duration", timeout))
        return element

    @staticmethod
    def _get_constant_timer(delay):
        """

        :type delay: int
        :rtype: lxml.etree.Element
        """
        element = etree.Element("ConstantTimer", guiclass="ConstantTimerGui",
                                testclass="ConstantTimer", testname="Think-Time")
        element.append(JMX._string_prop("ConstantTimer.delay", delay))
        return element

    @staticmethod
    def _get_extractor(varname, regexp, template, match_no, default='NOT_FOUND'):
        """

        :type varname: str
        :type regexp: str
        :type template: str|int
        :type match_no: int
        :type default: str
        :rtype: lxml.etree.Element
        """
        if isinstance(template, int):
            template = '$%s$' % template

        element = etree.Element("RegexExtractor", guiclass="RegexExtractorGui",
                                testclass="RegexExtractor", testname="Get %s" % varname)
        element.append(JMX._string_prop("RegexExtractor.refname", varname))
        element.append(JMX._string_prop("RegexExtractor.regex", regexp))
        element.append(JMX._string_prop("RegexExtractor.template", template))
        element.append(JMX._string_prop("RegexExtractor.match_number", match_no))
        element.append(JMX._string_prop("RegexExtractor.default", default))
        return element

    @staticmethod
    def _get_jquerycss_extractor(varname, selector, attribute, match_no, default="NOT_FOUND"):
        """

        :type varname: str
        :type regexp: str
        :type match_no: int
        :type default: str
        :rtype: lxml.etree.Element
        """

        element = etree.Element("HtmlExtractor", guiclass="HtmlExtractorGui", testclass="HtmlExtractor",
                                testname="Get %s" % varname)
        element.append(JMX._string_prop("HtmlExtractor.refname", varname))
        element.append(JMX._string_prop("HtmlExtractor.expr", selector))
        element.append(JMX._string_prop("HtmlExtractor.attribute", attribute))
        element.append(JMX._string_prop("HtmlExtractor.match_number", match_no))
        element.append(JMX._string_prop("HtmlExtractor.default", default))
        return element

    @staticmethod
    def _get_json_extractor(varname, jsonpath, default='NOT_FOUND', from_variable=None):
        """

        :type varname: str
        :type default: str
        :rtype: lxml.etree.Element
        """
        package = "com.atlantbh.jmeter.plugins.jsonutils.jsonpathextractor"
        element = etree.Element("%s.JSONPathExtractor" % package,
                                guiclass="%s.gui.JSONPathExtractorGui" % package,
                                testclass="%s.JSONPathExtractor" % package,
                                testname="Get %s" % varname)
        element.append(JMX._string_prop("VAR", varname))
        element.append(JMX._string_prop("JSONPATH", jsonpath))
        element.append(JMX._string_prop("DEFAULT", default))
        if from_variable:
            element.append(JMX._string_prop("VARIABLE", from_variable))
            element.append(JMX._string_prop("SUBJECT", "VAR"))
        return element

    @staticmethod
    def _get_json_path_assertion(jsonpath, expected_value, json_validation, expect_null, invert):
        """
        :type jsonpath: str
        :type expected_value: str
        :type json_validation: bool
        :type expect_null: bool
        :return: lxml.etree.Element
        """
        package = "com.atlantbh.jmeter.plugins.jsonutils.jsonpathassertion"
        element = etree.Element("%s.JSONPathAssertion" % package,
                                guiclass="%s.gui.JSONPathAssertionGui" % package,
                                testclass="%s.JSONPathAssertion" % package,
                                testname="JSon path assertion")
        element.append(JMX._string_prop("JSON_PATH", jsonpath))
        element.append(JMX._string_prop("EXPECTED_VALUE", expected_value))
        element.append(JMX._bool_prop("JSONVALIDATION", json_validation))
        element.append(JMX._bool_prop("EXPECT_NULL", expect_null))
        element.append(JMX._bool_prop("INVERT", invert))

        return element

    @staticmethod
    def _get_resp_assertion(field, contains, is_regexp, is_invert, assume_success=False):
        """

        :type field: str
        :type contains: list[str]
        :type is_regexp: bool
        :type is_invert:  bool
        :rtype: lxml.etree.Element
        """
        tname = "Assert %s has %s" % ("not" if is_invert else "", [str(x) for x in contains])
        element = etree.Element("ResponseAssertion", guiclass="AssertionGui",
                                testclass="ResponseAssertion", testname=tname)
        if field == JMX.FIELD_HEADERS:
            fld = "Assertion.response_headers"
        elif field == JMX.FIELD_RESP_CODE:
            fld = "Assertion.response_code"
        else:
            fld = "Assertion.response_data"

        if is_regexp:
            if is_invert:
                mtype = 6  # not contains
            else:
                mtype = 2  # contains
        else:
            if is_invert:
                mtype = 20  # not substring
            else:
                mtype = 16  # substring

        element.append(JMX._string_prop("Assertion.test_field", fld))
        element.append(JMX._string_prop("Assertion.test_type", mtype))
        element.append(JMX._bool_prop("Assertion.assume_success", assume_success))

        coll_prop = etree.Element("collectionProp", name="Asserion.test_strings")
        for string in contains:
            coll_prop.append(JMX._string_prop("", string))
        element.append(coll_prop)

        return element

    @staticmethod
    def _get_csv_config(path, delimiter, is_quoted, is_recycle):
        """

        :type path: str
        :type delimiter: str
        :type is_quoted: bool
        :type is_recycle: bool
        :return:
        """
        element = etree.Element("CSVDataSet", guiclass="TestBeanGUI",
                                testclass="CSVDataSet", testname="CSV %s" % os.path.basename(path))
        element.append(JMX._string_prop("filename", path))
        element.append(JMX._string_prop("delimiter", delimiter))
        element.append(JMX._bool_prop("quotedData", is_quoted))
        element.append(JMX._bool_prop("recycle", is_recycle))
        return element

    def set_enabled(self, sel, state):
        """
        Toggle items by selector

        :type sel: str
        :type state: bool
        """
        items = self.get(sel)
        self.log.debug("Enable %s elements %s: %s", state, sel, items)
        for item in items:
            item.set("enabled", 'true' if state else 'false')

    def set_text(self, sel, text):
        """
        Set text value

        :type sel: str
        :type text: str
        """
        items = self.get(sel)
        for item in items:
            item.text = str(text)

    @staticmethod
    def _get_simple_controller(name):
        return etree.Element("GenericController", guiclass="LogicControllerGui", testclass="GenericController",
                             testname=name)

    def _add_results_tree(self):
        dbg_tree = etree.Element("ResultCollector",
                                 testname="View Results Tree",
                                 testclass="ResultCollector",
                                 guiclass="ViewResultsFullVisualizer")
        self.append(self.TEST_PLAN_SEL, dbg_tree)
        self.append(self.TEST_PLAN_SEL, etree.Element("hashTree"))


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
            label = row["label"]
            if self.is_distributed:
                concur = int(row["grpThreads"])
                trname = row["threadName"][:row["threadName"].rfind('-')]
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

            tstmp = int(int(row["timeStamp"]) / 1000)
            yield tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname

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
            lines = self.fds.readlines(1024 * 1024)  # 1MB limit to read

        self.offset = self.fds.tell()

        self.log.debug("Read lines: %s / %s bytes", len(lines), len(''.join(lines)))

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

    def __del__(self):
        if self.fds:
            self.fds.close()

    def read_file(self, final_pass=False):
        """
        Read the next part of the file

        :type final_pass: bool
        :return:
        """
        if not self.fds:
            if os.path.exists(self.filename):
                self.log.debug("Opening %s", self.filename)
                self.fds = open(self.filename, 'rb')  # NOTE: maybe we have the same mac problem with seek() needed
            else:
                self.log.debug("File not exists: %s", self.filename)
                return

        self.fds.seek(self.offset)
        try:
            self.parser.feed(self.fds.read(1024 * 1024))  # "Huge input lookup" error without capping :)
        except etree.XMLSyntaxError as exc:
            self.log.debug("Error reading errors.jtl: %s", traceback.format_exc())
            self.log.warning("Failed to parse errors XML: %s", exc)

        self.offset = self.fds.tell()
        for _action, elem in self.parser.read_events():
            if elem.getparent() is None or elem.getparent().tag != 'testResults':
                continue

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

        :param max_ts:
        :return:
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
        if len(massert):
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

    :param original: inherited from JMX
    """

    def __init__(self, original=None):
        super(JMeterScenarioBuilder, self).__init__(original)
        self.scenario = Scenario()
        self.system_props = BetterDict()

    def __add_managers(self):
        headers = self.scenario.get_headers()
        if headers:
            self.append(self.TEST_PLAN_SEL, self._get_header_mgr(headers))
            self.append(self.TEST_PLAN_SEL, etree.Element("hashTree"))
        if self.scenario.get("store-cache", True):
            self.append(self.TEST_PLAN_SEL, self._get_cache_mgr())
            self.append(self.TEST_PLAN_SEL, etree.Element("hashTree"))
        if self.scenario.get("store-cookie", True):
            self.append(self.TEST_PLAN_SEL, self._get_cookie_mgr())
            self.append(self.TEST_PLAN_SEL, etree.Element("hashTree"))
        if self.scenario.get("use-dns-cache-mgr", True):
            self.append(self.TEST_PLAN_SEL, self.get_dns_cache_mgr())
            self.append(self.TEST_PLAN_SEL, etree.Element("hashTree"))
            self.system_props.merge({"system-properties": {"sun.net.inetaddr.ttl": 0}})

    def __add_defaults(self):
        """

        :return:
        """
        default_address = self.scenario.get("default-address", None)
        retrieve_resources = self.scenario.get("retrieve-resources", True)
        concurrent_pool_size = self.scenario.get("concurrent-pool-size", 4)

        timeout = self.scenario.get("timeout", None)
        timeout = int(1000 * dehumanize_time(timeout))
        self.append(self.TEST_PLAN_SEL, self._get_http_defaults(default_address, timeout,
                                                                retrieve_resources, concurrent_pool_size))
        self.append(self.TEST_PLAN_SEL, etree.Element("hashTree"))

    def __add_think_time(self, children, req):
        global_ttime = self.scenario.get("think-time", None)
        if req.think_time is not None:
            ttime = int(1000 * dehumanize_time(req.think_time))
        elif global_ttime is not None:
            ttime = int(1000 * dehumanize_time(global_ttime))
        else:
            ttime = None
        if ttime is not None:
            children.append(JMX._get_constant_timer(ttime))
            children.append(etree.Element("hashTree"))

    def __add_extractors(self, children, req):
        extractors = req.config.get("extract-regexp", BetterDict())
        for varname in extractors:
            cfg = ensure_is_dict(extractors, varname, "regexp")
            extractor = JMX._get_extractor(varname, cfg['regexp'], cfg.get('template', 1),
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
            children.append(
                    JMX._get_jquerycss_extractor(varname, cfg['expression'], cfg.get('attribute', ""),
                                                 cfg.get('match-no', 0), cfg.get('default', 'NOT_FOUND')))
            children.append(etree.Element("hashTree"))

    def __add_assertions(self, children, req):
        assertions = req.config.get("assert", [])
        for idx, assertion in enumerate(assertions):
            assertion = ensure_is_dict(assertions, idx, "contains")
            if not isinstance(assertion['contains'], list):
                assertion['contains'] = [assertion['contains']]
            children.append(JMX._get_resp_assertion(assertion.get("subject", self.FIELD_BODY),
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

    def __add_requests(self):
        global_timeout = self.scenario.get("timeout", None)
        global_keepalive = self.scenario.get("keepalive", True)

        for req in self.scenario.get_requests():
            if req.timeout is not None:
                timeout = int(1000 * dehumanize_time(req.timeout))
            elif global_timeout is not None:
                timeout = int(1000 * dehumanize_time(global_timeout))
            else:
                timeout = None

            http = JMX._get_http_request(req.url, req.label, req.method, timeout, req.body,
                                         global_keepalive)
            self.append(self.THR_GROUP_SEL, http)

            children = etree.Element("hashTree")
            self.append(self.THR_GROUP_SEL, children)
            if req.headers:
                children.append(JMX._get_header_mgr(req.headers))
                children.append(etree.Element("hashTree"))

            self.__add_think_time(children, req)

            self.__add_assertions(children, req)

            if timeout is not None:
                children.append(JMX._get_dur_assertion(timeout))
                children.append(etree.Element("hashTree"))

            self.__add_extractors(children, req)

    def __generate(self):
        """
        Generate the test plan
        """
        # NOTE: set realistic dns-cache and JVM prop by default?
        self.__add_managers()
        self.__add_defaults()
        self.__add_datasources()

        thread_group = self._get_thread_group(1, 0, -1)
        self.append(self.TEST_PLAN_SEL, thread_group)
        self.append(self.TEST_PLAN_SEL, etree.Element("hashTree", type="tg"))  # arbitrary trick with our own attribute

        self.__add_requests()
        self._add_results_tree()

    def save(self, filename):
        """
        Generate test plan and save

        :type filename: str
        """
        # NOTE: bad design, as repetitive save will duplicate stuff
        self.__generate()
        super(JMeterScenarioBuilder, self).save(filename)

    def __add_datasources(self):
        sources = self.scenario.get("data-sources", [])
        for idx, source in enumerate(sources):
            source = ensure_is_dict(sources, idx, "path")

            delimiter = source.get("delimiter", self.__guess_delimiter(source['path']))

            self.append(self.TEST_PLAN_SEL, JMX._get_csv_config(
                    os.path.abspath(source['path']), delimiter,
                    source.get("quoted", False), source.get("loop", True)
            ))
            self.append(self.TEST_PLAN_SEL, etree.Element("hashTree"))

    def __guess_delimiter(self, path):
        with open(path) as fhd:
            header = fhd.read(4096)  # 4KB is enough for header
            return guess_csv_dialect(header).delimiter


class JMeter(RequiredTool):
    """
    JMeter tool
    """

    def __init__(self, tool_path, parent_logger, jmeter_version):
        super(JMeter, self).__init__("JMeter", tool_path)
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.version = jmeter_version
        self.mirror_manager = JMeterMirrorsManager(self.log, self.version)

    def check_if_installed(self):
        self.log.debug("Trying jmeter: %s", self.tool_path)
        try:
            jmlog = tempfile.NamedTemporaryFile(prefix="jmeter", suffix="log", delete=False)
            jm_proc = shell_exec([self.tool_path, '-j', jmlog.name, '--version'], stderr=subprocess.STDOUT)
            jmout, jmerr = jm_proc.communicate()
            self.log.debug("JMeter check: %s / %s", jmout, jmerr)
            jmlog.close()
            os.remove(jmlog.name)
            if isinstance(jmout, binary_type):
                jmout = jmout.decode()
            if "is too low to run JMeter" in jmout:
                self.log.error(jmout)
                raise ValueError("Java version is too low to run JMeter")
            return True
        except OSError:
            self.log.debug("JMeter check failed.")
            return False

    def install(self):
        dest = os.path.dirname(os.path.dirname(os.path.expanduser(self.tool_path)))
        dest = os.path.abspath(dest)
        jmeter_dist = super(JMeter, self).install_with_mirrors(dest, ".zip")
        self.log.info("Unzipping %s to %s", jmeter_dist.name, dest)
        unzip(jmeter_dist.name, dest, 'apache-jmeter-%s' % self.version)
        # set exec permissions
        os.chmod(self.tool_path, 0o755)
        jmeter_dist.close()
        os.remove(jmeter_dist.name)
        if self.check_if_installed():
            return self.tool_path
        else:
            raise RuntimeError("Unable to run %s after installation!" % self.tool_name)


class JMeterPlugins(RequiredTool):
    """
    JMeter plugins
    """

    def __init__(self, tool_path, download_link, parent_logger):
        super(JMeterPlugins, self).__init__("JMeterPlugins", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.plugins = ["Standard", "Extras", "ExtrasLibs", "WebDriver"]

    def check_if_installed(self):
        plugin_folder = os.path.join(os.path.dirname(os.path.dirname(self.tool_path)), "lib", "ext")
        if os.path.exists(plugin_folder):
            listed_files = os.listdir(plugin_folder)
            for plugin in self.plugins:
                if "JMeterPlugins-%s.jar" % plugin not in listed_files:
                    return False
            return True
        else:
            return False

    def install(self):
        dest = os.path.dirname(os.path.dirname(os.path.expanduser(self.tool_path)))
        for set_name in ("Standard", "Extras", "ExtrasLibs", "WebDriver"):
            plugin_dist = tempfile.NamedTemporaryFile(suffix=".zip", delete=False, prefix=set_name)
            plugin_download_link = self.download_link.format(plugin=set_name)
            self.log.info("Downloading %s", plugin_download_link)
            downloader = request.FancyURLopener()
            with ProgressBarContext() as pbar:
                try:
                    downloader.retrieve(plugin_download_link, plugin_dist.name, pbar.download_callback)
                except BaseException as exc:
                    self.log.error("Error while downloading %s", plugin_download_link)
                    raise exc

            self.log.info("Unzipping %s", plugin_dist.name)
            unzip(plugin_dist.name, dest)
            plugin_dist.close()
            os.remove(plugin_dist.name)
        cleaner = JarCleaner(self.log)
        cleaner.clean(os.path.join(dest, 'lib'))


class JarCleaner(object):
    def __init__(self, parent_logger):
        self.log = parent_logger.getChild(self.__class__.__name__)

    def clean(self, path):
        """
        Remove old jars
        """
        self.log.debug("Removing old jars from %s", path)
        jarlib = namedtuple("jarlib", ("file_name", "lib_name"))
        jars = [fname for fname in os.listdir(path) if '-' in fname and os.path.isfile(os.path.join(path, fname))]
        jar_libs = [jarlib(file_name=jar, lib_name='-'.join(jar.split('-')[:-1])) for jar in jars]

        duplicated_libraries = []
        for jar_lib_obj in jar_libs:
            similar_packages = [LooseVersion(_jarlib.file_name) for _jarlib in
                                [lib for lib in jar_libs if lib.lib_name == jar_lib_obj.lib_name]]
            if len(similar_packages) > 1:
                right_version = max(similar_packages)
                similar_packages.remove(right_version)
                duplicated_libraries.extend([lib for lib in similar_packages if lib not in duplicated_libraries])

        for old_lib in duplicated_libraries:
            os.remove(os.path.join(path, old_lib.vstring))
            self.log.debug("Old jar removed %s", old_lib.vstring)


class JMeterMirrorsManager(MirrorsManager):
    def __init__(self, parent_logger, jmeter_version):
        self.jmeter_version = jmeter_version
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
        default_link = JMeterExecutor.JMETER_DOWNLOAD_LINK.format(version=self.jmeter_version)
        if default_link not in links:
            links.append(default_link)
        self.log.debug('Total mirrors: %d', len(links))
        return links
