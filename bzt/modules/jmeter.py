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
from collections import Counter
import os
import platform
import subprocess
import time
import signal
import traceback
import logging
from subprocess import CalledProcessError
import six
import shutil

from cssselect import GenericTranslator
import urwid

from bzt.engine import ScenarioExecutor, Scenario, FileLister
from bzt.modules.console import WidgetProvider
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader, DataPoint, KPISet
from bzt.utils import shell_exec, ensure_is_dict, humanize_time, dehumanize_time, BetterDict, \
    guess_csv_delimiter, unzip, download_progress_hook


try:
    from lxml import etree
except ImportError:
    try:
        import cElementTree as etree
    except ImportError:
        import elementtree.ElementTree as etree

try:
    from urllib import URLopener
except ImportError:
    from urllib.request import URLopener

exe_suffix = ".bat" if platform.system() == 'Windows' else ""


class JMeterExecutor(ScenarioExecutor, WidgetProvider, FileLister):
    """
    JMeter executor module
    """
    JMETER_DOWNLOAD_LINK = "http://apache.claz.org/jmeter/binaries/apache-jmeter-{version}.zip"
    JMETER_VER = "2.13"
    PLUGINS_DOWNLOAD_TPL = "http://jmeter-plugins.org/files/JMeterPlugins-{plugin}-1.2.1.zip"

    def __init__(self):
        super(JMeterExecutor, self).__init__()
        self.original_jmx = None
        self.modified_jmx = None
        self.jmeter_log = None
        self.properties_file = None
        self.kpi_jtl = None
        self.errors_jtl = None
        self.process = None
        self.start_time = None
        self.end_time = None
        self.retcode = None
        self.reader = None
        self.widget = None

    def prepare(self):
        """
        Preparation for JMeter involves either getting existing JMX
        and modifying it, or generating new JMX from input data. Then,
        original JMX is modified to contain JTL writing classes with
        required settings and have workload as suggested by Provisioning

        :raise ValueError:
        """
        # TODO: global variables
        # TODO: move all files to artifacts
        self.jmeter_log = self.engine.create_artifact("jmeter", ".log")

        # TODO: switch to verifier.verify()
        self.__check_jmeter()
        # self.verifier.verify()

        scenario = self.get_scenario()

        if Scenario.SCRIPT in scenario:
            self.original_jmx = self.__get_script()
            self.engine.existing_artifact(self.original_jmx)
        elif "requests" in scenario:
            self.original_jmx = self.__jmx_from_requests()
        else:
            raise ValueError("There must be a JMX file to run JMeter")

        load = self.get_load()
        self.modified_jmx = self.__get_modified_jmx(self.original_jmx, load)

        props = self.settings.get("properties")
        props_local = scenario.get("properties")
        props.merge(props_local)
        props['user.classpath'] = self.engine.artifacts_dir
        if props:
            self.log.debug("Additional properties: %s", props)
            props_file = self.engine.create_artifact("jmeter-bzt", ".properties")
            with open(props_file, 'w') as fds:
                for key, val in six.iteritems(props):
                    fds.write("%s=%s\n" % (key, val))
            self.properties_file = props_file

        self.reader = JTLReader(self.kpi_jtl, self.log, self.errors_jtl)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    # TODO: weighted requests
    def startup(self):
        """
        Should start JMeter as fast as possible.
        """
        cmdline = [self.settings.get("path")]  # default is set when prepared
        if not self.settings.get("gui", False):
            cmdline += ["-n"]
        cmdline += ["-t", self.modified_jmx]
        if self.jmeter_log:
            cmdline += ["-j", self.jmeter_log]

        if self.properties_file:
            cmdline += ["-p", self.properties_file]

        self.start_time = time.time()
        try:
            self.process = shell_exec(cmdline)
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

            if self.kpi_jtl:
                if not os.path.exists(self.kpi_jtl) or not os.path.getsize(self.kpi_jtl):
                    msg = "Empty results JTL, most likely JMeter failed: %s"
                    raise RuntimeWarning(msg % self.kpi_jtl)
            return True

        return False

    def shutdown(self):
        """
        If JMeter is still running - let's stop it.
        """
        while self.process and self.process.poll() is None:
            # TODO: find a way to have graceful shutdown, then kill
            self.log.info("Terminating jmeter PID: %s", self.process.pid)
            time.sleep(1)
            try:
                if platform.system() == 'Windows':
                    os.kill(self.process.pid, signal.SIGTERM)
                else:
                    os.killpg(self.process.pid, signal.SIGTERM)
            except OSError as exc:
                self.log.debug("Failed to terminate jmeter: %s", exc)

        if self.start_time:
            self.end_time = time.time()
            self.log.debug("JMeter worked for %s seconds", self.end_time - self.start_time)

    def __apply_ramp_up(self, jmx, ramp_up):
        rampup_sel = "stringProp[name='ThreadGroup.ramp_time']"
        xpath = GenericTranslator().css_to_xpath(rampup_sel)

        for group in jmx.enabled_thread_groups():
            prop = group.xpath(xpath)
            prop[0].text = str(ramp_up)

    def __apply_duration(self, jmx, duration):
        sched_sel = "[name='ThreadGroup.scheduler']"
        sched_xpath = GenericTranslator().css_to_xpath(sched_sel)
        dur_sel = "[name='ThreadGroup.duration']"
        dur_xpath = GenericTranslator().css_to_xpath(dur_sel)

        for group in jmx.enabled_thread_groups():
            group.xpath(sched_xpath)[0].text = 'true'
            group.xpath(dur_xpath)[0].text = str(int(duration))

    def __apply_iterations(self, jmx, iterations):
        sel = "elementProp>[name='LoopController.loops']"
        xpath = GenericTranslator().css_to_xpath(sel)

        flag_sel = "elementProp>[name='LoopController.continue_forever']"
        flag_xpath = GenericTranslator().css_to_xpath(flag_sel)

        for group in jmx.enabled_thread_groups():
            bprop = group.xpath(flag_xpath)
            if not iterations:
                bprop[0].text = 'true'
            else:
                bprop[0].text = 'false'

            sprop = group.xpath(xpath)
            if not iterations:
                sprop[0].text = str(-1)
            else:
                sprop[0].text = str(iterations)

    def __apply_concurrency(self, jmx, concurrency):
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

    def __disable_listeners(self, jmx):
        sel = 'stringProp[name=filename]'
        xpath = GenericTranslator().css_to_xpath(sel)

        listeners = jmx.get('ResultCollector')
        for listener in listeners:
            file_setting = listener.xpath(xpath)
            if not file_setting or not file_setting[0].text:
                listener.set("enabled", "false")

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
            self.__disable_listeners(jmx)

        self.__apply_modifications(jmx)

        if load.duration and load.iterations:
            msg = "You have specified both iterations count"
            msg += " and ramp-up/hold duration times, so test will end"
            msg += " on what runs out first"
            self.log.warning(msg)

        if load.concurrency:
            self.__apply_concurrency(jmx, load.concurrency)

        if load.ramp_up is not None:
            self.__apply_ramp_up(jmx, int(load.ramp_up))

        if load.iterations is not None:
            self.__apply_iterations(jmx, load.iterations)

        if load.duration:
            self.__apply_duration(jmx, int(load.duration))

        if load.throughput:
            self.__add_shaper(jmx, load)

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

        prefix = "modified_" + os.path.basename(original)
        filename = self.engine.create_artifact(prefix, ".jmx")
        jmx.save(filename)
        return filename

    def __jmx_from_requests(self):
        filename = self.engine.create_artifact("requests", ".jmx")
        jmx = JMeterScenarioBuilder()
        jmx.scenario = self.get_scenario()
        jmx.save(filename)
        return filename

    def get_widget(self):
        """
        Add progress widget to console screen sidebar

        :return:
        """
        if not self.widget:
            self.widget = JMeterWidget(self)
        return self.widget

    def resource_files(self):
        """
        Get resource files
        """
        # TODO: get CSVs, other known files like included test plans
        resource_files = []
        # get all resource files from settings
        files_from_requests = self.extract_resources_from_scenario()
        script = self.__get_script()
        if script:
            script_xml_tree = etree.fromstring(open(script, "rb").read())
            resource_files, modified_xml_tree = self.__get_resource_files_from_script(script_xml_tree)
            if resource_files:
                # copy to artifacts dir
                for resource_file in resource_files:
                    if os.path.exists(resource_file):
                        try:
                            shutil.copy(resource_file, self.engine.artifacts_dir)
                        except:
                            self.log.warning("Cannot copy file: %s" % resource_file)
                    else:
                        self.log.warning("File not found: %s" % resource_file)

                script_name, script_ext = os.path.splitext(script)
                script_name = os.path.basename(script_name)
                # create modified jmx script in artifacts dir
                modified_script = self.engine.create_artifact(script_name, script_ext)
                with open(modified_script, 'wb') as _fds:
                    _fds.write(
                        etree.tostring(modified_xml_tree, pretty_print=True, encoding="UTF-8", xml_declaration=True))
                resource_files.append(modified_script)
            else:
                # copy original script to artifacts
                shutil.copy2(script, self.engine.artifacts_dir)
                resource_files.append(script)

        resource_files.extend(files_from_requests)
        return [os.path.basename(file_path) for file_path in resource_files]  # return list of file names

    def __get_resource_files_from_script(self, script_xml_tree):
        """

        :return: (list, etree)
        """
        resource_files = []
        search_patterns = ["File.path", "filename", "BeanShellSampler.filename"]
        for pattern in search_patterns:
            resource_elements = script_xml_tree.findall(".//stringProp[@name='%s']" % pattern)
            for resource_element in resource_elements:
                # check if none of parents are disabled
                parent = resource_element.getparent()
                parent_disabled = False
                while parent is not None:  # ?
                    if parent.get('enabled') == 'false':
                        parent_disabled = True
                        break
                    parent = parent.getparent()

                if resource_element.text and parent_disabled is False:
                    resource_files.append(resource_element.text)
                    resource_element.text = os.path.basename(resource_element.text)
        return resource_files, script_xml_tree

    def extract_resources_from_scenario(self):
        """
        Get post-body files from scenario
        :return:
        """
        post_body_files = []
        scenario = self.get_scenario()
        requests = scenario.data.get("requests")
        if requests:
            for req in requests:
                if isinstance(req, dict):
                    post_body_path = req.get('body-file')
                    if post_body_path:
                        if os.path.exists(post_body_path):
                            try:
                                shutil.copy(post_body_path, self.engine.artifacts_dir)
                            except:
                                self.log.warning("Cannot copy file: %s" % post_body_path)
                        else:
                            self.log.warning("File not found: %s" % post_body_path)

                        post_body_files.append(post_body_path)

        return post_body_files

    def __get_script(self):
        scenario = self.get_scenario()
        if Scenario.SCRIPT not in scenario:
            return None

        scen = ensure_is_dict(scenario, Scenario.SCRIPT, "path")
        fname = scen["path"]
        if fname is not None:
            return self.engine.find_file(fname)
        else:
            return None

    def __add_shaper(self, jmx, load):
        # TODO: add RPS control when needed
        # TODO: how to make it spread load appropriately?
        pass

    def __apply_modifications(self, jmx):
        """
        :type jmx: JMX
        """
        modifs = self.get_scenario().get("modifications")
        for action, items in six.iteritems(modifs):
            if action in ('disable', 'enable'):
                if not isinstance(items, list):
                    modifs[action] = [items]
                    items = modifs[action]
                for name in items:
                    jmx.set_enabled("[testname='%s']" % name, True if action == 'enable' else False)
            elif action == 'set-prop':
                for path, text in six.iteritems(items):
                    parts = path.split('>')
                    if len(parts) < 2:
                        raise ValueError("Property selector must have at least 2 levels")
                    sel = "[testname='%s']" % parts[0]
                    for add in parts[1:]:
                        sel += ">[name='%s']" % add
                    jmx.set_text(sel, text)
            else:
                raise ValueError("Unsupported JMX modification action: %s" % action)

    def __jmeter(self, jmeter):
        """
        Try to execute JMeter
        """
        self.log.debug("Trying jmeter: %s > %s", jmeter, self.jmeter_log)
        jmout = subprocess.check_output([jmeter, '-j', self.jmeter_log, '--version'], stderr=subprocess.STDOUT)
        self.log.debug("JMeter check: %s", jmout)

    def __check_jmeter(self):
        """
        Checks if JMeter is available, otherwise download and install it.
        """
        jmeter = self.settings.get("path", "~/jmeter-taurus/bin/jmeter" + exe_suffix)
        jmeter = os.path.abspath(os.path.expanduser(jmeter))
        self.settings['path'] = jmeter  # set back after expanding ~

        try:
            self.__jmeter(jmeter)
            return
        except (OSError, CalledProcessError):
            self.log.debug("Failed to run JMeter: %s", traceback.format_exc())
            try:
                jout = subprocess.check_output(["java", '-version'], stderr=subprocess.STDOUT)
                self.log.debug("Java check: %s", jout)
            except BaseException:
                self.log.warning("Failed to run java: %s", traceback.format_exc())
                raise RuntimeError("The 'java' is not operable or not available. Consider installing it")
            self.settings['path'] = self.__install_jmeter(jmeter)
            self.__jmeter(self.settings['path'])

    def __install_jmeter(self, path):
        """
        Installs JMeter and plugins.
        JMeter version, download links (templates) for JMeter and plugins may be set in config:
        for JMeter: "download-link":"http://domain/resource-{version}.zip"
        for plugins: "plugins-download-link": "http://domain/resource-{plugins}.zip"
        JMeter version: "version":"1.2.3"
        """
        # normalize path
        dest = os.path.dirname(os.path.dirname(os.path.expanduser(path)))

        if not dest:
            dest = "jmeter-taurus"
        dest = os.path.abspath(dest)
        jmeter = os.path.join(dest, "bin", "jmeter" + exe_suffix)
        try:
            self.__jmeter(jmeter)
            return jmeter
        except OSError:
            self.log.info("Will try to install JMeter into %s", dest)

        downloader = URLopener()
        jmeter_dist = self.engine.create_artifact("jmeter-dist", ".zip")
        jmeter_download_link = self.settings.get("download-link", JMeterExecutor.JMETER_DOWNLOAD_LINK)
        jmeter_version = self.settings.get("version", JMeterExecutor.JMETER_VER)
        jmeter_download_link = jmeter_download_link.format(version=jmeter_version)
        self.log.info("Downloading %s", jmeter_download_link)

        try:
            downloader.retrieve(jmeter_download_link, jmeter_dist, download_progress_hook)
        except BaseException as exc:
            self.log.error("Error while downloading %s", jmeter_download_link)
            raise exc

        self.log.info("Unzipping %s to %s", jmeter_dist, dest)
        unzip(jmeter_dist, dest, 'apache-jmeter-' + jmeter_version)
        # NOTE: should we remove this file in test environment? or not?
        os.remove(jmeter_dist)

        # TODO: remove old versions for httpclient JARs

        # set exec permissions
        os.chmod(jmeter, 0o755)
        # NOTE: other files like shutdown.sh might also be needed later
        # install plugins
        for set_name in ("Standard", "Extras", "ExtrasLibs", "WebDriver"):
            plugin_dist = self.engine.create_artifact("jmeter-plugin-%s" % set_name, ".zip")
            plugin_download_link = self.settings.get("plugins-download-link", JMeterExecutor.PLUGINS_DOWNLOAD_TPL)
            plugin_download_link = plugin_download_link.format(plugin=set_name)
            self.log.info("Downloading %s", plugin_download_link)
            # TODO: fix socket timeout timer (tcp connection timeout too long)
            try:
                downloader.retrieve(plugin_download_link, plugin_dist, download_progress_hook)
            except BaseException as e:
                self.log.error("Error while downloading %s", plugin_download_link)
                raise e

            self.log.info("Unzipping %s", plugin_dist)
            unzip(plugin_dist, dest)
            os.remove(plugin_dist)

        self.log.info("Installed JMeter and Plugins successfully")
        return jmeter


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

    def __init__(self, original=None):
        self.log = logging.getLogger(self.__class__.__name__)
        if original:
            self.load(original)
        else:
            root = etree.Element("jmeterTestPlan")
            self.tree = etree.ElementTree(root)

            test_plan = etree.Element("TestPlan", guiclass="TestPlanGui",
                                      testname="BZT Generated Test Plan",
                                      testclass="TestPlan")

            htree = etree.Element("hashTree")
            htree.append(test_plan)
            htree.append(etree.Element("hashTree"))
            self.append("jmeterTestPlan", htree)

            element_prop = self._get_arguments_panel(
                "TestPlan.user_defined_variables")
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

    def enabled_thread_groups(self):
        """
        Get thread groups that are enabled
        """
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

        for key, val in six.iteritems(flags):
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
            "threadName": False,
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
        return etree.Element("elementProp",
                             name=name,
                             elementType="Arguments",
                             guiclass="ArgumentsPanel",
                             testclass="Arguments")

    @staticmethod
    def _get_http_request(url, label, method, timeout, body):
        """
        Generates HTTP request
        :type timeout: float
        :type method: str
        :type label: str
        :type url: str
        :rtype: lxml.etree.Element
        """
        proxy = etree.Element("HTTPSamplerProxy", guiclass="HttpTestSampleGui",
                              testclass="HTTPSamplerProxy")
        proxy.set("testname", label)

        args = JMX._get_arguments_panel("HTTPsampler.Arguments")

        # six.u
        if isinstance(body, six.string_types):
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
                http_element_prop.append(JMX._bool_prop("HTTPArgument.always_encode", False))
                http_element_prop.append(JMX._string_prop("Argument.value", arg_value))
                http_element_prop.append(JMX._string_prop("Argument.name", arg_name))
                http_args_coll_prop.append(http_element_prop)
            args.append(http_args_coll_prop)
            proxy.append(args)

        proxy.append(JMX._string_prop("HTTPSampler.path", url))
        proxy.append(JMX._string_prop("HTTPSampler.method", method))
        proxy.append(JMX._bool_prop("HTTPSampler.use_keepalive", True))  # TODO: parameterize it

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
        res.text = str(value)
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
    def _get_thread_group(concurrency=None, rampup=None, iterations=None):
        """
        Generates ThreadGroup with 1 thread and 1 loop

        :param iterations:
        :param rampup:
        :param concurrency:
        :return:
        """
        trg = etree.Element("ThreadGroup", guiclass="ThreadGroupGui",
                            testclass="ThreadGroup", testname="TG")
        loop = etree.Element("elementProp",
                             name="ThreadGroup.main_controller",
                             elementType="LoopController",
                             guiclass="LoopControlPanel",
                             testclass="LoopController")
        loop.append(JMX._bool_prop("LoopController.continue_forever", False))
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

    @staticmethod
    def _get_header_mgr(hdict):
        """

        :type hdict: dict[str,str]
        :rtype: lxml.etree.Element
        """
        mgr = etree.Element("HeaderManager", guiclass="HeaderPanel", testclass="HeaderManager", testname="Headers")

        coll_prop = etree.Element("collectionProp", name="HeaderManager.headers")
        for hname, hval in six.iteritems(hdict):
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
        return mgr

    @staticmethod
    def _get_cookie_mgr():
        """
        :rtype: lxml.etree.Element
        """
        mgr = etree.Element("CookieManager", guiclass="CookiePanel", testclass="CookieManager", testname="Cookies")
        return mgr

    @staticmethod
    def _get_http_defaults(timeout):
        """

        :type timeout: int
        :rtype: lxml.etree.Element
        """
        cfg = etree.Element("ConfigTestElement", guiclass="HttpDefaultsGui",
                            testclass="ConfigTestElement", testname="Defaults")

        params = etree.Element("elementProp",
                               name="HTTPsampler.Arguments",
                               elementType="Arguments",
                               guiclass="HTTPArgumentsPanel",
                               testclass="Arguments")
        cfg.append(params)

        # TODO: have an option for it, with full features (include/exclude, concurrency, etc)
        cfg.append(JMX._bool_prop("HTTPSampler.image_parser", True))

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
    def _get_extractor(varname, regexp, template, match_no, default):
        """

        :type varname: str
        :type regexp: str
        :type template: str
        :type match_no: int
        :type default: str
        :rtype: lxml.etree.Element
        """
        element = etree.Element("RegexExtractor", guiclass="RegexExtractorGui",
                                testclass="RegexExtractor", testname="Get %s" % varname)
        element.append(JMX._string_prop("RegexExtractor.refname", varname))
        element.append(JMX._string_prop("RegexExtractor.regex", regexp))
        element.append(JMX._string_prop("RegexExtractor.template", template))
        element.append(JMX._string_prop("RegexExtractor.match_number", match_no))
        element.append(JMX._string_prop("RegexExtractor.default", default))
        return element

    @staticmethod
    def _get_json_extractor(varname, jsonpath, default):
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
    def _get_resp_assertion(field, contains, is_regexp, is_invert):
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
            item.text = text


class JTLReader(ResultsReader):
    """
    Class to read KPI JTL
    :type errors_reader: JTLErrorsReader
    """

    def __init__(self, filename, parent_logger, errors_filename):
        super(JTLReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        self.indexes = {}
        self.partial_buffer = ""
        self.delimiter = ","
        self.offset = 0
        self.errors_reader = JTLErrorsReader(errors_filename, parent_logger)

    def _read(self, last_pass=False):
        """
        Generator method that returns next portion of data

        :type last_pass: bool
        """
        self.errors_reader.read_file(last_pass)

        while not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            yield None

        self.log.debug("Reading JTL [%s]: %s", os.path.getsize(self.filename), self.filename)

        self.fds.seek(self.offset)  # without this we have a stuck reads on Mac

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

            if not self.indexes:
                self.delimiter = guess_csv_delimiter(line)
                columns = line.strip().split(self.delimiter)
                for idx, field in enumerate(columns):
                    self.indexes[field] = idx
                self.log.debug("Analyzed header line: %s", self.indexes)
                continue

            fields = line.strip().split(self.delimiter)
            label = fields[self.indexes["label"]]
            concur = int(fields[self.indexes["allThreads"]])
            rtm = int(fields[self.indexes["elapsed"]]) / 1000.0
            ltc = int(fields[self.indexes["Latency"]]) / 1000.0
            if "Connect" in self.indexes:
                cnn = int(fields[self.indexes["Connect"]]) / 1000.0
                if cnn < ltc:  # this is generally bad idea...
                    ltc -= cnn  # fixing latency included into connect time
            else:
                cnn = None
            rcd = fields[self.indexes["responseCode"]]
            if rcd.endswith('Exception'):
                rcd = rcd.split('.')[-1]

            if fields[self.indexes["success"]] != "true":
                error = fields[self.indexes["responseMessage"]]
            else:
                error = None

            tstmp = int(int(fields[self.indexes["timeStamp"]]) / 1000)

            yield tstmp, label, concur, rtm, cnn, ltc, rcd, error

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
            logging.debug("Closing file descriptor for %s", self.filename)
            self.fds.close()

    def _calculate_datapoints(self, final_pass=False):
        for point in super(JTLReader, self)._calculate_datapoints(final_pass):
            data = self.errors_reader.get_data(point[DataPoint.TIMESTAMP])

            for label, label_data in six.iteritems(point[DataPoint.CURRENT]):
                if label in data:
                    label_data[KPISet.ERRORS] = data[label]
                else:
                    label_data[KPISet.ERRORS] = {}

            yield point


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
            self.log.debug("Closing file descriptor for %s", self.filename)
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
                self.fds = open(self.filename)  # NOTE: maybe we have the same mac problem with seek() needed
            else:
                return

        self.fds.seek(self.offset)
        self.parser.feed(self.fds.read(1024 * 1024))  # "Huge input lookup" error without capping :)
        self.offset = self.fds.tell()
        for action, elem in self.parser.read_events():
            if elem.getparent() is None or elem.getparent().tag != 'testResults':
                continue

            # extract necessary data
            # TODO: support non-standard samples notation
            ts = int(elem.get("ts")) / 1000
            label = elem.get("lb")
            message = elem.get("rm")
            rc = elem.get("rc")
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

            err_item = KPISet.error_item_skel(message, rc, 1, errtype, url)
            KPISet.inc_list(self.buffer.get(ts).get(label, []), ("msg", message), err_item)
            KPISet.inc_list(self.buffer.get(ts).get('', []), ("msg", message), err_item)

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
        for ts in sorted(self.buffer.keys()):
            if ts > max_ts:
                break
            labels = self.buffer.pop(ts)
            for label, label_data in six.iteritems(labels):
                res = result.get(label, [])
                for err_item in label_data:
                    KPISet.inc_list(res, ('msg', err_item['msg']), err_item)

        return result


class JMeterWidget(urwid.Pile):
    """
    Progress sidebar widget

    :type executor: bzt.modules.jmeter.JMeterExecutor
    """

    def __init__(self, executor):
        self.executor = executor
        self.dur = executor.get_load().duration
        widgets = []
        if self.executor.original_jmx:
            self.script_name = urwid.Text("Script: %s" % os.path.basename(self.executor.original_jmx))
            widgets.append(self.script_name)

        if self.dur:
            self.progress = urwid.ProgressBar('pb-en', 'pb-dis', done=self.dur)
        else:
            self.progress = urwid.Text("Running...")
        widgets.append(self.progress)
        self.elapsed = urwid.Text("Elapsed: N/A")
        self.eta = urwid.Text("ETA: N/A", align=urwid.RIGHT)
        widgets.append(urwid.Columns([self.elapsed, self.eta]))
        super(JMeterWidget, self).__init__(widgets)

    def update(self):
        """
        Refresh widget values
        """
        if self.executor.start_time:
            elapsed = time.time() - self.executor.start_time
            self.elapsed.set_text("Elapsed: %s" % humanize_time(elapsed))

            if self.dur:
                eta = self.dur - elapsed
                if eta >= 0:
                    self.eta.set_text("ETA: %s" % humanize_time(eta))
                else:
                    over = elapsed - self.dur
                    self.eta.set_text("Overtime: %s" % humanize_time(over))
            else:
                self.eta.set_text("")

            if isinstance(self.progress, urwid.ProgressBar):
                self.progress.set_completion(elapsed)

        self._invalidate()


class JMeterScenarioBuilder(JMX):
    """
    Helper to build JMeter test plan from Scenario

    :param original: inherited from JMX
    """

    def __init__(self, original=None):
        super(JMeterScenarioBuilder, self).__init__(original)
        self.scenario = Scenario()

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

    def __add_defaults(self):
        """

        :return:
        """
        # TODO: default hostname and port

        timeout = self.scenario.get("timeout", None)

        if timeout is not None:
            self.append(self.TEST_PLAN_SEL, self._get_http_defaults(int(1000 * dehumanize_time(timeout))))
            self.append(self.TEST_PLAN_SEL, etree.Element("hashTree"))

    def __add_think_time(self, children, request):
        global_ttime = self.scenario.get("think-time", None)
        if request.think_time is not None:
            ttime = int(1000 * dehumanize_time(request.think_time))
        elif global_ttime is not None:
            ttime = int(1000 * dehumanize_time(global_ttime))
        else:
            ttime = None
        if ttime is not None:
            children.append(JMX._get_constant_timer(ttime))
            children.append(etree.Element("hashTree"))

    def __add_extractors(self, children, request):
        extractors = request.config.get("extract-regexp", BetterDict())
        for varname in extractors:
            cfg = ensure_is_dict(extractors, varname, "regexp")
            extractor = JMX._get_extractor(varname, cfg['regexp'], '$%s$' % cfg.get('template', 1),
                                           cfg.get('match-no', 1), cfg.get('default', 'NOT_FOUND'))
            children.append(extractor)
            children.append(etree.Element("hashTree"))

        jextractors = request.config.get("extract-jsonpath", BetterDict())
        for varname in jextractors:
            cfg = ensure_is_dict(jextractors, varname, "jsonpath")
            children.append(JMX._get_json_extractor(
                varname,
                cfg['jsonpath'],
                cfg.get('default', 'NOT_FOUND'))
            )
            children.append(etree.Element("hashTree"))

    def __add_assertions(self, children, request):
        assertions = request.config.get("assert", [])
        for idx, assertion in enumerate(assertions):
            assertion = ensure_is_dict(assertions, idx, "contains")
            if not isinstance(assertion['contains'], list):
                assertion['contains'] = [assertion['contains']]
            children.append(JMX._get_resp_assertion(
                assertion.get("subject", self.FIELD_BODY),
                assertion['contains'],
                assertion.get('regexp', True),
                assertion.get('not', False)
            ))
            children.append(etree.Element("hashTree"))

        jpath_assertions = request.config.get("assert-jsonpath", [])
        for idx, assertion in enumerate(jpath_assertions):
            assertion = ensure_is_dict(jpath_assertions, idx, "jsonpath")

            children.append(JMX._get_json_path_assertion(
                assertion['jsonpath'],
                assertion.get('expected-value', ''),
                assertion.get('validate', False),
                assertion.get('expect-null', False),
                assertion.get('invert', False),
            ))
            children.append(etree.Element("hashTree"))

    def __add_requests(self):
        global_timeout = self.scenario.get("timeout", None)

        for request in self.scenario.get_requests():
            if request.timeout is not None:
                timeout = int(1000 * dehumanize_time(request.timeout))
            elif global_timeout is not None:
                timeout = int(1000 * dehumanize_time(global_timeout))
            else:
                timeout = None

            http = JMX._get_http_request(request.url, request.label, request.method, timeout, request.body)
            self.append(self.THR_GROUP_SEL, http)

            children = etree.Element("hashTree")
            self.append(self.THR_GROUP_SEL, children)
            if request.headers:
                children.append(JMX._get_header_mgr(request.headers))
                children.append(etree.Element("hashTree"))

            self.__add_think_time(children, request)

            self.__add_assertions(children, request)

            if timeout is not None:
                children.append(JMX._get_dur_assertion(timeout))
                children.append(etree.Element("hashTree"))

            self.__add_extractors(children, request)

    def __generate(self):
        """
        Generate the test plan
        """
        # NOTE: set realistic dns-cache and JVM prop by default?
        self.__add_managers()
        self.__add_defaults()
        self.__add_datasources()

        thread_group = JMX._get_thread_group(1, 0, 1)
        self.append(self.TEST_PLAN_SEL, thread_group)
        self.append(self.TEST_PLAN_SEL, etree.Element("hashTree", type="tg"))  # arbitrary trick with our own attribute

        self.__add_requests()
        self.__add_results_tree()

    def save(self, filename):
        """
        Generate test plan and save

        :type filename: str
        """
        # NOTE: bad design, as repetitive save will duplicate stuff
        self.__generate()
        super(JMeterScenarioBuilder, self).save(filename)

    def __add_results_tree(self):
        dbg_tree = etree.Element("ResultCollector",
                                 testname="View Results Tree",
                                 testclass="ResultCollector",
                                 guiclass="ViewResultsFullVisualizer")
        self.append(self.TEST_PLAN_SEL, dbg_tree)
        self.append(self.TEST_PLAN_SEL, etree.Element("hashTree"))

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
            return guess_csv_delimiter(header)
