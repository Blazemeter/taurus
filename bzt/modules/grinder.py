"""
Module holds all stuff regarding Grinder tool usage

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

from bzt.engine import ScenarioExecutor, Scenario, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, SidebarWidget
from bzt.six import iteritems
from bzt.utils import shell_exec, MirrorsManager
from bzt.utils import unzip, RequiredTool, JavaVM, shutdown_process, TclLibrary


class GrinderExecutor(ScenarioExecutor, WidgetProvider, FileLister):
    """
    Grinder executor module
    """
    # OLD_DOWNLOAD_LINK = "http://switch.dl.sourceforge.net/project/grinder/The%20Grinder%203/{version}" \
    # "/grinder-{version}-binary.zip"
    DOWNLOAD_LINK = "http://sourceforge.net/projects/grinder/files/The%20Grinder%203/{version}" \
                    "/grinder-{version}-binary.zip/download"
    VERSION = "3.11"
    MIRRORS_SOURCE = "http://sourceforge.net/settings/mirror_choices?projectname=grinder&filename=The%20Grinder" \
                     "%203/{version}/grinder-{version}-binary.zip&dialog=true".format(version=VERSION)

    def __init__(self):
        super(GrinderExecutor, self).__init__()
        self.script = None
        self.exec_id = "grinder-bzt-%s" % id(self)
        self.properties_file = None
        self.kpi_file = None
        self.cmd_line = None
        self.process = None
        self.start_time = None
        self.end_time = None
        self.retcode = None
        self.reader = None
        self.stdout_file = None
        self.stderr_file = None
        self.widget = None

    def __write_base_props(self, fds):
        """
        write base properties and base properties file contents to fds
        :param fds: fds
        :return:
        """
        base_props_file = self.settings.get("properties-file", "")
        if base_props_file:
            fds.write("# Base Properies File Start: %s\n" % base_props_file)
            with open(base_props_file) as bpf:
                fds.write(bpf.read())
            fds.write("# Base Properies File End: %s\n\n" % base_props_file)

        # base props
        base_props = self.settings.get("properties")
        if base_props:
            fds.write("# Base Properies Start\n")
            for key, val in iteritems(base_props):
                fds.write("%s=%s\n" % (key, val))
            fds.write("# Base Properies End\n\n")

    def __write_scenario_props(self, fds, scenario):
        """
        Write scenario props and scenario file props to fds
        :param fds:
        :param scenario: dict
        :return:
        """
        script_props_file = scenario.get("properties-file", "")
        if script_props_file:
            fds.write("# Script Properies File Start: %s\n" % script_props_file)
            with open(script_props_file) as spf:
                fds.write(spf.read())
            fds.write("# Script Properies File End: %s\n\n" % script_props_file)

        # scenario props
        local_props = scenario.get("properties")
        if local_props:
            fds.write("# Scenario Properies Start\n")
            for key, val in iteritems(local_props):
                fds.write("%s=%s\n" % (key, val))
            fds.write("# Scenario Properies End\n\n")

    def __write_bzt_props(self, fds):
        """
        Write bzt properties to fds
        :param fds:
        :return:
        """
        fds.write("# BZT Properies Start\n")
        fds.write("grinder.hostID=%s\n" % self.exec_id)
        fds.write("grinder.script=%s\n" % os.path.realpath(self.script).replace(os.path.sep, "/"))
        dirname = os.path.realpath(self.engine.artifacts_dir)
        fds.write("grinder.logDirectory=%s\n" % dirname.replace(os.path.sep, "/"))

        load = self.get_load()
        if load.concurrency:
            if load.ramp_up:
                interval = int(1000 * load.ramp_up / load.concurrency)
                fds.write("grinder.processIncrementInterval=%s\n" % interval)
            fds.write("grinder.processes=%s\n" % int(load.concurrency))
            fds.write("grinder.runs=%s\n" % load.iterations)
            fds.write("grinder.processIncrement=1\n")
            if load.duration:
                fds.write("grinder.duration=%s\n" % int(load.duration * 1000))
        fds.write("# BZT Properies End\n")

    def prepare(self):
        self._check_installed()

        scenario = self.get_scenario()

        if Scenario.SCRIPT in scenario:
            self.script = self.engine.find_file(scenario[Scenario.SCRIPT])
            self.engine.existing_artifact(self.script)
        elif "requests" in scenario:
            self.script = self.__scenario_from_requests()
        else:
            raise ValueError("There must be a scenario file to run Grinder")

        self.properties_file = self.engine.create_artifact("grinder", ".properties")

        with open(self.properties_file, 'w') as fds:
            self.__write_base_props(fds)
            self.__write_scenario_props(fds, scenario)
            self.__write_bzt_props(fds)

        self.kpi_file = os.path.join(self.engine.artifacts_dir, self.exec_id + "-kpi.log")

        self.reader = DataLogReader(self.kpi_file, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

        # add logback configurations used by worker processes (logback-worker.xml)
        classpath = os.path.join(os.path.abspath(os.path.dirname(__file__)), os.pardir, 'resources')

        classpath += os.path.pathsep + os.path.realpath(self.settings.get("path"))

        self.cmd_line = ["java", "-classpath", classpath]
        self.cmd_line += ["net.grinder.Grinder", self.properties_file]

    def startup(self):
        """
        Should start the tool as fast as possible.
        """
        self.start_time = time.time()
        out = self.engine.create_artifact("grinder-stdout", ".log")
        err = self.engine.create_artifact("grinder-stderr", ".log")
        self.stdout_file = open(out, "w")
        self.stderr_file = open(err, "w")

        env = {"T_GRINDER_PREFIX": self.exec_id}
        self.process = self.execute(self.cmd_line, cwd=self.engine.artifacts_dir,
                                    stdout=self.stdout_file,
                                    stderr=self.stderr_file,
                                    env=env)

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
        if self.retcode is not None:
            if self.retcode != 0:
                self.log.info("Grinder exit code: %s", self.retcode)
                raise RuntimeError("Grinder exited with non-zero code")

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
            self.log.debug("Grinder worked for %s seconds", self.end_time - self.start_time)

    def post_process(self):
        """
        Collect data file artifact
        """
        if self.kpi_file:
            self.engine.existing_artifact(self.kpi_file)
        if self.reader and not self.reader.buffer:
            raise RuntimeWarning("Empty results, most likely Grinder failed")

    def __scenario_from_requests(self):
        """
        Generate grinder scenario from requests
        :return: script
        """
        script = self.engine.create_artifact("requests", ".py")
        tpl = os.path.join(os.path.abspath(os.path.dirname(__file__)), os.pardir, 'resources', "grinder-requests.tpl")
        self.log.debug("Generating grinder scenario: %s", tpl)
        with open(script, 'w') as fds:
            with open(tpl) as tds:
                fds.write(tds.read())
            for req in self.get_scenario().get_requests():
                line = '\t\trequest.%s("%s")\n' % (req.method, req.url)
                fds.write(line)
        return script

    def _check_installed(self):
        grinder_path = self.settings.get("path", "~/.bzt/grinder-taurus/lib/grinder.jar")
        grinder_path = os.path.abspath(os.path.expanduser(grinder_path))
        self.settings["path"] = grinder_path
        required_tools = [TclLibrary(self.log),
                          JavaVM("", "", self.log),
                          Grinder(grinder_path, self.log, GrinderExecutor.VERSION)]

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
        resource_files = []
        script_file_path = self.get_scenario().get(Scenario.SCRIPT, None)
        if script_file_path:
            resource_files.append(script_file_path)

        prop_file = self.get_scenario().get("properties-file", None)
        if prop_file:
            resource_files.append(prop_file)

        return resource_files


class DataLogReader(ResultsReader):
    """ Class to read KPI from data log """

    def __init__(self, filename, parent_logger):
        super(DataLogReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        self.idx = {}
        self.partial_buffer = ""
        self.delimiter = ","
        self.offset = 0

    def _read(self, last_pass=False):
        """
        Generator method that returns next portion of data

        :param last_pass:
        """
        while not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            yield None

        self.log.debug("Reading grinder results")
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
            if not fields[1].strip().isdigit():
                self.log.debug("Skipping line: %s", line)
                continue
            t_stamp = int(fields[self.idx["Start time (ms since Epoch)"]]) / 1000.0
            label = ""
            r_time = int(fields[self.idx["Test time"]]) / 1000.0
            latency = int(fields[self.idx["Time to first byte"]]) / 1000.0
            r_code = fields[self.idx["HTTP response code"]].strip()
            con_time = int(fields[self.idx["Time to resolve host"]]) / 1000.0
            con_time += int(fields[self.idx["Time to establish connection"]]) / 1000.0
            if int(fields[self.idx["Errors"]]):
                error = "There were some errors in Grinder test"
            else:
                error = None
            concur = None  # TODO: how to get this for grinder
            yield int(t_stamp), label, concur, r_time, con_time, latency, r_code, error, ''

    def __open_fds(self):
        """
        opens grinder kpi-file
        """
        if not os.path.isfile(self.filename):
            self.log.debug("File not appeared yet")
            return False

        if not os.path.getsize(self.filename):
            self.log.debug("File is empty: %s", self.filename)
            return False

        self.fds = open(self.filename)
        header = self.fds.readline().strip().split(self.delimiter)
        for _ix, field in enumerate(header):
            self.idx[field.strip()] = _ix
        return True

    def __del__(self):
        if self.fds:
            self.fds.close()


class Grinder(RequiredTool):
    def __init__(self, tool_path, parent_logger, version):
        super(Grinder, self).__init__("Grinder", tool_path)
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.version = version
        self.mirror_manager = GrinderMirrorsManager(self.log, self.version)

    def check_if_installed(self):
        self.log.debug("Trying grinder: %s", self.tool_path)
        grinder_launch_command = ["java", "-classpath", self.tool_path, "net.grinder.Grinder"]
        grinder_subprocess = shell_exec(grinder_launch_command, stderr=subprocess.STDOUT)
        output = grinder_subprocess.communicate()
        self.log.debug("%s output: %s", self.tool_name, output)
        if grinder_subprocess.returncode == 0:
            self.already_installed = True
            return True
        else:
            return False

    def install(self):
        dest = os.path.dirname(os.path.dirname(os.path.expanduser(self.tool_path)))
        dest = os.path.abspath(dest)
        grinder_dist = super(Grinder, self).install_with_mirrors(dest, ".zip")
        self.log.info("Unzipping %s", grinder_dist.name)
        unzip(grinder_dist.name, dest, 'grinder-' + self.version)
        grinder_dist.close()
        os.remove(grinder_dist.name)
        self.log.info("Installed grinder successfully")
        if not self.check_if_installed():
            raise RuntimeError("Unable to run %s after installation!" % self.tool_name)


class GrinderMirrorsManager(MirrorsManager):
    def __init__(self, parent_logger, grinder_version):
        self.grinder_version = grinder_version
        super(GrinderMirrorsManager, self).__init__(GrinderExecutor.MIRRORS_SOURCE, parent_logger)

    def _parse_mirrors(self):
        links = []
        if self.page_source is not None:
            self.log.debug('Parsing mirrors...')
            base_link = "http://sourceforge.net/projects/grinder/files/The%20Grinder%203/{version}/grinder-{version}" \
                        "-binary.zip/download?use_mirror={mirror}"
            li_search_pattern = re.compile(r'<li id=".*?">')
            li_elements = li_search_pattern.findall(self.page_source)
            if li_elements:
                links = [base_link.format(version=self.grinder_version, mirror=link.strip('<li id="').strip('">')) for
                         link in li_elements]
        default_link = GrinderExecutor.DOWNLOAD_LINK.format(version=self.grinder_version)
        if default_link not in links:
            links.append(default_link)
        self.log.debug('Total mirrors: %d', len(links))
        return links
