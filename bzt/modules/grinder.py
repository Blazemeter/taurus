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
import time
import signal
import subprocess
from subprocess import CalledProcessError
import traceback
import six
import re
import shutil

import urwid

from bzt.engine import ScenarioExecutor, Scenario, FileLister

from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader

from bzt.utils import shell_exec
from bzt.utils import unzip, download_progress_hook, humanize_time
from bzt.modules.console import WidgetProvider


try:
    from urllib import FancyURLopener
except ImportError:
    from urllib.request import FancyURLopener


class GrinderExecutor(ScenarioExecutor, WidgetProvider, FileLister):
    """
    Grinder executor module
    """
    DOWNLOAD_LINK = "http://switch.dl.sourceforge.net/project/grinder/The%20Grinder%203/{version}" \
                    "/grinder-{version}-binary.zip"
    VERSION = "3.11"

    def __init__(self):
        super(GrinderExecutor, self).__init__()
        self.script = None

        self.properties_file = None
        self.kpi_file = None
        self.process = None
        self.start_time = None
        self.end_time = None
        self.retcode = None
        self.reader = None
        self.stdout_file = None
        self.stderr_file = None
        self.widget = None

    def __write_base_props(self, fds):
        # base props file
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
            for key, val in six.iteritems(base_props):
                fds.write("%s=%s\n" % (key, val))
            fds.write("# Base Properies End\n\n")

    def __write_scenario_props(self, fds, scenario):
        # scenario props file
        script_props_file = scenario.get("properties-file", "")
        if script_props_file:
            fds.write(
                "# Script Properies File Start: %s\n" % script_props_file)
            with open(script_props_file) as spf:
                fds.write(spf.read())
            fds.write(
                "# Script Properies File End: %s\n\n" % script_props_file)

        # scenario props
        local_props = scenario.get("properties")
        if local_props:
            fds.write("# Scenario Properies Start\n")
            for key, val in six.iteritems(local_props):
                fds.write("%s=%s\n" % (key, val))
            fds.write("# Scenario Properies End\n\n")

    def __write_bzt_props(self, fds):
        # BZT props
        fds.write("# BZT Properies Start\n")
        fds.write("grinder.hostID=grinder-bzt\n")
        fds.write("grinder.script=%s\n" % os.path.realpath(self.script))
        dirname = os.path.realpath(self.engine.artifacts_dir)
        fds.write("grinder.logDirectory=%s\n" % dirname)

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
        """

        :return:
        """
        scenario = self.get_scenario()

        self.__check_grinder()

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

        # FIXME: multi-grinder executions have different names
        self.kpi_file = os.path.join(self.engine.artifacts_dir, "grinder-bzt-kpi.log")

        self.reader = DataLogReader(self.kpi_file, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def startup(self):
        """
        Should start the tool as fast as possible.
        """
        cmdline = ["java", "-classpath",
                   os.path.dirname(__file__) + os.path.pathsep + os.path.realpath(self.settings.get("path"))]
        cmdline += ["net.grinder.Grinder", self.properties_file]

        self.start_time = time.time()
        out = self.engine.create_artifact("grinder-stdout", ".log")
        err = self.engine.create_artifact("grinder-stderr", ".log")
        self.stdout_file = open(out, "w")
        self.stderr_file = open(err, "w")
        self.process = shell_exec(cmdline, cwd=self.engine.artifacts_dir,
                                  stdout=self.stdout_file,
                                  stderr=self.stderr_file)

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

            if self.kpi_file:
                if not os.path.exists(self.kpi_file) \
                        or not os.path.getsize(self.kpi_file):
                    msg = "Empty results log, most likely the tool failed: %s"
                    raise RuntimeWarning(msg % self.kpi_file)

            return True
        return False

    def post_process(self):
        """
        Collect data file artifact
        """
        if self.kpi_file:
            self.engine.existing_artifact(self.kpi_file)

    def shutdown(self):
        """
        If tool is still running - let's stop it.
        """
        while self.process and self.process.poll() is None:
            self.log.info("Terminating Grinder PID: %s", self.process.pid)
            time.sleep(1)
            try:
                os.killpg(self.process.pid, signal.SIGTERM)
            except OSError as exc:
                self.log.debug("Failed to terminate: %s", exc)

            if self.stdout_file:
                self.stdout_file.close()
            if self.stderr_file:
                self.stderr_file.close()

        if self.start_time:
            self.end_time = time.time()
            self.log.debug("Grinder worked for %s seconds",
                           self.end_time - self.start_time)

    def __scenario_from_requests(self):
        script = self.engine.create_artifact("requests", ".py")
        tpl = os.path.join(os.path.dirname(__file__), "grinder-requests.tpl")
        self.log.debug("Generating grinder scenario: %s", tpl)
        with open(script, 'w') as fds:
            with open(tpl) as tds:
                fds.write(tds.read())
            for request in self.get_scenario().get_requests():
                line = '\t\trequest.%s("%s")\n' % (request.method, request.url)
                fds.write(line)
        return script

    def __grinder(self, grinder_full_path):
        """Check if grinder installed"""
        # java -classpath /home/user/Downloads/grinder-3.11/lib/grinder.jar net.grinder.Grinder --help
        # CHECK ERRORLEVEL TO BE SURE
        self.log.debug("Trying grinder: %s", grinder_full_path)
        grinder_launch_command = ["java", "-classpath", grinder_full_path, "net.grinder.Grinder"]
        # print "grinder_launch command:", grinder_launch_command
        grinder_subprocess = subprocess.Popen(grinder_launch_command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        _process_output = grinder_subprocess.communicate()[0]

        if grinder_subprocess.returncode != 0:
            raise CalledProcessError(grinder_subprocess.returncode, " ".join(grinder_launch_command))
        # grinder_subprocess = subprocess.check_output(["java -classpath " + grinder_full_path + "grinder.jar net.grinder.Grinder", '--help'], stderr=subprocess.STDOUT)
        self.log.debug("grinder check: %s", _process_output)

    def __check_grinder(self):
        """
        Checks if Grinder is available, otherwise download and install it.
        """
        grinder_path = self.settings.get("path", "~/grinder-taurus/lib/grinder.jar")
        grinder_path = os.path.abspath(os.path.expanduser(grinder_path))
        self.settings['path'] = grinder_path

        try:
            self.__grinder(grinder_path)
            return
        except (OSError, CalledProcessError) as exc:
            self.log.debug("Failed to run grinder: %s", traceback.format_exc())

            try:
                jout = subprocess.check_output(["java", '-version'], stderr=subprocess.STDOUT)
                self.log.debug("Java check: %s", jout)
            except BaseException as exc:
                self.log.warning("Failed to run java: %s", traceback.format_exc())
                raise RuntimeError("The 'java' is not operable or not available. Consider installing it")

            self.settings['path'] = self.__install_grinder(grinder_path)
            self.__grinder(self.settings['path'])

    def __install_grinder(self, grinder_path):
        """
        Installs Grinder.
        Grinder version and download link may be set in config:
        "download-link":"http://domain/resource-{version}.zip"
        "version":"1.2.3"
        """

        dest = os.path.dirname(os.path.dirname(os.path.expanduser(grinder_path)))
        if not dest:
            dest = os.path.expanduser("~/grinder-taurus")
        dest = os.path.abspath(dest)
        grinder_full_path = os.path.join(dest, "lib", "grinder.jar")
        try:
            self.__grinder(grinder_full_path)
            return grinder_full_path
        except CalledProcessError:
            self.log.info("Will try to install grinder into %s", dest)

        downloader = FancyURLopener()
        grinder_zip_path = self.engine.create_artifact("grinder-dist", ".zip")
        version = self.settings.get("version", GrinderExecutor.VERSION)
        download_link = self.settings.get("download-link", GrinderExecutor.DOWNLOAD_LINK)
        download_link = download_link.format(version=version)
        self.log.info("Downloading %s", download_link)

        try:
            downloader.retrieve(download_link, grinder_zip_path, download_progress_hook)
        except BaseException as e:
            self.log.error("Error while downloading %s", download_link)
            raise e

        self.log.info("Unzipping %s", grinder_zip_path)
        unzip(grinder_zip_path, dest, 'grinder-' + version)
        os.remove(grinder_zip_path)
        self.log.info("Installed grinder successfully")
        return grinder_full_path

    def get_widget(self):
        if not self.widget:
            self.widget = GrinderWidget(self)
        return self.widget

    def resource_files(self):
        """

        :return:
        """
        resource_files = []
        prop_file = self.get_scenario().get("properties-file")

        if prop_file:
            prop_file_contents = open(prop_file, 'rt').read()
            resource_files, modified_contents = self.__get_resource_files_from_script(prop_file_contents)
            if resource_files:
                for resource_file in resource_files:
                    if os.path.exists(resource_file):
                        try:
                            shutil.copy(resource_file, self.engine.artifacts_dir)
                        except:
                            self.log.warning("Cannot copy file: %s" % resource_file)
                    else:
                        self.log.warning("File not found: %s" % resource_file)

                script_name, script_ext = os.path.splitext(prop_file)
                script_name = os.path.basename(script_name)
                modified_script = self.engine.create_artifact(script_name, script_ext)
                with open(modified_script, 'wt') as _fds:
                    _fds.write(modified_contents)
                resource_files.append(modified_script)
            else:
                shutil.copy2(prop_file, self.engine.artifacts_dir)
                resource_files.append(prop_file)

        return [os.path.basename(x) for x in resource_files]

    def __get_resource_files_from_script(self, prop_file_contents):
        """
        if "script" in scenario:
            add script file to resources and override script name in .prop file
        else:
            take script name from .prop file and add it to resources

        :param prop_file_contents:
        :return: list of resource files and contents of .prop file
        """
        resource_files = []
        script_file_path = self.get_scenario().get("script")

        search_pattern = re.compile("grinder\.script.*")
        found_pattern = search_pattern.findall(prop_file_contents)[-1]  # take last
        file_path_in_prop = found_pattern.split("=")[-1].strip()

        if script_file_path:
            resource_files.append(script_file_path)
            prop_file_contents = prop_file_contents.replace(file_path_in_prop, os.path.basename(script_file_path))
        else:
            resource_files.append(file_path_in_prop)

        return resource_files, prop_file_contents


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
            ts = int(fields[self.idx["Start time (ms since Epoch)"]]) / 1000.0
            label = ""
            rt = int(fields[self.idx["Test time"]]) / 1000.0
            lt = int(fields[self.idx["Time to first byte"]]) / 1000.0
            rc = fields[self.idx["HTTP response code"]].strip()
            cn = int(fields[self.idx["Time to resolve host"]]) / 1000.0
            cn += int(fields[self.idx["Time to establish connection"]]) / 1000.0
            if int(fields[self.idx["Errors"]]):
                error = "There were some errors in Grinder test"
            else:
                error = None
            concur = None  # TODO: how to get this for grinder
            yield int(ts), label, concur, rt, cn, lt, rc, error

    def __open_fds(self):
        if not os.path.isfile(self.filename):
            self.log.debug("File not appeared yet")
            return False

        if not os.path.getsize(self.filename):
            self.log.debug("File is empty: %s", self.filename)
            return False

        self.fds = open(self.filename)
        header = self.fds.readline().strip().split(self.delimiter)
        for ix, field in enumerate(header):
            self.idx[field.strip()] = ix
        return True


class GrinderWidget(urwid.Pile):
    """
    Progress sidebar widget

    :type executor: bzt.modules.grinder.GrinderExecutor
    """

    def __init__(self, executor):
        self.executor = executor
        self.dur = executor.get_load().duration
        widgets = []
        if self.executor.script:
            self.script_name = urwid.Text("Script: %s" % os.path.basename(self.executor.script))
            widgets.append(self.script_name)
        if self.dur:
            self.progress = urwid.ProgressBar('pb-en', 'pb-dis', done=self.dur)
        else:
            self.progress = urwid.Text("Running...")
        widgets.append(self.progress)
        self.elapsed = urwid.Text("Elapsed: N/A")
        self.eta = urwid.Text("ETA: N/A", align=urwid.RIGHT)
        widgets.append(urwid.Columns([self.elapsed, self.eta]))
        super(GrinderWidget, self).__init__(widgets)

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