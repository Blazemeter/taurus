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
import time
import signal
import subprocess
from subprocess import CalledProcessError
import traceback
import platform
import shutil

import urwid

from bzt.engine import ScenarioExecutor, Scenario, FileLister
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.utils import shell_exec, ensure_is_dict
from bzt.utils import unzip, download_progress_hook, humanize_time
from bzt.modules.console import WidgetProvider


try:
    from urllib import FancyURLopener
except ImportError:
    from urllib.request import FancyURLopener

exe_suffix = ".bat" if platform.system() == 'Windows' else ".sh"


class GatlingExecutor(ScenarioExecutor, WidgetProvider, FileLister):
    """
    Gatling executor module
    """
    # NOTE: will be moved to GatlingVerifier
    DOWNLOAD_LINK = "https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle" \
                    "/{version}/gatling-charts-highcharts-bundle-{version}-bundle.zip"
    VERSION = "2.1.4"

    def __init__(self):
        super(GatlingExecutor, self).__init__()
        self.script = None
        self.process = None
        self.start_time = None
        self.end_time = None
        self.retcode = None
        self.reader = None
        self.stdout_file = None
        self.stderr_file = None
        self.widget = None

    def prepare(self):
        """

        :return:
        """
        scenario = self.get_scenario()

        # TODO: will be moved to GatlingVerifier
        self.__check_gatling()

        if Scenario.SCRIPT in scenario:
            self.script = self.engine.find_file(scenario[Scenario.SCRIPT])
            self.engine.existing_artifact(self.script)
            with open(os.path.join(self.engine.artifacts_dir, os.path.basename(self.script)), 'rt') as fds:
                script_contents = fds.read()
            resource_files = self.__get_resource_files_from_script(script_contents)
            if resource_files:
                modified_contents = self.__modify_resources_paths_in_scala(script_contents, resource_files)
                with open(os.path.join(self.engine.artifacts_dir, os.path.basename(self.script)), 'wt') as fds:
                    fds.write(modified_contents)
                self.__copy_resources_to_artifacts_dir(resource_files)

        elif "requests" in scenario:
            raise NotImplementedError()  # TODO: implement script generation for gatling
        else:
            raise ValueError("There must be a scenario file to run Gatling")

        self.reader = DataLogReader(self.engine.artifacts_dir, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def startup(self):
        """
        Should start the tool as fast as possible.
        """

        simulation = self.get_scenario().get("simulation", "")
        if not simulation:
            # TODO: guess simulation from script file
            raise NotImplementedError()

        datadir = os.path.realpath(self.engine.artifacts_dir)

        # NOTE: exe_suffix already in "path"
        cmdline = [self.settings["path"]]
        cmdline += ["-sf", datadir, "-df", datadir, " -rf ", datadir]
        cmdline += ["-on", "gatling-bzt", "-m", "-s", simulation]

        self.start_time = time.time()
        out = self.engine.create_artifact("gatling-stdout", ".log")
        err = self.engine.create_artifact("gatling-stderr", ".log")
        self.stdout_file = open(out, "w")
        self.stderr_file = open(err, "w")

        self.process = shell_exec(cmdline, cwd=self.engine.artifacts_dir, stdout=self.stdout_file,
                                  stderr=self.stderr_file)

    def post_process(self):
        """
        Save data log as artifact
        """
        if self.reader.filename:
            self.engine.existing_artifact(self.reader.filename)

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
                self.log.info("Gatling tool exit code: %s", self.retcode)
                raise RuntimeError("Gatling tool exited with non-zero code")

            if not self.reader.filename:
                msg = "No simulation.log, most likely the tool failed to run"
                raise RuntimeWarning(msg)
            return True
        return False

    def shutdown(self):
        """
        If tool is still running - let's stop it.
        """
        while self.process and self.process.poll() is None:
            self.log.info("Terminating Gatling PID: %s", self.process.pid)
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
            self.log.debug("Gatling worked for %s seconds", self.end_time - self.start_time)

    def __gatling(self, gatling_full_path):
        """Check if Gatling installed"""

        self.log.debug("Trying Gatling: %s", gatling_full_path)
        gatling_output = subprocess.check_output([gatling_full_path, '--help'], stderr=subprocess.STDOUT)
        self.log.debug("Gatling check: %s", gatling_output)

    def __check_gatling(self):
        """
        Checks if Gatling is available, otherwise download and install it.
        """
        # NOTE: file extension should be in config
        gatling_path = self.settings.get("path", "~/gatling-taurus/bin/gatling" + exe_suffix)
        gatling_path = os.path.abspath(os.path.expanduser(gatling_path))
        self.settings["path"] = gatling_path

        try:
            self.__gatling(gatling_path)
            return
        except (OSError, CalledProcessError) as exc:
            self.log.debug("Failed to run Gatling: %s", traceback.format_exc())
            try:
                jout = subprocess.check_output(["java", '-version'], stderr=subprocess.STDOUT)
                self.log.debug("Java check: %s", jout)
            except BaseException as exc:
                self.log.warning("Failed to run java: %s", traceback.format_exc())
                raise RuntimeError("The 'java' is not operable or not available. Consider installing it")

            self.__install_gatling(gatling_path)
            self.__gatling(self.settings['path'])

    def __install_gatling(self, gatling_path):
        """
        Installs Gatling.
        Gatling version and download link may be set in config:
        "download-link":"http://domain/resource-{version}.zip"
        "version":"1.2.3"
        """
        dest = os.path.dirname(os.path.dirname(os.path.expanduser(gatling_path)))  # ../..
        dest = os.path.abspath(dest)

        try:
            self.__gatling(gatling_path)
            return gatling_path
        except OSError:
            self.log.info("Will try to install Gatling into %s", dest)

        # download gatling
        downloader = FancyURLopener()
        gatling_zip_path = self.engine.create_artifact("gatling-dist", ".zip")
        version = self.settings.get("version", GatlingExecutor.VERSION)
        download_link = self.settings.get("download-link", GatlingExecutor.DOWNLOAD_LINK)
        download_link = download_link.format(version=version)
        self.log.info("Downloading %s", download_link)
        # TODO: check archive checksum/hash before unzip and run

        try:
            downloader.retrieve(download_link, gatling_zip_path, download_progress_hook)
        except BaseException as e:
            self.log.error("Error while downloading %s", download_link)
            raise e

        self.log.info("Unzipping %s", gatling_zip_path)
        unzip(gatling_zip_path, dest, 'gatling-charts-highcharts-bundle-' + version)
        os.remove(gatling_zip_path)
        os.chmod(os.path.expanduser(gatling_path), 0o755)
        self.log.info("Installed Gatling successfully")

    def get_widget(self):
        if not self.widget:
            self.widget = GatlingWidget(self)
        return self.widget

    def resource_files(self):
        script = self.__get_script()
        resource_files = []

        if script:
            script_contents = open(script, 'rt').read()
            resource_files = self.__get_resource_files_from_script(script_contents)

            if resource_files:

                script_name, script_ext = os.path.splitext(script)
                script_name = os.path.basename(script_name)
                modified_script = self.engine.create_artifact(script_name, script_ext)
                modified_contents = self.__modify_resources_paths_in_scala(script_contents, resource_files)
                self.__copy_resources_to_artifacts_dir(resource_files)
                with open(modified_script, 'wt') as _fds:
                    _fds.write(modified_contents)
                resource_files.append(modified_script)
            else:
                shutil.copy2(script, self.engine.artifacts_dir)
                resource_files.append(script)

        return [os.path.basename(file_path) for file_path in resource_files]

    def __get_resource_files_from_script(self, script_contents):
        resource_files = []
        search_patterns = [re.compile('\.formUpload\(".*?"\)'),
                           re.compile('RawFileBody\(".*?"\)'),
                           re.compile('RawFileBodyPart\(".*?"\)'),
                           re.compile('ELFileBody\(".*?"\)'),
                           re.compile('ELFileBodyPart\(".*?"\)'),
                           re.compile('csv\(".*?"\)'),
                           re.compile('tsv\(".*?"\)'),
                           re.compile('ssv\(".*?"\)'),
                           re.compile('jsonFile\(".*?"\)'),
                           re.compile('separatedValues\(".*?"\)')]
        for search_pattern in search_patterns:
            found_samples = search_pattern.findall(script_contents)
            for found_sample in found_samples:
                param_list = found_sample.split(",")
                param_index = 0 if "separatedValues" in search_pattern.pattern else -1  # first or last param
                file_path = re.compile('\".*?\"').findall(param_list[param_index])[0].strip('"')
                resource_files.append(file_path)

        return resource_files

    def __modify_resources_paths_in_scala(self, scala_script_contents, file_list):
        """

        :param scala_path:
        :param file_list:
        :return:
        """
        for file_path in file_list:
            scala_script_contents = scala_script_contents.replace(file_path, os.path.basename(file_path))
        return scala_script_contents


    def __copy_resources_to_artifacts_dir(self, resource_files_list):
        """

        :param file_list:
        :return:
        """
        for resource_file in resource_files_list:
            if os.path.exists(resource_file):
                try:
                    shutil.copy(resource_file, self.engine.artifacts_dir)
                except:
                    self.log.warning("Cannot copy file: %s" % resource_file)
            else:
                self.log.warning("File not found: %s" % resource_file)

    def __get_script(self):
        scenario = self.get_scenario()
        if Scenario.SCRIPT not in scenario:
            return None

        ensure_is_dict(scenario, Scenario.SCRIPT, "path")
        fname = scenario[Scenario.SCRIPT]["path"]
        if fname is not None:
            return self.engine.find_file(fname)
        else:
            return None


class DataLogReader(ResultsReader):
    """ Class to read KPI from data log """

    def __init__(self, basedir, parent_logger):
        super(DataLogReader, self).__init__()
        self.concurrency = 0
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.basedir = basedir
        self.filename = False
        self.fds = None
        self.partial_buffer = ""
        self.delimiter = "\t"
        self.offset = 0

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
                self.log.debug("Skipping line: %s", line)
                continue
            self.log.debug("Accept line: %s", line)
            label = fields[4]
            ts = int(fields[8]) / 1000.0

            rt = (int(fields[8]) - int(fields[5])) / 1000.0
            lt = (int(fields[7]) - int(fields[5])) / 1000.0
            cn = (int(fields[6]) - int(fields[5])) / 1000.0

            if fields[-1] == 'OK':
                rc = '200'
            else:
                _tmp_rc = fields[-1].split(" ")[-1]
                rc = _tmp_rc if _tmp_rc.isdigit() else 'No RC'

            if len(fields) >= 11 and fields[10]:
                error = fields[10]
            else:
                error = None

            yield int(ts), label, self.concurrency, rt, cn, lt, rc, error

    def __open_fds(self):
        prog = re.compile("^gatling-bzt-[0-9]+$")

        for fname in os.listdir(self.basedir):
            if prog.match(fname):
                self.filename = os.path.join(self.basedir, fname, "simulation.log")
                break

        if not self.filename:
            self.log.debug("File is empty: %s", self.filename)
            return False

        self.fds = open(self.filename)
        return True


class GatlingWidget(urwid.Pile):
    """
    Progress sidebar widget

    :type executor: bzt.modules.grinder.GatlingExecutor
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
        super(GatlingWidget, self).__init__(widgets)

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