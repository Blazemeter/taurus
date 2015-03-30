"""
Module holds all stuff regarding Gatling tool usage
"""
import os
import re
import time
import signal
import subprocess
from subprocess import CalledProcessError
import traceback
import urllib
import platform

from bzt.engine import ScenarioExecutor, Scenario
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.utils import shell_exec
from bzt.utils import unzip, download_progress_hook

exe_suffix = ".bat" if platform.system() == 'Windows' else ".sh"


class GatlingExecutor(ScenarioExecutor):
    """
    Gatling executor module
    """
    # NOTE: will be moved to GatlingVerifier
    DOWNLOAD_LINK = "https://repo1.maven.org/maven2/io/gatling/highcharts/gatling-charts-highcharts-bundle/"\
            "{version}/gatling-charts-highcharts-bundle-{version}-bundle.zip"
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
        cmdline = self.settings["path"]
        cmdline += " -sf " + datadir + " -df " + datadir + " -rf " + datadir
        cmdline += " -on gatling-bzt -m  -s " + simulation

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
            except OSError, exc:
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
        except (OSError, CalledProcessError), exc:
            self.log.debug("Failed to run Gatling: %s", traceback.format_exc(exc))
            try:
                jout = subprocess.check_output(["java", '-version'], stderr=subprocess.STDOUT)
                self.log.debug("Java check: %s", jout)
            except BaseException, exc:
                self.log.warn("Failed to run java: %s", traceback.format_exc(exc))
                raise RuntimeError("The 'java' is not operable or not available. Consider installing it")

            self.__install_gatling(gatling_path)
            self.__gatling(self.settings['path'])

    def __install_gatling(self, gatling_path):
        """
        Installs Gatling.
        Gatling version and download link may be set in config:
        "download-link":"http://blah-{version}.zip"
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
        downloader = urllib.FancyURLopener()
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
        os.chmod(os.path.expanduser(gatling_path), 0755)
        self.log.info("Installed Gatling successfully")


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

    def _read(self, last_pass=False):
        """
        Generator method that returns next portion of data

        :param last_pass:
        """
        while not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            yield None

        self.log.debug("Reading gatling results")
        if last_pass:
            lines = self.fds.readlines()  # unlimited
        else:
            lines = self.fds.readlines(1024 * 1024)  # 1MB limit to read

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

            if fields[0] == 'OK':
                rc = '200'
            else:
                # TODO: we can do more intelligent analysis here for some cases
                rc = "500"

            if len(fields) >= 11 and fields[10]:
                error = fields[10]
            else:
                error = None

            yield int(ts), label, self.concurrency, rt, cn, lt, rc, error

    def __open_fds(self):
        prog = re.compile("^gatling-bzt-[0-9]+$")

        for fname in os.listdir(self.basedir):
            if prog.match(fname):
                self.filename = self.basedir + os.path.sep + fname
                self.filename += os.path.sep + "simulation.log"
                break

        if not self.filename:
            self.log.debug("File is empty: %s", self.filename)
            return False

        self.fds = open(self.filename)
        return True