"""
Module holds all stuff regarding Gatling tool usage
"""
import os
import re
import time
import signal

from bzt.engine import ScenarioExecutor, Scenario
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.utils import shell_exec
import logging
import subprocess

class GatlingExecutor(ScenarioExecutor):
    """
    Gatling executor module
    """

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
        
        self.gatling_log = None
        
        #will be moved to utils.GatlingVerifier
        self.DOWNLOAD_LINK = "http://goo.gl/o14jQg"
        self.VERSION = "2.1.4"
        

    def prepare(self):
        """

        :return:
        """
        scenario = self.get_scenario()
        
        # TODO: install the tool if missing, just like JMeter
        
        #create new artifact in artifacts dir with given prefix and suffix
        #returns str
        self.gatling_log = self.engine.create_artifact("gatling", ".log")
        
        #vill be moved to GutlingVerifier
        self.__check_gutling()
        
        
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
        cmdline = os.path.realpath(self.settings.get("path", "gatling.sh"))
        cmdline += " -sf " + datadir
        cmdline += " -df " + datadir
        cmdline += " -rbf " + datadir
        cmdline += " -bf " + datadir
        cmdline += " -rf " + datadir
        cmdline += " -on gatling-bzt -m"
        cmdline += " -s " + simulation

        self.start_time = time.time()
        out = self.engine.create_artifact("gatling-stdout", ".log")
        err = self.engine.create_artifact("gatling-stderr", ".log")
        self.stdout_file = open(out, "w")
        self.stderr_file = open(err, "w")
        self.process = shell_exec(cmdline, cwd=self.engine.artifacts_dir,
                                  stdout=self.stdout_file,
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
            self.log.debug("Gatling worked for %s seconds",
                           self.end_time - self.start_time)

    def __gatling(self, gatling_full_path):
        """Check if gatling installed"""
        self.log.debug("Trying gatling: %s > %s", gatling_full_path, self.gatling_log)
        gatling_out = subprocess.check_output([gatling_full_path, '-j', self.gatling_log, '--version'], stderr=subprocess.STDOUT)
        self.settings.get()
        
        
    def __check_gatling(self):
        '''Gatling'''

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