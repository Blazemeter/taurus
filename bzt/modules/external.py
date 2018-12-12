import os
import re
import time
from glob import glob

from bzt import TaurusInternalException
from bzt.engine import ScenarioExecutor
from bzt.modules.ab import TSVDataReader
from bzt.modules.aggregator import AggregatorListener, ConsolidatingAggregator, DataPoint
from bzt.modules.gatling import DataLogReader as GatlingLogReader
from bzt.modules.grinder import DataLogReader as GrinderLogReader
from bzt.modules.jmeter import JTLReader, XMLJTLReader
from bzt.modules.pbench import PBenchKPIReader
from bzt.utils import dehumanize_time


class ExternalResultsLoader(ScenarioExecutor, AggregatorListener):
    AB_HEADER = "starttime\tseconds\tctime\tdtime\tttime\twait"
    PBENCH_FORMAT = re.compile("^[0-9]+\.[0-9]{3}\t[^\t]*\t([0-9]+\t){9}[0-9]+$")

    def __init__(self):
        # TODO: document this executor
        super(ExternalResultsLoader, self).__init__()
        self._result_timeout = 1
        self._file_exists_wait = 1
        self._data_file_pattern = None
        self.data_file = None
        self.errors_file = None
        self.reader = None
        self._last_ts = -1
        self._prev_ts = -1
        self._file_check_ts = time.time()

    def prepare(self):
        self.data_file = self.execution.get("data-file", self.data_file)
        self._data_file_pattern = self.execution.get("data-file-pattern", self._data_file_pattern)
        assert self._data_file_pattern or self.data_file, "Option is required: data-file or data-file-pattern"
        self.label = self.data_file
        self.errors_file = self.execution.get("errors-jtl", None)
        if self.errors_file:
            self.errors_file = self.engine.find_file(self.errors_file)

        str_wait = self.execution.get("wait-for-file", self.settings.get("wait-for-file", self._file_exists_wait))
        self._file_exists_wait = dehumanize_time(str_wait)

        str_to = self.execution.get("results-timeout", self.settings.get("results-timeout", self._result_timeout))
        self._result_timeout = dehumanize_time(str_to)

        self._file_check_ts = time.time()
        self._try_make_reader()

    def _try_make_reader(self):
        if self.reader:
            return

        if not self.data_file:
            files = glob(self._data_file_pattern)
            if not files:
                return
            files.sort()
            self.log.debug("Files found by pattern: %s", files)
            if not os.path.getsize(files[-1]):
                return
            self.data_file = files[-1]

        self.data_file = self.engine.find_file(self.data_file)
        if not os.path.exists(self.data_file):
            if time.time() - self._file_check_ts < self._file_exists_wait:
                self.log.debug("File not exists yet: %s", self.data_file)
                return
            else:
                msg = "File has not appeared within %ss: %s" % (self._file_exists_wait, self.data_file)
                raise TaurusInternalException(msg)

        self.log.info("Will load external results from file: %s", self.data_file)
        self.label = self.data_file

        self.reader = self._get_reader()
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)
            self.engine.aggregator.add_listener(self)

    def _get_reader(self):
        with open(self.data_file) as fhd:
            header = fhd.readline(2048).strip()  # just header chunk of file

        # TODO: detect CSV dialect for JTLs

        if header.startswith(self.AB_HEADER):
            return TSVDataReader(self.data_file, self.log)
        elif header.startswith("<?xml"):
            return XMLJTLReader(self.data_file, self.log)
        elif self.PBENCH_FORMAT.match(header):
            return PBenchKPIReader(self.data_file, self.log, self.errors_file)
        elif header.startswith("RUN\t") or "\tRUN\t" in header:
            return GatlingLogReader(self.data_file, self.log, None)
        elif "timestamp" in header.lower() and "elapsed" in header.lower():
            return JTLReader(self.data_file, self.log, self.errors_file)
        elif "worker process" in header.lower() and header.startswith("worker."):
            return GrinderLogReader(self.data_file, self.log)
        else:
            self.log.info("Header line was: %s", header)
            raise TaurusInternalException("Unable to detect results format for: %s" % self.data_file)

    def aggregated_second(self, data):
        self._last_ts = data[DataPoint.TIMESTAMP]

    def startup(self):
        super(ExternalResultsLoader, self).startup()
        self._try_make_reader()

    def check(self):
        self._try_make_reader()
        if self._last_ts > 0 and self._last_ts == self._prev_ts and time.time() - self._last_ts > self._result_timeout:
            return True
        else:
            self._prev_ts = self._last_ts
