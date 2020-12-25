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
from bzt.utils import dehumanize_time


class ExternalResultsLoader(ScenarioExecutor, AggregatorListener):
    """
    :type reader: bzt.modules.aggregator.ResultsReader
    """
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
        self._last_update_ts = -1
        self._last_datapoint_ts = -1
        self._prev_datapoint_ts = -1
        self._file_check_ts = time.time()

    def prepare(self):
        super(ExternalResultsLoader, self).prepare()
        self._read_options()
        assert self._data_file_pattern or self.data_file, "Option is required: data-file or data-file-pattern"
        self.label = self.data_file
        if self.errors_file:
            self.errors_file = self.engine.find_file(self.errors_file)

        str_wait = self.execution.get("wait-for-file", self.settings.get("wait-for-file", self._file_exists_wait))
        self._file_exists_wait = dehumanize_time(str_wait)

        def_timout = self.engine.check_interval * 10
        str_to = self.execution.get("results-timeout", self.settings.get("results-timeout", def_timout))
        self._result_timeout = dehumanize_time(str_to)

        self._file_check_ts = time.time()
        self._try_make_reader()

    def _read_options(self):
        # read from scenario
        if 'scenario' in self.execution:
            scenario = self.get_scenario()
            self.data_file = scenario.get('data-file', self.data_file)
            self.errors_file = scenario.get('errors-file', self.errors_file)
            self._data_file_pattern = scenario.get("data-file-pattern", self._data_file_pattern)

        # execution level overrides scenario level
        self.data_file = self.execution.get("data-file", self.data_file)
        self.errors_file = self.execution.get("errors-file", self.errors_file)
        self._data_file_pattern = self.execution.get("data-file-pattern", self._data_file_pattern)

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
            reader = TSVDataReader(self.data_file, self.log)
            reader.url_label = "N/A"
            return reader
        elif header.startswith("<?xml"):
            return XMLJTLReader(self.data_file, self.log)
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
        self._last_datapoint_ts = data[DataPoint.TIMESTAMP]
        self._last_update_ts = time.time()
        # self.log.debug("Notified of datapoint %s", self._last_datapoint_ts)

    def startup(self):
        super(ExternalResultsLoader, self).startup()
        self._try_make_reader()

    def check(self):
        self._try_make_reader()
        ts_not_changed = self._last_datapoint_ts == self._prev_datapoint_ts
        no_new_results = time.time() - self._last_update_ts > self._result_timeout
        has_read_some = self._last_datapoint_ts > 0 or bool(self.reader and self.reader.buffer)
        # self.log.info("%s %s %s", self._last_datapoint_ts, self._prev_datapoint_ts, self._last_update_ts)
        if has_read_some and ts_not_changed and no_new_results:
            return True
        else:
            self._prev_datapoint_ts = self._last_datapoint_ts

    def get_resource_files(self):
        self._read_options()
        files = []
        if self.data_file:
            files.append(self.data_file)
        if self.errors_file:
            files.append(self.errors_file)
        return super(ExternalResultsLoader, self).get_resource_files() + files
