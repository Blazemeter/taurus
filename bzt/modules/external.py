import re

from bzt import TaurusInternalException, TaurusConfigError
from bzt.engine import ScenarioExecutor
from bzt.modules.ab import TSVDataReader
from bzt.modules.aggregator import AggregatorListener, ConsolidatingAggregator, DataPoint
from bzt.modules.gatling import DataLogReader as GatlingLogReader
from bzt.modules.grinder import DataLogReader as GrinderLogReader
from bzt.modules.jmeter import JTLReader, XMLJTLReader
from bzt.modules.pbench import PBenchKPIReader


class ExternalResultsLoader(ScenarioExecutor, AggregatorListener):
    AB_HEADER = "starttime\tseconds\tctime\tdtime\tttime\twait"
    PBENCH_FORMAT = re.compile("^[0-9]+\.[0-9]{3}\t[^\t]*\t([0-9]+\t){9}[0-9]+$")

    def __init__(self):
        # TODO: document this executor
        super(ExternalResultsLoader, self).__init__()
        self.data_file = None
        self.errors_file = None
        self.reader = None
        self._last_ts = -1
        self._prev_ts = -1

    def prepare(self):
        exc = TaurusConfigError("Option is required for executor: data-file")
        self.data_file = self.execution.get("data-file", exc)
        self.data_file = self.engine.find_file(self.data_file)
        self.label = self.data_file
        self.errors_file = self.execution.get("errors-jtl", None)
        if self.errors_file:
            self.errors_file = self.engine.find_file(self.errors_file)

        self.reader = self._get_reader()
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
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

    def check(self):
        if self._last_ts > 0 and self._last_ts == self._prev_ts:
            return True
        else:
            self._prev_ts = self._last_ts
