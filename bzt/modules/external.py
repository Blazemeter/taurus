import re

from bzt import TaurusInternalException, TaurusConfigError
from bzt.engine import ScenarioExecutor
from bzt.modules.ab import TSVDataReader
from bzt.modules.aggregator import AggregatorListener
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader, DataPoint
from bzt.modules.gatling import DataLogReader as GatlingLogReader
from bzt.modules.grinder import DataLogReader as GrinderLogReader
from bzt.modules.jmeter import JTLReader, JTLErrorsReader
from bzt.modules.pbench import PBenchKPIReader


class ExternalReportAnalyzer(ScenarioExecutor, AggregatorListener):
    AB_HEADER = "starttime\tseconds\tctime\tdtime\tttime\twait"
    PBENCH_FORMAT = re.compile("^[0-9]+\.[0-9]{3}\t[^\t]*\t([0-9]+\t){9}[0-9]+$")
    # PBENCH_FORMAT = re.compile(r'^([\d\.]+\s+\S+\s+\s+(\d+\s+){9}\d+)$', re.MULTILINE)

    def __init__(self):
        # TODO: document this executor
        super(ExternalReportAnalyzer, self).__init__()
        self.data_file = None
        self.errors_file = None
        self.reader = None
        self._last_ts = -1
        self._prev_ts = -1

    def prepare(self):
        self.data_file = self.execution.get("data-file", TaurusConfigError("Option is required for executor: data-file"))
        self.label = self.data_file
        self.errors_file = self.execution.get("errors-jtl", None)

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


class XMLJTLReader(JTLErrorsReader, ResultsReader):
    def __init__(self, filename, parent_logger):
        super(XMLJTLReader, self).__init__(filename, parent_logger)
        self.items = []

    def _read(self, final_pass=False):
        self.read_file()
        while self.items:
            yield self.items.pop(0)

    def _parse_element(self, elem):
        tstmp = int(int(elem.get("ts")) / 1000)
        label = elem.get("lb")
        rtm = int(elem.get("t")) / 1000.0
        ltc = int(elem.get("lt")) / 1000.0 if "lt" in elem.attrib else 0
        cnn = int(elem.get("ct")) / 1000.0 if "ct" in elem.attrib else 0
        byte_count = int(elem.get("by")) if "by" in elem.attrib else 0
        concur = int(elem.get("na")) if "na" in elem.attrib else 0
        trname = ''

        rcd = elem.get("rc")
        message = self.get_failure_message(elem)
        if message is None:
            message = elem.get('rm')

        error = message if elem.get("s") == "false" else None
        self.items.append((tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname, byte_count))
