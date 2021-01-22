import re

from bzt import TaurusConfigError, ToolError
from bzt.engine import HavingInstallableTools
from bzt.modules import ScenarioExecutor, FileLister, SelfDiagnosable
from bzt.modules.console import WidgetProvider
from bzt.modules.aggregator import ResultsReader, ConsolidatingAggregator
from bzt.utils import RequiredTool, CALL_PROBLEMS, FileReader, shutdown_process


class K6Executor(ScenarioExecutor, FileLister, WidgetProvider, HavingInstallableTools, SelfDiagnosable):
    def __init__(self):
        super(K6Executor, self).__init__()
        self.output_file = None
        self.log_file = None
        self.script = None
        self.process = None
        self.k6 = None
        self.kpi_file = None

    def prepare(self):
        super(K6Executor, self).prepare()
        self.install_required_tools()

        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("'script' should be present for k6 executor")

        self.stdout = open(self.engine.create_artifact("k6", ".out"), "w")
        self.stderr = open(self.engine.create_artifact("k6", ".err"), "w")

        self.kpi_file = self.engine.create_artifact("kpi", ".jtl")
        self.reader = K6LogReader(self.kpi_file, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def startup(self):
        cmdline = ["k6", "run", "--out", f"csv={self.kpi_file}"]

        load = self.get_load()
        if load.concurrency:
            cmdline += ['--vus', str(load.concurrency)]

        if load.hold:
            cmdline += ['--duration', str(int(load.hold)) + "s"]

        # if load.iterations:
        #     cmdline += ['--iterations', str(load.iterations)]

        cmdline += [self.script]
        self.process = self._execute(cmdline)

    def check(self):
        retcode = self.process.poll()
        if retcode is not None:
            ToolError(f"K6 tool exited with non-zero code: {retcode}")
            return True
        return False

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def post_process(self):
        if self.kpi_file:
            self.engine.existing_artifact(self.kpi_file)
        super(K6Executor, self).post_process()

    def install_required_tools(self):
        self.k6 = self._get_tool(K6, config=self.settings)
        if not self.k6.check_if_installed():
            self.k6.install()


class K6LogReader(ResultsReader):
    def __init__(self, filename, parent_logger):
        super(K6LogReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.file = FileReader(filename=filename, parent_logger=self.log)

    def _read(self, last_pass=False):
        self.lines = list(self.file.get_lines(size=1024 * 1024, last_pass=last_pass))

        self.timestamp_list = []
        self.label_list = []
        self.r_code_list = []
        self.error_msg_list = []
        self.http_req_duration_list = []
        self.http_req_connecting_list = []
        self.http_req_receiving_list = []
        self.vus_list = []
        self.data_received_list = []

        for line in self.lines:
            if line.startswith("http_reqs"):
                self.timestamp_list.append(int(line.split(',')[1]))
                self.label_list.append(line.split(',')[8])
                self.r_code_list.append(line.split(',')[12])
                self.error_msg_list.append(line.split(',')[4])
            elif line.startswith("http_req_duration"):
                self.http_req_duration_list.append(float(line.split(',')[2]))
            elif line.startswith("http_req_connecting"):
                self.http_req_connecting_list.append(float(line.split(',')[2]))
            elif line.startswith("http_req_receiving"):
                self.http_req_receiving_list.append(float(line.split(',')[2]))
            elif line.startswith("vus"):
                self.vus_list.append(int(float(line.split(',')[2])))
            elif line.startswith("data_received"):
                self.data_received_list.append(float(line.split(',')[2]))

            if self.vus_list and len(self.data_received_list) == self.vus_list[0] and \
                    len(self.http_req_receiving_list) == self.vus_list[0]:
                for i in range(self.vus_list[0]):
                    kpi_set = (self.timestamp_list[0], self.label_list[0], self.vus_list[0], self.http_req_duration_list[0],
                               self.http_req_connecting_list[0],
                               self.http_req_duration_list[0] - self.http_req_receiving_list[0],
                               self.r_code_list[0], self.error_msg_list[0], '', self.data_received_list[0])
                    self.timestamp_list.pop(0)
                    self.label_list.pop(0)
                    self.http_req_connecting_list.pop(0)
                    self.http_req_duration_list.pop(0)
                    self.http_req_receiving_list.pop(0)
                    self.r_code_list.pop(0)
                    self.error_msg_list.pop(0)
                    self.data_received_list.pop(0)

                    yield kpi_set

                self.vus_list.pop(0)


class K6(RequiredTool):
    def __init__(self, config=None, **kwargs):
        super(K6, self).__init__(installable=False, **kwargs)

    def check_if_installed(self):
        self.log.debug('Checking K6 Framework: %s' % self.tool_path)
        try:
            out, err = self.call(['k6', 'version'])
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            return False

        if err:
            out += err
        self.log.debug("K6 output: %s", out)
        return True
