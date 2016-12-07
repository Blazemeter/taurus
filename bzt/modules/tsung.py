"""
Module holds all stuff regarding usage of Tsung

Copyright 2016 BlazeMeter Inc.

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
import logging
import os
import re
import time
import traceback

from bzt import TaurusConfigError, ToolError, TaurusInternalException
from bzt.engine import FileLister, Scenario, ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.modules.services import HavingInstallableTools
from bzt.six import etree, parse, iteritems
from bzt.utils import shell_exec, shutdown_process, RequiredTool, dehumanize_time, which


class TsungExecutor(ScenarioExecutor, WidgetProvider, FileLister, HavingInstallableTools):
    """
    Tsung executor module
    """

    def __init__(self):
        super(TsungExecutor, self).__init__()
        self.log = logging.getLogger('')
        self.process = None
        self.__out = None
        self.__err = None
        self.__stats_file = None
        self.tsung_config = None
        self.tool_path = None
        self.tsung_controller_id = None
        self.tsung_artifacts_basedir = None

    def prepare(self):
        scenario = self.get_scenario()
        self.tool_path = self.install_required_tools()

        if Scenario.SCRIPT in scenario and scenario[Scenario.SCRIPT]:
            script = self.get_script_path()
            if not script or not os.path.exists(script):
                raise TaurusConfigError("Tsung: script '%s' doesn't exist" % script)
            self.tsung_config = self.__modify_user_tsung_config(script)
        elif scenario.get("requests"):
            self.tsung_config = self._generate_tsung_config()
        else:
            raise TaurusConfigError("Tsung: you must specify either a script or a list of requests")

        self.tsung_controller_id = "tsung_taurus_%s" % id(self)
        self.tsung_artifacts_basedir = os.path.join(self.engine.artifacts_dir, self.tsung_controller_id)
        if not os.path.exists(self.tsung_artifacts_basedir):
            os.makedirs(self.tsung_artifacts_basedir)
        else:
            msg = "Tsung artifacts basedir already exists, will not create: %s"
            self.log.warning(msg, self.tsung_artifacts_basedir)

        self.reader = TsungStatsReader(self.tsung_artifacts_basedir, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

        self.__out = open(self.engine.create_artifact("tsung", ".out"), 'w')
        self.__err = open(self.engine.create_artifact("tsung", ".err"), 'w')

    def __modify_user_tsung_config(self, user_config_path):
        modified_config_path = self.engine.create_artifact("tsung-config", ".xml")
        load = self.get_load()
        tsung = Tsung(self.tool_path, self.log)
        config = TsungConfig(tsung)
        config.load(user_config_path)
        config.apply_load_profile(load)
        config.apply_dumpstats()
        config.save(modified_config_path)
        return modified_config_path

    def _generate_tsung_config(self):
        config_file = self.engine.create_artifact("tsung-config", ".xml")
        scenario = self.get_scenario()
        load = self.get_load()
        tsung = Tsung(self.tool_path, self.log)
        config = TsungConfig(tsung)
        config.generate(scenario, load)
        config.save(config_file)
        return config_file

    def startup(self):
        args = [
            self.tool_path,
            '-f', self.tsung_config,
            '-l', self.tsung_artifacts_basedir,
            '-i', self.tsung_controller_id,
            '-w', '0',
            'start',
        ]
        self.start_time = time.time()
        self.process = self.execute(args, stdout=self.__out, stderr=self.__err)

    def check(self):
        ret_code = self.process.poll()
        if ret_code is None:
            return False
        if ret_code != 0:
            raise ToolError("Tsung exited with non-zero code: %s" % ret_code)
        return True

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def post_process(self):
        if self.__out and not self.__out.closed:
            self.__out.close()
        if self.__err and not self.__err.closed:
            self.__err.close()

    def install_required_tools(self):
        tool_path = self.settings.get('path', 'tsung')
        tsung = Tsung(tool_path, self.log)
        if not tsung.check_if_installed():
            tsung.install()
        return tool_path

    def get_widget(self):
        if not self.widget:
            self.widget = ExecutorWidget(self, self.tsung_controller_id)
        return self.widget

    def resource_files(self):
        scenario = self.get_scenario()
        script = scenario.get(Scenario.SCRIPT, None)
        if script:
            return [script]
        else:
            return []


class TsungStatsReader(ResultsReader):
    def __init__(self, tsung_basedir, parent_logger):
        super(TsungStatsReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.tsung_basedir = tsung_basedir
        self.stats_filename = None
        self.stats_fds = None
        self.stats_offset = 0
        self.log_filename = None
        self.log_fds = None
        self.log_offset = 0
        self.delimiter = ";"
        self.partial_buffer = ""
        self.skipped_header = False
        self.concurrency = 0

    def _open_fds(self):
        if not self._locate_stats_file():
            return False

        if not os.path.isfile(self.stats_filename):
            self.log.debug("Stats file not appeared yet")
            return False

        if not os.path.getsize(self.stats_filename):
            self.log.debug("Stats file is empty: %s", self.stats_filename)
            return False

        if not self.stats_fds:
            self.stats_fds = open(self.stats_filename)
        if not self.log_fds:
            self.log_fds = open(self.log_filename)

        return True

    def _locate_stats_file(self):
        basedir_contents = os.listdir(self.tsung_basedir)

        if not basedir_contents:
            self.log.debug("Tsung artifacts not appeared yet")
            return False
        if len(basedir_contents) != 1:
            self.log.warning("Multiple files in Tsung basedir %s, this shouldn't happen", self.tsung_basedir)
            return False

        self.stats_filename = os.path.join(self.tsung_basedir, basedir_contents[0], "tsung.dump")
        self.log_filename = os.path.join(self.tsung_basedir, basedir_contents[0], "tsung.log")
        return True

    def __del__(self):
        if self.stats_fds:
            self.stats_fds.close()
        if self.log_fds:
            self.log_fds.close()

    def _read_concurrency(self, last_pass):
        self.log_fds.seek(self.log_offset)
        if last_pass:
            lines = self.log_fds.readlines()
        else:
            lines = self.log_fds.readlines(1024 * 1024)
        self.log_offset = self.log_fds.tell()
        extractor = re.compile(r'^stats: users (\d+) (\d+)$')
        for line in lines:
            match = extractor.match(line.strip())
            if not match:
                continue
            self.concurrency = int(match.group(2))
            self.log.debug("Actual Tsung concurrency: %s", self.concurrency)

    def _read(self, last_pass=False):
        while not self.stats_fds and not self._open_fds():
            self.log.debug("No data to start reading yet")
            yield None

        self.log.debug("Reading Tsung results")
        self.stats_fds.seek(self.stats_offset)
        if last_pass:
            lines = self.stats_fds.readlines()
        else:
            lines = self.stats_fds.readlines(1024 * 1024)
        self.stats_offset = self.stats_fds.tell()

        self._read_concurrency(last_pass)

        for line in lines:
            if not line.endswith("\n"):
                self.partial_buffer += line
                continue

            if not self.skipped_header and line.startswith("#"):
                self.skipped_header = True
                continue

            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""

            line = line.strip()
            fields = line.split(self.delimiter)

            tstamp = int(float(fields[0]))
            url = fields[4] + fields[5]
            rstatus = fields[6]
            rsize = int(fields[7])
            etime = float(fields[8]) / 1000
            trname = fields[9]
            error = fields[10] or None

            con_time = 0
            latency = 0

            yield tstamp, url, self.concurrency, etime, con_time, latency, rstatus, error, trname, rsize


class TsungConfig(object):
    def __init__(self, tsung_tool):
        self.log = logging.getLogger(self.__class__.__name__)
        self.root = etree.Element("tsung", loglevel="notice", version="1.0", dumptraffic="protocol", backend="text")
        self.tree = etree.ElementTree(self.root)
        self.tsung_tool = tsung_tool

    def load(self, filename):
        try:
            self.tree = etree.ElementTree()
            self.tree.parse(filename)
            self.root = self.tree.getroot()
        except BaseException as exc:
            self.log.debug("Tsung: XML parsing error: %s", traceback.format_exc())
            raise TaurusInternalException("Tsung: XML parsing failed for file %s: %s" % (filename, exc))

    def save(self, filename):
        self.log.debug("Saving Tsung config to: %s", filename)
        with open(filename, "wb") as fhd:
            tsung_dtd = self.tsung_tool.get_dtd_path()
            self.log.debug("Detected Tsung DTD: %s", tsung_dtd)
            doctype = '<!DOCTYPE tsung SYSTEM "%s">' % tsung_dtd
            xml = etree.tostring(self.tree, pretty_print=True, encoding="UTF-8", xml_declaration=True, doctype=doctype)
            fhd.write(xml)

    def generate(self, scenario, load):
        self.root.append(self.__gen_clients())
        self.root.append(self.__gen_servers(scenario))
        self.root.append(self.__gen_load(load))
        self.root.append(self.__gen_options(scenario))
        self.root.append(self.__gen_sessions(scenario))

    def apply_dumpstats(self):
        self.root.set("dumptraffic", "protocol")
        self.root.set("backend", "text")

    def apply_load_profile(self, load):
        # do not apply unspecified load profile
        if not load.concurrency and not load.hold:
            return
        original_load = self.find("//tsung/load")
        generated_load = self.__gen_load(load)
        if not original_load:
            self.log.warning("<load> section not found in Tsung config, will create one")
            servers = self.find("//tsung/servers")
            if not servers:
                raise TaurusConfigError("Provided Tsung script is invalid: <servers> section not found")
            servers.addnext(generated_load)
        else:
            self.root.replace(original_load[0], generated_load)

    @staticmethod
    def __time_to_tsung_time(time_amount):
        if time_amount % 3600 == 0:
            return time_amount // 3600, "hour"
        elif time_amount % 60 == 0:
            return time_amount // 60, "minute"
        else:
            return time_amount, "second"

    def __gen_clients(self):
        # TODO: distributed clients?
        clients = etree.Element("clients")
        client = etree.Element("client", host="localhost", use_controller_vm="true")
        clients.append(client)
        return clients

    def __gen_servers(self, scenario):
        default_address = scenario.get("default-address", None)
        if default_address is None:
            requests = list(scenario.get_requests())
            if not requests:
                raise TaurusConfigError("Tsung: you must specify requests in scenario")
            base_addr = parse.urlparse(requests[0].url)
            self.log.debug("default-address was not specified, using %s instead", base_addr.hostname)
        else:
            base_addr = parse.urlparse(default_address)
        servers = etree.Element("servers")
        port = base_addr.port if base_addr.port is not None else 80
        server = etree.Element("server", host=base_addr.hostname, port=str(port), type="tcp")
        servers.append(server)
        return servers

    def __gen_load(self, load):
        """
        Generate Tsung load profile.

        Tsung load progression is scenario-based. Virtual users are erlang processes which are spawned according to
        load profile. Each user executes assigned session (requests + think-time + logic) and then dies.
        :param scenario:
        :param load:
        :return:
        """
        concurrency = load.concurrency if load.concurrency is not None else 1
        load_elem = etree.Element("load")
        if load.duration:
            duration, unit = self.__time_to_tsung_time(int(round(load.duration)))
            load_elem.set('duration', str(duration))
            load_elem.set('unit', unit)
        phases = []

        if load.hold:
            duration, unit = self.__time_to_tsung_time(int(round(load.hold)))
            users = etree.Element("users", arrivalrate=str(concurrency), unit="second")
            phase = etree.Element("arrivalphase",
                                  phase=str("1"),
                                  duration=str(duration),
                                  unit=unit)
            phase.append(users)
            phases.append(phase)
        else:
            raise TaurusConfigError("Tsung: you must specify test duration with 'hold-for'")

        for phase in phases:
            load_elem.append(phase)

        return load_elem

    def __gen_options(self, scenario):
        options = etree.Element("options")

        global_think_time = scenario.data.get('think-time', None)
        if global_think_time:
            think_time = int(dehumanize_time(global_think_time))
            options.append(etree.Element("option", name="thinktime", value=str(think_time), random="false"))

        global_tcp_timeout = scenario.data.get('timeout')
        if global_tcp_timeout:
            timeout = int(dehumanize_time(global_tcp_timeout)) * 1000
            options.append(etree.Element("option", name="connect_timeout", value=str(timeout)))

        global_max_retries = scenario.data.get('max-retries', 1)
        options.append(etree.Element("option", name="max_retries", value=str(global_max_retries)))
        return options

    def __gen_sessions(self, scenario):
        sessions = etree.Element("sessions")
        session = etree.Element("session", name="taurus_requests", probability="100", type="ts_http")
        for request in scenario.get_requests():
            request_elem = etree.Element("request")
            http_elem = etree.Element("http", url=request.url, method=request.method, version="1.1")
            if request.body:
                http_elem.set('contents', request.body)

            headers = {}
            headers.update(scenario.data.get('headers', {}))
            headers.update(request.headers)
            for header_name, header_value in iteritems(headers):
                http_elem.append(etree.Element("http_header", name=header_name, value=header_value))

            request_elem.append(http_elem)
            session.append(request_elem)
            if request.think_time is not None:
                think_time = int(dehumanize_time(request.think_time))
                session.append(etree.Element("thinktime", value=str(think_time), random="false"))
        sessions.append(session)
        return sessions

    def find(self, xpath_selector):
        return self.tree.xpath(xpath_selector)


class Tsung(RequiredTool):
    INSTALLATION_DOCS = "http://gettaurus.org/docs/Tsung/#Tsung-Installation"
    DEFAULT_DTD_PATH = "/usr/share/tsung/tsung-1.0.dtd"

    def __init__(self, tool_path, parent_logger):
        super(Tsung, self).__init__("Tsung", tool_path)
        self.tool_path = tool_path
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug('Checking Tsung at %s' % self.tool_path)
        try:
            shell_exec([self.tool_path, '-v'])
        except OSError:
            return False
        return True

    def install(self):
        raise ToolError("You must install Tsung manually to use it, see %s" % self.INSTALLATION_DOCS)

    def get_tool_abspath(self):
        if not self.tool_path:
            return None

        abspath = os.path.abspath(os.path.expanduser(self.tool_path))
        if os.path.exists(abspath):
            return abspath

        executables = which(self.tool_path)
        if executables:
            return executables[0]

    @staticmethod
    def get_tool_prefix(tool_abspath):
        """
        Get tsung installation prefix (like /usr/ or /usr/local)
        :return: str
        """
        if tool_abspath is None:
            return None

        parts = tool_abspath.split(os.sep)

        if len(parts) < 2:
            return None

        # cut 'bin/tsung' from abspath
        prefix = os.sep.join(parts[:-2])
        return prefix

    def get_dtd_path(self):
        "Get path of DTD validation file for Tsung."
        tsung_abspath = self.get_tool_abspath()
        prefix = self.get_tool_prefix(tsung_abspath)
        if not prefix:
            return self.DEFAULT_DTD_PATH
        else:
            return os.path.join(prefix, "share", "tsung", "tsung-1.0.dtd")
