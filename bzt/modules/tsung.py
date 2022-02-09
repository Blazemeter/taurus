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
import copy
import logging
import os
import re
import traceback
from urllib import parse
from shutil import which

from bzt import TaurusConfigError, ToolError, TaurusInternalException
from bzt.engine import ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import ExecutorWidget
from bzt.requests_model import HTTPRequest
from bzt.utils import CALL_PROBLEMS, shutdown_process, RequiredTool, dehumanize_time, FileReader, etree, iteritems


class TsungExecutor(ScenarioExecutor):
    """
    Tsung executor module
    """

    def __init__(self):
        super(TsungExecutor, self).__init__()
        self.process = None
        self.__stats_file = None
        self.tsung_config = None
        self.tool = None
        self.tsung_controller_id = None
        self.tsung_artifacts_basedir = None

    def prepare(self):
        super(TsungExecutor, self).prepare()
        scenario = self.get_scenario()
        self.install_required_tools()

        script = self.get_script_path()
        if script:
            if not os.path.exists(script):
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

        self.stdout = open(self.engine.create_artifact("tsung", ".out"), 'w')
        self.stderr = open(self.engine.create_artifact("tsung", ".err"), 'w')

    def __modify_user_tsung_config(self, user_config_path):
        modified_config_path = self.engine.create_artifact("tsung-config", ".xml")
        load = self.get_load()
        config = TsungConfig(self.tool)
        config.load(user_config_path)
        config.apply_load_profile(load)
        config.apply_dumpstats()
        config.save(modified_config_path)
        return modified_config_path

    def _generate_tsung_config(self):
        config_file = self.engine.create_artifact("tsung-config", ".xml")
        scenario = self.get_scenario()
        load = self.get_load()
        config = TsungConfig(self.tool)
        config.generate(scenario, load)
        config.save(config_file)
        return config_file

    def startup(self):
        args = [
            self.tool.tool_path,
            '-f', self.tsung_config,
            '-l', self.tsung_artifacts_basedir,
            '-i', self.tsung_controller_id,
            '-w', '0',
        ]

        user_cmd = self.settings.get("cmdline")
        if user_cmd:
            args += user_cmd.split(" ")

        args.append('start')

        self.process = self._execute(args)

    def check(self):
        ret_code = self.process.poll()
        if ret_code is None:
            return False
        if ret_code != 0:
            raise ToolError("Tsung exited with non-zero code: %s" % ret_code, self.get_error_diagnostics())
        return True

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def install_required_tools(self):
        self.tool = self._get_tool(Tsung, config=self.settings)
        if not self.tool.check_if_installed():
            self.tool.install()

    def get_widget(self):
        if not self.widget:
            self.widget = ExecutorWidget(self, "Tsung: " + self.tsung_controller_id)
        return self.widget

    def resource_files(self):
        script = self.get_script_path()
        if script:
            return [script]
        else:
            return []

    def get_error_diagnostics(self):
        diagnostics = []
        if self.stdout is not None:
            with open(self.stdout.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("Tsung STDOUT:\n" + contents)
        if self.stderr is not None:
            with open(self.stderr.name) as fds:
                contents = fds.read().strip()
                if contents.strip():
                    diagnostics.append("Tsung STDERR:\n" + contents)
        return diagnostics


class TsungStatsReader(ResultsReader):
    def __init__(self, tsung_basedir, parent_logger):
        super(TsungStatsReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.tsung_basedir = tsung_basedir
        self.stats_file = FileReader(parent_logger=self.log, file_opener=self.open_stats)
        self.log_file = FileReader(parent_logger=self.log, file_opener=self.open_log)
        self.delimiter = ";"
        self.partial_buffer = ""
        self.skipped_header = False
        self.concurrency = 0

    def open_stats(self, filename):
        return self.open_file(ext='dump')

    def open_log(self, filename):
        return self.open_file(ext='log')

    def open_file(self, ext):
        basedir_contents = os.listdir(self.tsung_basedir)

        if not basedir_contents:
            self.log.debug("Tsung artifacts not appeared yet")
            return

        if len(basedir_contents) != 1:
            self.log.warning("Multiple files in Tsung basedir %s, this shouldn't happen", self.tsung_basedir)
            return

        filename = os.path.join(self.tsung_basedir, basedir_contents[0], "tsung." + ext)

        if not os.path.isfile(filename):
            self.log.debug("File not appeared yet: %s", filename)
            return
        if not os.path.getsize(filename):
            self.log.debug("File is empty: %s", filename)
            return

        self.log.debug('Opening file: %s', filename)
        return open(filename, mode='rb')

    def _read_concurrency(self, last_pass):
        lines = self.log_file.get_lines(size=1024 * 1024, last_pass=last_pass)
        extractor = re.compile(r'^stats: users (\d+) (\d+)$')

        for line in lines:
            match = extractor.match(line.strip())
            if not match:
                continue
            self.concurrency = int(match.group(2))
            self.log.debug("Actual Tsung concurrency: %s", self.concurrency)

    def _read(self, last_pass=False):
        self.log.debug("Reading Tsung results")

        self._read_concurrency(last_pass)
        lines = self.stats_file.get_lines(size=1024 * 1024, last_pass=last_pass)

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
            etime = float(fields[8]) / 1000.0
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
        self.tool = tsung_tool

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
            tsung_dtd = self.tool.get_dtd_path()
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
        clients = etree.Element("clients")
        client = etree.Element("client", host="localhost", use_controller_vm="true")
        clients.append(client)
        return clients

    def __first_http_request(self, scenario):
        for request in scenario.get_requests():
            if isinstance(request, HTTPRequest):
                return request
        return None

    def __gen_servers(self, scenario):
        default_address = scenario.get("default-address")
        if default_address:
            base_addr = parse.urlparse(default_address)
        else:
            first_request = self.__first_http_request(scenario)
            if not first_request:
                raise TaurusConfigError("Tsung: you must specify requests in scenario")
            base_addr = parse.urlparse(first_request.url)
            self.log.debug("default-address was not specified, using %s instead", base_addr.hostname)

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

        global_think_time = scenario.get_think_time()
        if global_think_time:
            think_time = int(dehumanize_time(global_think_time))
            options.append(etree.Element("option", name="thinktime", value=str(think_time), random="false"))

        global_tcp_timeout = scenario.get("timeout")
        if global_tcp_timeout:
            timeout = int(dehumanize_time(global_tcp_timeout) * 1000)
            options.append(etree.Element("option", name="connect_timeout", value=str(timeout)))

        global_max_retries = scenario.get("max-retries", 1)
        options.append(etree.Element("option", name="max_retries", value=str(global_max_retries)))
        return options

    def __gen_sessions(self, scenario):
        sessions = etree.Element("sessions")
        session = etree.Element("session", name="taurus_requests", probability="100", type="ts_http")
        for request in scenario.get_requests():
            if not isinstance(request, HTTPRequest):
                msg = "Tsung config generator doesn't support '%s' blocks, skipping"
                self.log.warning(msg, request.NAME)
                continue

            request_elem = etree.Element("request")
            http_elem = etree.Element("http", url=request.url, method=request.method, version="1.1")
            if request.body:
                http_elem.set('contents', request.body)

            headers = copy.deepcopy(scenario.get_headers())
            headers.update(copy.deepcopy(request.headers))
            for header_name, header_value in iteritems(headers):
                http_elem.append(etree.Element("http_header", name=header_name, value=header_value))

            request_elem.append(http_elem)
            session.append(request_elem)
            if request.get_think_time():
                think_time = int(dehumanize_time(request.get_think_time()))
                session.append(etree.Element("thinktime", value=str(think_time), random="false"))
        sessions.append(session)
        return sessions

    def find(self, xpath_selector):
        return self.tree.xpath(xpath_selector)


class Tsung(RequiredTool):
    INSTALLATION_DOCS = "http://gettaurus.org/docs/Tsung/#Tsung-Installation"
    DEFAULT_DTD_PATH = "/usr/share/tsung/tsung-1.0.dtd"

    def __init__(self, config=None, **kwargs):
        settings = config or {}
        tool_path = settings.get("path", "tsung")
        super(Tsung, self).__init__(tool_path=tool_path, installable=False, **kwargs)

    def check_if_installed(self):
        self.log.debug("Trying %s...", self.tool_name)
        try:
            out, err = self.call([self.tool_path, "-v"])
        except CALL_PROBLEMS as exc:
            self.log.warning("%s check failed: %s", self.tool_name, exc)
            self.log.warning("Info for tsung installation: %s", self.INSTALLATION_DOCS)
            return False

        if err:
            out += err
        self.log.debug("%s output: %s", self.tool_name, out)
        return True

    def get_tool_abspath(self):
        if not self.tool_path:
            return None

        abspath = os.path.abspath(self.tool_path)
        if os.path.exists(abspath):
            return abspath

        return which(self.tool_path)


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
