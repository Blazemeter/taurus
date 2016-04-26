"""
Module holds all stuff regarding usage of Apache Benchmark

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
import time
import traceback

import psutil

from bzt.engine import FileLister, Scenario, ScenarioExecutor
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, SidebarWidget
from bzt.utils import shell_exec, shutdown_process, RequiredTool, dehumanize_time
from bzt.six import etree, parse, iteritems


class TsungExecutor(ScenarioExecutor, WidgetProvider, FileLister):
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
        self.tsung_artifacts_basedir = None
        self.stats_reader = None
        self.start_time = None
        self.widget = None

    def prepare(self):
        scenario = self.get_scenario()
        self.tool_path = self._check_installed()

        if Scenario.SCRIPT in scenario and scenario[Scenario.SCRIPT]:
            script = self._get_script()
            if not script or not os.path.exists(script):
                raise ValueError("Tsung script '%s' doesn't exist" % script)
            self.tsung_config = self.__modify_user_tsung_config(script)
        elif scenario.get("requests"):
            self.tsung_config = self._generate_tsung_config()
        else:
            raise ValueError("You must specify either a script or a list of requests to run Tsung")

        basedir_prefix = "tsung_taurus_%s" % id(self)
        self.tsung_artifacts_basedir = os.path.join(self.engine.artifacts_dir, basedir_prefix)
        os.makedirs(self.tsung_artifacts_basedir)

        self.stats_reader = TsungStatsReader(self.tsung_artifacts_basedir, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.stats_reader)

        self.__out = open(self.engine.create_artifact("tsung", ".out"), 'w')
        self.__err = open(self.engine.create_artifact("tsung", ".err"), 'w')

    def _get_script(self):
        scenario = self.get_scenario()
        if Scenario.SCRIPT not in scenario:
            return None

        fname = scenario[Scenario.SCRIPT]
        if fname is not None:
            return self.engine.find_file(fname)
        else:
            return None

    def __modify_user_tsung_config(self, user_config_path):
        modified_config_path = self.engine.create_artifact("tsung-config", ".xml")
        load = self.get_load()
        config = TsungConfig()
        config.load(user_config_path)
        config.apply_load_profile(load)
        config.apply_dumpstats()
        config.save(modified_config_path)
        return modified_config_path

    def _generate_tsung_config(self):
        config_file = self.engine.create_artifact("tsung-config", ".xml")
        scenario = self.get_scenario()
        load = self.get_load()
        config = TsungConfig()
        config.generate(scenario, load)
        config.save(config_file)
        return config_file

    def startup(self):
        args = [
            self.tool_path,
            '-f', self.tsung_config,
            '-l', self.tsung_artifacts_basedir,
            '-w', '0',
            'start',
        ]
        self.start_time = time.time()
        self.process = self.execute(args, stdout=self.__out, stderr=self.__err)

    def check(self):
        if self.widget:
            self.widget.update()

        ret_code = self.process.poll()
        if ret_code is None:

            return False
        self.log.info("tsung exit code: %s", ret_code)
        if ret_code != 0:
            raise RuntimeError("tsung exited with non-zero code %s" % ret_code)
        return True

    def shutdown(self):
        shutdown_process(self.process, self.log)

    def post_process(self):
        if self.__out and not self.__out.closed:
            self.__out.close()
        if self.__err and not self.__err.closed:
            self.__err.close()

    def _check_installed(self):
        tool_path = self.settings.get('path', 'tsung')
        tsung = Tsung(tool_path, self.log)
        if not tsung.check_if_installed():
            raise RuntimeError("You must install Tsung manually to use it, see %s" % tsung.INSTALLATION_DOCS)
        return tool_path

    def get_widget(self):
        if not self.widget:
            self.widget = SidebarWidget(self, "Tsung")
        return self.widget

    def resource_files(self):
        resource_files = []
        scenario = self.get_scenario()
        if Scenario.SCRIPT in scenario and scenario[Scenario.SCRIPT]:
            script = self._get_script()
            if not script or not os.path.exists(script):
                raise ValueError("Tsung script '%s' doesn't exist" % script)
            resource_files.append(script)
        return resource_files


class TsungStatsReader(ResultsReader):
    def __init__(self, tsung_basedir, parent_logger):
        super(TsungStatsReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.tsung_basedir = tsung_basedir
        self.filename = None
        self.fds = None
        self.delimiter = ";"
        self.offset = 0
        self.partial_buffer = ""
        self.skipped_header = False

    def _open_fds(self):
        if not self._locate_stats_file():
            return False

        if not os.path.isfile(self.filename):
            self.log.debug("Stats file not appeared yet")
            return False

        if not os.path.getsize(self.filename):
            self.log.debug("Stats file is empty: %s", self.filename)
            return False

        if not self.fds:
            self.fds = open(self.filename)

        return True

    def _locate_stats_file(self):
        basedir_contents = os.listdir(self.tsung_basedir)

        if not basedir_contents:
            self.log.debug("Tsung artifacts not appeared yet")
            return False
        if len(basedir_contents) != 1:
            self.log.warning("Multiple files in Tsung basedir %s, this shouldn't happen", self.tsung_basedir)
            return False

        self.filename = os.path.join(self.tsung_basedir, basedir_contents[0], "tsung.dump")
        return True

    def __del__(self):
        if self.fds:
            self.fds.close()

    def _read(self, last_pass=False):
        while not self.fds and not self._open_fds():
            self.log.debug("No data to start reading yet")
            yield None

        self.log.debug("Reading Tsung results")
        self.fds.seek(self.offset)
        if last_pass:
            lines = self.fds.readlines()
        else:
            lines = self.fds.readlines(1024 * 1024)
        self.offset = self.fds.tell()

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
            etime = float(fields[8]) / 1000
            trname = fields[9]
            error = fields[10] or None

            concur = int(fields[2])
            con_time = 0
            latency = 0

            yield tstamp, url, concur, etime, con_time, latency, rstatus, error, trname


class TsungConfig(object):
    def __init__(self):
        self.log = logging.getLogger(self.__class__.__name__)
        self.root = etree.Element("tsung", loglevel="notice", version="1.0", dumptraffic="protocol")
        self.tree = etree.ElementTree(self.root)

    def load(self, filename):
        try:
            self.tree = etree.ElementTree()
            self.tree.parse(filename)
            self.root = self.tree.getroot()
        except BaseException as exc:
            self.log.debug("XML parsing error: %s", traceback.format_exc())
            raise RuntimeError("XML parsing failed for file %s: %s" % (filename, exc))

    def save(self, filename):
        self.log.debug("Saving Tsung config to: %s", filename)
        with open(filename, "wb") as fhd:
            # TODO: dynamically generate this /usr/ path?
            doctype = '<!DOCTYPE tsung SYSTEM "/usr/share/tsung/tsung-1.0.dtd">'
            xml = etree.tostring(self.tree, pretty_print=True, encoding="UTF-8", xml_declaration=True, doctype=doctype)
            fhd.write(xml)

    def generate(self, scenario, load):
        self.root.append(self.__gen_clients())
        self.root.append(self.__gen_servers(scenario))
        self.root.append(self.__gen_load(load))
        self.root.append(self.__gen_sessions(scenario))

    def apply_dumpstats(self):
        self.root.set("dumptraffic", "protocol")

    def apply_load_profile(self, load):
        # do not apply unspecified load profile
        if not load.throughput and not load.hold:
            return
        original_load = self.find("//tsung/load")
        generated_load = self.__gen_load(load)
        if not original_load:
            self.log.warning("<load> section not found in Tsung config, will create one")
            servers = self.find("//tsung/servers")
            if not servers:
                raise ValueError("<servers> section not found. Provided Tsung script is invalid")
            servers.addnext(generated_load)
        else:
            self.root.replace(original_load[0], generated_load)

    def __time_to_tsung_time(self, time):
        if time % 3600 == 0:
            return time // 3600, "hour"
        elif time % 60 == 0:
            return time // 60, "minute"
        else:
            return time, "second"

    def __gen_clients(self):
        # TODO: distributed clients?
        clients = etree.Element("clients")
        client = etree.Element("client", host="localhost", use_controller_vm="true")
        clients.append(client)
        return clients

    def __gen_servers(self, scenario):
        default_address = scenario.get("default-address", None)
        # TODO: don't crash if there's no default-address but all requests are pointed to the same domain
        if default_address is None:
            raise ValueError("default-address is not specified")
        base_addr = parse.urlparse(default_address)
        servers = etree.Element("servers")
        port = base_addr.port if base_addr.port is not None else 80
        server = etree.Element("server", host=base_addr.hostname, port=str(port), type="tcp")
        servers.append(server)
        return servers

    def __gen_load(self, load):
        """
        Generate Tsung load profile.

        Basically, generates two phases: one for ramp-up, one for hold-for.

        Tsung load progression is similar to pbench one. The users are erlang processes which are spawned according to
        load profile. Each user executes assigned session (requests + think-time + logic) and then dies. So if we want
        to maintain constant rps - we have to spawn N new users each second.
        :param scenario:
        :param load:
        :return:
        """
        throughput = load.throughput if load.throughput is not None else 1
        load_elem = etree.Element("load")
        if load.duration:
            duration, unit = self.__time_to_tsung_time(int(load.duration))
            load_elem.set('duration', str(duration))
            load_elem.set('unit', unit)
        phases = []

        if load.hold:
            duration, unit = self.__time_to_tsung_time(int(load.hold))
            users = etree.Element("users", arrivalrate=str(throughput), unit="second")
            phase = etree.Element("arrivalphase",
                                  phase=str("1"),
                                  duration=str(duration),
                                  unit=unit)
            phase.append(users)
            phases.append(phase)
        else:
            raise ValueError("You must specify test duration with `hold-for`")

        for phase in phases:
            load_elem.append(phase)

        return load_elem

    def __gen_sessions(self, scenario):
        sessions = etree.Element("sessions")
        session = etree.Element("session", name="taurus_requests", probability="100", type="ts_http")
        for request in scenario.get_requests():
            request_elem = etree.Element("request")
            http_elem = etree.Element("http", url=request.url, method=request.method, version="1.1")
            if request.body:
                http_elem.set('contents', request.body)

            for header in request.headers:
                for header_name, header_value in iteritems(header):
                    http_elem.append(etree.Element("http_header", name=header_name, value=header_value))

            request_elem.append(http_elem)
            session.append(request_elem)
            if request.think_time is not None:
                think_time = int(dehumanize_time(request.think_time))
                session.append(etree.Element("thinktime", value=str(think_time)))
        sessions.append(session)
        return sessions

    def find(self, xpath_selector):
        return self.tree.xpath(xpath_selector)


class Tsung(RequiredTool):
    INSTALLATION_DOCS = "http://gettaurus.org/docs/Tsung/#Tsung-Installation"

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
