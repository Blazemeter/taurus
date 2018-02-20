"""
Copyright 2017 BlazeMeter Inc.

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
import os
import subprocess
import traceback
from subprocess import CalledProcessError

from bzt import ToolError, TaurusConfigError
from bzt.engine import HavingInstallableTools
from bzt.modules import SubprocessedExecutor
from bzt.six import string_types, iteritems
from bzt.utils import get_full_path, TclLibrary, RequiredTool, is_windows, Node, dehumanize_time, to_json, Environment

MOCHA_NPM_PACKAGE_NAME = "mocha@4.0.1"
SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME = "selenium-webdriver@3.6.0"
WDIO_NPM_PACKAGE_NAME = "webdriverio@4.8.0"
WDIO_MOCHA_PLUGIN_NPM_PACKAGE_NAME = "wdio-mocha-framework"
NEWMAN_NPM_PACKAGE_NAME = "newman"


class MochaTester(SubprocessedExecutor, HavingInstallableTools):
    """
    Mocha tests runner

    :type node_tool: Node
    :type mocha_tool: Mocha
    """

    def __init__(self):
        super(MochaTester, self).__init__()
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=2),
                                        "resources",
                                        "mocha-taurus-plugin.js")
        self.tools_dir = "~/.bzt/selenium-taurus/mocha"
        self.node_tool = None
        self.npm_tool = None
        self.mocha_tool = None

    def prepare(self):
        super(MochaTester, self).prepare()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("Script not passed to runner %s" % self)

        self.tools_dir = get_full_path(self.settings.get("tools-dir", self.tools_dir))
        self.install_required_tools()
        self.reporting_setup(suffix='.ldjson')

    def install_required_tools(self):
        tools = []
        tools.append(TclLibrary(self.log))
        self.node_tool = Node(self.log)
        self.npm_tool = NPM(self.log)
        self.mocha_tool = Mocha(self.tools_dir, self.node_tool, self.npm_tool, self.log)
        tools.append(self.node_tool)
        tools.append(self.npm_tool)
        tools.append(self.mocha_tool)
        tools.append(JSSeleniumWebdriverPackage(self.tools_dir, self.node_tool, self.npm_tool, self.log))
        tools.append(TaurusMochaPlugin(self.plugin_path, ""))

        self._check_tools(tools)

    def startup(self):
        mocha_cmdline = [
            self.node_tool.executable,
            self.plugin_path,
            "--report-file",
            self.report_file,
            "--test-suite",
            self.script
        ]
        load = self.get_load()
        if load.iterations:
            mocha_cmdline += ['--iterations', str(load.iterations)]

        if load.hold:
            mocha_cmdline += ['--hold-for', str(load.hold)]

        self.env.set({"NODE_PATH": self.mocha_tool.env.get("NODE_PATH")})

        self._start_subprocess(mocha_cmdline)


class WebdriverIOExecutor(SubprocessedExecutor, HavingInstallableTools):
    """
    WebdriverIO-based test runner

    :type node_tool: Node
    :type wdio_tool: WDIO
    """

    def __init__(self):
        super(WebdriverIOExecutor, self).__init__()
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=2),
                                        "resources",
                                        "wdio-taurus-plugin.js")
        self.tools_dir = "~/.bzt/selenium-taurus/wdio"
        self.node_tool = None
        self.npm_tool = None
        self.wdio_tool = None

    def prepare(self):
        super(WebdriverIOExecutor, self).prepare()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("Script not passed to executor %s" % self)

        self.tools_dir = get_full_path(self.settings.get("tools-dir", self.tools_dir))
        self.install_required_tools()
        self.reporting_setup(suffix='.ldjson')

    def install_required_tools(self):
        tools = []
        tools.append(TclLibrary(self.log))
        self.node_tool = Node(self.log)
        self.npm_tool = NPM(self.log)
        self.wdio_tool = WDIO(self.tools_dir, self.node_tool, self.npm_tool, self.log)
        tools.append(self.node_tool)
        tools.append(self.npm_tool)
        tools.append(self.wdio_tool)
        tools.append(TaurusWDIOPlugin(self.plugin_path, ""))
        tools.append(WDIOMochaPlugin(self.tools_dir, self.node_tool, self.npm_tool, self.log))

        self._check_tools(tools)

    def startup(self):
        script_dir = get_full_path(self.script, step_up=1)
        script_file = os.path.basename(self.script)
        cmdline = [
            self.node_tool.executable,
            self.plugin_path,
            "--report-file",
            self.report_file,
            "--wdio-config",
            script_file,
        ]

        load = self.get_load()
        if load.iterations:
            cmdline += ['--iterations', str(load.iterations)]

        if load.hold:
            cmdline += ['--hold-for', str(load.hold)]

        self.env.set({"NODE_PATH": self.wdio_tool.env.get("NODE_PATH")})
        self.env.add_path({"NODE_PATH": "node_modules"}, finish=True)

        self._start_subprocess(cmdline, cwd=script_dir)


class NewmanExecutor(SubprocessedExecutor, HavingInstallableTools):
    """
    Newman-based test runner

    :type node_tool: Node
    :type newman_tool: Newman
    """

    def __init__(self):
        super(NewmanExecutor, self).__init__()
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=2),
                                        "resources",
                                        "newman-reporter-taurus.js")
        self.tools_dir = "~/.bzt/newman"
        self.node_tool = None
        self.npm_tool = None
        self.newman_tool = None

    def prepare(self):
        super(NewmanExecutor, self).prepare()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("Script not passed to executor %s" % self)

        self.tools_dir = get_full_path(self.settings.get("tools-dir", self.tools_dir))
        self.install_required_tools()
        self.reporting_setup(suffix='.ldjson')

    def install_required_tools(self):
        tools = []
        tools.append(TclLibrary(self.log))
        self.node_tool = Node(self.log)
        self.npm_tool = NPM(self.log)
        self.newman_tool = Newman(self.tools_dir, self.node_tool, self.npm_tool, self.log)
        tools.append(self.node_tool)
        tools.append(self.npm_tool)
        tools.append(self.newman_tool)
        tools.append(TaurusNewmanPlugin(self.plugin_path, ""))

        self._check_tools(tools)

    def startup(self):
        script_dir = get_full_path(self.script, step_up=1)
        script_file = os.path.basename(self.script)
        cmdline = [
            self.node_tool.executable,
            self.newman_tool.entrypoint,
            "run",
            script_file,
            "--reporters", "taurus",
            "--reporter-taurus-filename", self.report_file,
            "--suppress-exit-code", "--insecure",
        ]

        scenario = self.get_scenario()
        timeout = scenario.get('timeout', None)
        if timeout is not None:
            cmdline += ["--timeout-request", str(int(dehumanize_time(timeout) * 1000))]

        think = scenario.get('think-time', None)
        if think is not None:
            cmdline += ["--delay-request", str(int(dehumanize_time(think) * 1000))]

        cmdline += self._dump_vars("globals")
        cmdline += self._dump_vars("environment")

        load = self.get_load()
        if load.iterations:
            cmdline += ['--iteration-count', str(load.iterations)]

        # TODO: allow running several collections like directory, see https://github.com/postmanlabs/newman/issues/871
        # TODO: support hold-for, probably by having own runner
        # if load.hold:
        #    cmdline += ['--hold-for', str(load.hold)]

        self.env.set({"NODE_PATH": self.newman_tool.env.get("NODE_PATH")})
        self.env.add_path({"NODE_PATH": os.path.join(get_full_path(__file__, step_up=2), "resources")})

        self._start_subprocess(cmdline, cwd=script_dir)

    def _dump_vars(self, key):
        cmdline = []
        vals = self.get_scenario().get(key)
        if isinstance(vals, string_types):
            cmdline += ["--%s" % key, vals]
        else:
            data = {"values": []}

            if isinstance(vals, list):
                data['values'] = vals
            else:
                for varname, val in iteritems(vals):
                    data["values"] = {
                        "key": varname,
                        "value": val,
                        "type": "any",
                        "enabled": True
                    }

            fname = self.engine.create_artifact(key, ".json")
            with open(fname, "wt") as fds:
                fds.write(to_json(data))
            cmdline += ["--%s" % key, fname]
        return cmdline


class NPM(RequiredTool):
    def __init__(self, parent_logger):
        super(NPM, self).__init__("NPM", "")
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.executable = None

    def check_if_installed(self):
        candidates = ["npm"]
        if is_windows():
            candidates.append("npm.cmd")
        for candidate in candidates:
            try:
                self.log.debug("Trying %r", candidate)
                output = subprocess.check_output([candidate, '--version'], stderr=subprocess.STDOUT)
                self.log.debug("%s output: %s", candidate, output)
                self.executable = candidate
                return True
            except (CalledProcessError, OSError):
                self.log.debug("%r is not installed", candidate)
                continue
        return False

    def install(self):
        raise ToolError("Automatic installation of npm is not implemented. Install it manually")


class NPMPackage(RequiredTool):
    def __init__(self, tool_name, package_name, tools_dir, node_tool, npm_tool, parent_logger):
        super(NPMPackage, self).__init__(tool_name, "")
        self.package_name = package_name
        self.tools_dir = tools_dir
        self.node_tool = node_tool
        self.npm_tool = npm_tool
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.env = Environment(self.log, dict(os.environ))
        self.env.add_path({"NODE_PATH": os.path.join(tools_dir, "node_modules")})

    def check_if_installed(self):
        try:
            node_binary = self.node_tool.executable
            package = self.package_name
            cmdline = [node_binary, '-e', "require('%s'); console.log('%s is installed');" % (package, package)]
            self.log.debug("%s check cmdline: %s", package, cmdline)
            self.log.debug("NODE_PATH for check: %s", self.env.get("NODE_PATH"))
            output = subprocess.check_output(cmdline, env=self.env.get(), stderr=subprocess.STDOUT)
            self.log.debug("%s check output: %s", self.package_name, output)
            return True
        except (CalledProcessError, OSError):
            self.log.debug("%s check failed: %s", self.package_name, traceback.format_exc())
            return False

    def install(self):
        try:
            cmdline = [self.npm_tool.executable, 'install', self.package_name, '--prefix', self.tools_dir]
            output = subprocess.check_output(cmdline, stderr=subprocess.STDOUT)
            self.log.debug("%s install output: %s", self.tool_name, output)
            return True
        except (CalledProcessError, OSError):
            self.log.debug("%s install failed: %s", self.package_name, traceback.format_exc())
            return False


class Mocha(NPMPackage):
    def __init__(self, tools_dir, node_tool, npm_tool, parent_logger):
        super(Mocha, self).__init__("Mocha", MOCHA_NPM_PACKAGE_NAME,
                                    tools_dir, node_tool, npm_tool, parent_logger)


class JSSeleniumWebdriverPackage(NPMPackage):
    def __init__(self, tools_dir, node_tool, npm_tool, parent_logger):
        super(JSSeleniumWebdriverPackage, self).__init__("selenium-webdriver npm package",
                                                         SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME,
                                                         tools_dir, node_tool, npm_tool, parent_logger)


class WDIO(NPMPackage):
    def __init__(self, tools_dir, node_tool, npm_tool, parent_logger):
        super(WDIO, self).__init__("WebdriverIO", WDIO_NPM_PACKAGE_NAME,
                                   tools_dir, node_tool, npm_tool, parent_logger)


class WDIOMochaPlugin(NPMPackage):
    def __init__(self, tools_dir, node_tool, npm_tool, parent_logger):
        super(WDIOMochaPlugin, self).__init__("WebdriverIOMochaPlugin", WDIO_MOCHA_PLUGIN_NPM_PACKAGE_NAME,
                                              tools_dir, node_tool, npm_tool, parent_logger)


class TaurusMochaPlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusMochaPlugin, self).__init__("TaurusMochaPlugin", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus mocha plugin isn't implemented")


class TaurusWDIOPlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusWDIOPlugin, self).__init__("Taurus WebdriverIO Plugin", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus WebdriverIO plugin isn't implemented")


class Newman(NPMPackage):
    def __init__(self, tools_dir, node_tool, npm_tool, parent_logger):
        super(Newman, self).__init__("Newman", NEWMAN_NPM_PACKAGE_NAME,
                                     tools_dir, node_tool, npm_tool, parent_logger)
        self.entrypoint = "%s/node_modules/%s/bin/newman.js" % (self.tools_dir, NEWMAN_NPM_PACKAGE_NAME)


class TaurusNewmanPlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusNewmanPlugin, self).__init__("Taurus Newman Reporter", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus Newman Reporter isn't implemented")
