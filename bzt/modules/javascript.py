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
import subprocess
import traceback
from subprocess import CalledProcessError

import os
from bzt import ToolError, TaurusConfigError

from bzt.engine import HavingInstallableTools
from bzt.modules import SubprocessedExecutor
from bzt.utils import get_full_path, TclLibrary, RequiredTool, is_windows, Node

MOCHA_NPM_PACKAGE_NAME = "mocha"
SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME = "selenium-webdriver"


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
        self.reporting_setup(suffix='ldjson')

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

        self.env["NODE_PATH"] = self.mocha_tool.get_node_path_envvar()

        self._start_subprocess(mocha_cmdline)


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
        self.node_modules_dir = os.path.join(tools_dir, "node_modules")

    def get_node_path_envvar(self):
        node_path = os.environ.get("NODE_PATH")
        if node_path:
            new_path = node_path + os.pathsep + self.node_modules_dir
        else:
            new_path = self.node_modules_dir
        return new_path

    def check_if_installed(self):
        try:
            node_binary = self.node_tool.executable
            package = self.package_name
            cmdline = [node_binary, '-e', "require('%s'); console.log('%s is installed');" % (package, package)]
            self.log.debug("%s check cmdline: %s", package, cmdline)
            node_path = self.get_node_path_envvar()
            self.log.debug("NODE_PATH for check: %s", node_path)
            env = os.environ.copy()
            env["NODE_PATH"] = str(node_path)
            output = subprocess.check_output(cmdline, env=env, stderr=subprocess.STDOUT)
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


class TaurusMochaPlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusMochaPlugin, self).__init__("TaurusMochaPlugin", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus mocha plugin isn't implemented")
