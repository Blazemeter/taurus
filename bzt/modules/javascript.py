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
import traceback

from bzt import ToolError, TaurusConfigError
from bzt.engine import HavingInstallableTools
from bzt.modules import SubprocessedExecutor
from bzt.six import string_types, iteritems
from bzt.utils import TclLibrary, RequiredTool, Node, CALL_PROBLEMS
from bzt.utils import sync_run, get_full_path, is_windows, to_json, dehumanize_time

MOCHA_NPM_PACKAGE_NAME = "mocha@4.0.1"
SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME = "selenium-webdriver@3.6.0"
WDIO_NPM_PACKAGE_NAME = "webdriverio@4.8.0"
WDIO_MOCHA_PLUGIN_NPM_PACKAGE_NAME = "wdio-mocha-framework@0.5.13"
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
        tcl_lib = self.engine.instantiate_tool(TclLibrary, self)
        self.node_tool = self.engine.instantiate_tool(Node, self)
        self.npm_tool = self.engine.instantiate_tool(NPM, self)
        self.mocha_tool = self.engine.instantiate_tool(
            Mocha, self, tools_dir=self.tools_dir, node_tool=self.node_tool, npm_tool=self.npm_tool)
        web_driver = self.engine.instantiate_tool(
            JSSeleniumWebdriver, self, tools_dir=self.tools_dir, node_tool=self.node_tool, npm_tool=self.npm_tool)
        mocha_plugin = self.engine.instantiate_tool(TaurusMochaPlugin, self, tool_path=self.plugin_path)

        tools = [tcl_lib, self.node_tool, self.npm_tool, self.mocha_tool, web_driver, mocha_plugin]
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
        tcl_lib = self.engine.instantiate_tool(TclLibrary, self)
        self.node_tool = self.engine.instantiate_tool(Node, self)
        self.npm_tool = self.engine.instantiate_tool(NPM, self)
        self.wdio_tool = self.engine.instantiate_tool(
            WDIO, tools_dir=self.tools_dir, node_tool=self.node_tool, npm_tool=self.npm_tool)

        wdio_taurus_plugin = self.engine.instantiate_tool(TaurusWDIOPlugin, self, tool_path=self.plugin_path)

        wdio_mocha_plugin = self.engine.instantiate_tool(
            WDIOMochaPlugin, self, tools_dir=self.tools_dir, node_tool=self.node_tool, npm_tool=self.npm_tool)

        tools = [tcl_lib, self.node_tool, self.npm_tool, self.wdio_tool, wdio_taurus_plugin, wdio_mocha_plugin]

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
        self.node_tool = self.engine.instantiate_tool(Node, self)
        self.npm_tool = self.engine.instantiate_tool(NPM, self)
        self.newman_tool = self.engine.instantiate_tool(
            Newman, self, tools_dir=self.tools_dir, node_tool=self.node_tool, npm_tool=self.npm_tool)
        tcl_lib = self.engine.instantiate_tool(TclLibrary, self)
        taurus_newman_plugin = self.engine.instantiate_tool(TaurusNewmanPlugin, self, tool_path=self.plugin_path)

        tools = [
            self.node_tool,
            self.npm_tool,
            self.newman_tool,
            tcl_lib,
            taurus_newman_plugin
        ]

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
    def __init__(self, **kwargs):
        super(NPM, self).__init__(**kwargs)
        self.executable = None

    def check_if_installed(self):
        candidates = ["npm"]
        if is_windows():
            candidates.append("npm.cmd")
        for candidate in candidates:
            try:
                self.log.debug("Trying %r", candidate)
                output = sync_run([candidate, '--version'])
                self.log.debug("%s output: %s", candidate, output)
                self.executable = candidate
                return True
            except CALL_PROBLEMS:
                self.log.debug("%r is not installed", candidate)
                continue
        return False

    def install(self):
        raise ToolError("Automatic installation of npm is not implemented. Install it manually")


class NPMPackage(RequiredTool):
    def __init__(self, package_name, tools_dir, node_tool, npm_tool, **kwargs):
        super(NPMPackage, self).__init__(**kwargs)
        self.env.add_path({"NODE_PATH": os.path.join(tools_dir, "node_modules")})

        if "@" in package_name:
            self.package_name, self.version = package_name.split("@")
        else:
            self.package_name = package_name

        self.tools_dir = tools_dir
        self.node_tool = node_tool
        self.npm_tool = npm_tool

    def check_if_installed(self):
        try:
            cmdline = [self.node_tool.executable, "-e"]
            ok_msg = "%s is installed" % self.package_name
            cmdline.append("require('%s'); console.log('%s');" % (self.package_name, ok_msg))
            self.log.debug("%s check cmdline: %s", self.package_name, cmdline)

            self.log.debug("NODE_PATH for check: %s", self.env.get("NODE_PATH"))
            output = sync_run(cmdline, env=self.env.get())
            return ok_msg in output

        except CALL_PROBLEMS:
            self.log.debug("%s check failed: %s", self.package_name, traceback.format_exc())
            return False

    def install(self):
        try:
            package_name = self.package_name
            if self.version:
                package_name += "@" + self.version
            cmdline = [self.npm_tool.executable, 'install', package_name, '--prefix', self.tools_dir, '--no-save']
            output = sync_run(cmdline)
            self.log.debug("%s install output: %s", self.tool_name, output)
            return True

        except CALL_PROBLEMS:
            self.log.debug("%s install failed: %s", self.package_name, traceback.format_exc())
            return False


class Mocha(NPMPackage):
    def __init__(self, **kwargs):
        super(Mocha, self).__init__(MOCHA_NPM_PACKAGE_NAME, **kwargs)


class JSSeleniumWebdriver(NPMPackage):
    def __init__(self, **kwargs):
        super(JSSeleniumWebdriver, self).__init__(SELENIUM_WEBDRIVER_NPM_PACKAGE_NAME, **kwargs)


class WDIO(NPMPackage):
    def __init__(self, **kwargs):
        super(WDIO, self).__init__(WDIO_NPM_PACKAGE_NAME, **kwargs)


class WDIOMochaPlugin(NPMPackage):
    def __init__(self, **kwargs):
        super(WDIOMochaPlugin, self).__init__(WDIO_MOCHA_PLUGIN_NPM_PACKAGE_NAME, **kwargs)


class Newman(NPMPackage):
    def __init__(self, **kwargs):
        super(Newman, self).__init__(NEWMAN_NPM_PACKAGE_NAME, **kwargs)
        self.entrypoint = "%s/node_modules/%s/bin/newman.js" % (self.tools_dir, NEWMAN_NPM_PACKAGE_NAME)


class TaurusMochaPlugin(RequiredTool):
    def __init__(self, **kwargs):
        super(TaurusMochaPlugin, self).__init__(**kwargs)

    def install(self):
        raise ToolError("Automatic installation of Taurus mocha plugin isn't implemented")


class TaurusWDIOPlugin(RequiredTool):
    def __init__(self, **kwargs):
        super(TaurusWDIOPlugin, self).__init__(**kwargs)

    def install(self):
        raise ToolError("Automatic installation of Taurus WebdriverIO plugin isn't implemented")


class TaurusNewmanPlugin(RequiredTool):
    def __init__(self, **kwargs):
        super(TaurusNewmanPlugin, self).__init__(**kwargs)

    def install(self):
        raise ToolError("Automatic installation of Taurus Newman Reporter isn't implemented")
