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
import json
import os
import re
import shutil
from abc import abstractmethod

from bzt import TaurusConfigError, ToolError
from bzt.modules import SubprocessedExecutor
from bzt.modules.aggregator import ResultsReader, ConsolidatingAggregator
from bzt.modules.functional import FunctionalResultsReader, FunctionalSample
from bzt.utils import TclLibrary, RequiredTool, Node, CALL_PROBLEMS, RESOURCES_DIR, FileReader
from bzt.utils import get_full_path, is_windows, is_linux, to_json, dehumanize_time, iteritems


class JavaScriptExecutor(SubprocessedExecutor):
    def __init__(self):
        super(JavaScriptExecutor, self).__init__()
        self.tools_dir = None
        self.node = None
        self.npm = None

    def prepare(self):
        super(JavaScriptExecutor, self).prepare()
        self.tools_dir = get_full_path(self.settings.get("tools-dir", self.tools_dir))
        self.env.add_path({"NODE_PATH": os.path.join(self.tools_dir, "node_modules")})

    @abstractmethod
    def get_launch_cmdline(self, *args):
        pass

    @abstractmethod
    def get_launch_cwd(self, *args):
        pass


class IncrementalLineReader(object):
    """
    Line reader (e.g. jsonl)
    """

    def __init__(self, parent_logger, filename):
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.partial_buffer = ""
        self.file = FileReader(filename=filename, parent_logger=self.log)
        self.read_speed = 1024 * 1024

    def read(self, last_pass=False):
        """
        read data from file
        yield one complete row (ending with \n)
        :type last_pass: bool
        """
        lines = self.file.get_lines(size=self.read_speed, last_pass=last_pass)

        for line in lines:
            if not line.endswith("\n"):
                self.partial_buffer += line
                continue

            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""
            yield line

class PlaywrightTester(JavaScriptExecutor):

    """
    Playwright tests runner
    """
    def __init__(self):
        super(PlaywrightTester, self).__init__()
        self.tools_dir = get_full_path("~/.bzt/playwright")

    def get_script_path(self, required=False, scenario=None):
        if not self.execution:
            return get_full_path("~/.bzt/playwright")
        return super(PlaywrightTester, self).get_script_path(required, scenario)

    def prepare(self):
        self.tools_dir = self.get_launch_cwd()
        super(PlaywrightTester, self).prepare()

        self.env.add_path({"NODE_PATH": "node_modules"}, finish=True)
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("Script not passed to runner %s" % self)
        self.reader = PlaywrightLogReader(self.engine.artifacts_dir + "/taurus-playwright-reporter.jsonl", self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

        self.install_required_tools()

    def install_required_tools(self):
        tcl_lib = self._get_tool(TclLibrary)
        self.node = self._get_tool(Node)
        self.npm = self._get_tool(NPM)

        # Runs first so a customer declaration for it in tools_dir's package.json gets
        # resolved before PlaywrightTestPackage has to reconcile the same file.
        npm_types_node = self._get_tool(
            PlaywrightTypesNodePackage, tools_dir=self.get_launch_cwd(), node_tool=self.node, npm_tool=self.npm)

        npm_playwright_test = self._get_tool(PlaywrightTestPackage, tools_dir=self.get_launch_cwd(), node_tool=self.node, npm_tool=self.npm)
        playwright = self._get_tool(PLAYWRIGHT, tools_dir=self.get_launch_cwd())
        playwright_reporter = self._get_tool(PlaywrightCustomReporter, tools_dir=self.get_launch_cwd(), node_tool=self.node, npm_tool=self.npm)

        npm_all_packages = self._get_tool(NPMModuleInstaller,node_tool=self.node, npm_tool=self.npm, tools_dir=self.get_launch_cwd())

        # Must run last: NPMModuleInstaller may have just reified a customer-declared
        # top-level "playwright" dependency, which overwrites node_modules/.bin/playwright
        # (same bin name as @playwright/test) if it does. Reassert the correct target here,
        # after everything else that could touch it has already run.
        playwright_bin_link = self._get_tool(PlaywrightBinLink, tools_dir=self.get_launch_cwd())

        tools = [tcl_lib, self.node, self.npm, npm_types_node, npm_playwright_test, npm_all_packages,
                 playwright, playwright_reporter, playwright_bin_link]
        self._check_tools(tools)

    def get_launch_cmdline(self, *args):
        return "npx playwright test " + self.get_script_path() + ' ' + ' '.join(args[0])

    def get_launch_cwd(self, *args):
        script_path = self.get_script_path()
        if os.path.isfile(script_path):
            script_path = os.path.dirname(script_path)
        return script_path

    def startup(self):
        config = self.get_scenario().engine.config
        env = config["settings"]["env"]
        if "BASE_URL" in env:
            self.env.set({"BASE_URL": env["BASE_URL"]})

        load = self.get_load()

        max_duration = None
        concurrency = max(1, load.concurrency)
        if load.duration > 0:
            max_duration = load.duration
            if load.iterations > 0:
                repeat_each = concurrency*load.iterations
            else:
                # playwright support approx 100000 tests (= num tests * repeat_each)
                # setting repeat each to some save value. Lets customer setup iterations
                # for their test to hold for expected duration....
                repeat_each = 1000
        else:
            iterations = max(1, load.iterations)
            repeat_each = concurrency*iterations

        reporter = "@taurus/playwright-custom-reporter"

        # Add custom reporters if specified in scenario config
        custom_reporters = self.get_scenario().get("reporters", [])
        has_additional_reporter = False
        if custom_reporters and isinstance(custom_reporters, list):
            # Sanitize reporter names for command line usage
            safe_reporters = [r.translate(str.maketrans("", "", " \t\r\n\v\f'\""))
                              for r in custom_reporters if r and isinstance(r, str)]
            if safe_reporters:
                reporter = reporter + "," + ",".join(safe_reporters)
                has_additional_reporter = True

        # self.env.set({"TAURUS_PWREPORT_VERBOSE": "true"})
        # Keep stdout for custom reporters if any
        self.env.set({"TAURUS_PWREPORT_STDOUT": "true" if not has_additional_reporter else "false"})
        self.env.set({"TAURUS_PWREPORT_DIR": self.engine.artifacts_dir})
        if max_duration:
            self.env.set({"TAURUS_PWREPORT_DURATION": str(int(max_duration * 1000))})
        granularity = str(self.get_scenario().get("report-granularity", "auto")).upper().replace("-", "_")
        if granularity not in ("STEP", "TEST", "STEP_LEAF", "AUTO"):
            self.log.warning("Unknown report-granularity '%s', reporter will default to AUTO", granularity)
        self.env.set({"TAURUS_PWREPORT_GRANULARITY": granularity})
        self.env.set({"TAURUS_PWREPORT_NOREPORT_PREFIX": self.get_scenario().get("report-exclude-prefix", "")})
        options = ["--reporter \"" + reporter + "\"",
                   "--output " + self.engine.artifacts_dir + "/test-output",
                   "--workers " + str(concurrency),
                   "--repeat-each " + str(repeat_each)]

        if "browser" in self.get_scenario().data:
            options.append("--project=" + self.get_scenario().data["browser"])
        if "test" in self.get_scenario().data:
            options.append("-g '" + self.get_scenario().data["test"] + "'")

        cmd_line = self.get_launch_cmdline(options)
        self.log.info("Launching Playwright: '%s'", cmd_line)
        self.process = self._execute(cmd_line, cwd=self.get_launch_cwd())

    def has_results(self):
        return True

    def check(self):
        ret_code = self.process.poll()
        if ret_code is not None:
            if ret_code != 0:
                if ret_code == 1 and self._tests_ran():
                    self.log.debug(
                        "Playwright process exited with code 1 and tests were run - treating as normal completion"
                    )
                    return True
                msg = "Test runner %s (%s) has failed with retcode %s"
                raise ToolError(msg % (self.label, self.__class__.__name__, ret_code),
                                self.get_error_diagnostics())
            return True
        return False

    def _tests_ran(self):
        if self.reader and os.path.exists(self.reader.filename):
            return os.path.getsize(self.reader.filename) > 0
        return False


class _PlaywrightReaderMixin:
    """Shared helpers for Playwright JSONL readers."""

    def _init_jsonl(self, filename, parent_logger):
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.filename = filename
        self.jsonl_reader = IncrementalLineReader(self.log, filename)

    def _safe_ms_to_s(self, t):
        if t:
            return t / 1000.0
        return t

    def _strip_ansi(self, text):
        if not text:
            return text
        ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
        return ansi_escape.sub('', text)


class PlaywrightLogReader(_PlaywrightReaderMixin, ResultsReader):

    def __init__(self, filename, parent_logger):
        super(PlaywrightLogReader, self).__init__()
        self._init_jsonl(filename, parent_logger)

    def _read(self, final_pass=False):
        for line in self.jsonl_reader.read(final_pass):
            content = json.loads(line)
            # runDetails: title of test, worker, repetition and browser platform
            yield (int(self._safe_ms_to_s(content.get("timestamp")) or 0),
                   content.get("label"), content.get("concurency"),
                   self._safe_ms_to_s(content.get("duration")),
                   self._safe_ms_to_s(content.get("connectTime", None)),
                   self._safe_ms_to_s(content.get("latency",  None)),
                   "", # return code
                   self._strip_ansi(content.get("error", None)),
                   "", # source id
                   content.get("byte_count", 0))


class PlaywrightFuncReader(_PlaywrightReaderMixin, FunctionalResultsReader):
    """Reads Playwright custom reporter JSONL and yields FunctionalSample objects."""

    def __init__(self, filename, engine, parent_logger):
        super(PlaywrightFuncReader, self).__init__()
        self.engine = engine
        self._init_jsonl(filename, parent_logger)

    def read(self, last_pass=False):
        for line in self.jsonl_reader.read(last_pass):
            content = json.loads(line)
            label = content.get("label", "unknown")
            error_msg = self._strip_ansi(content.get("error", None))
            # prefer the reporter's "ok" field — handles expected failures (test.fail())
            # where ok=true but error message is still present
            if "ok" in content:
                status = "PASSED" if content["ok"] else "FAILED"
            else:
                status = "FAILED" if error_msg else "PASSED"
            duration = self._safe_ms_to_s(content.get("duration")) or 0
            timestamp = self._safe_ms_to_s(content.get("timestamp")) or 0

            # split label into suite and case: "suite > case" or just "case"
            parts = label.rsplit(" > ", 1)
            if len(parts) == 2:
                test_suite, test_case = parts
            else:
                test_suite = "Playwright"
                test_case = label

            yield FunctionalSample(
                test_case=test_case,
                test_suite=test_suite,
                status=status,
                start_time=timestamp,
                duration=duration,
                error_msg=error_msg or "",
                error_trace="",
            )


class MochaTester(JavaScriptExecutor):
    """
    Mocha tests runner

    :type mocha: Mocha
    :type mocha_plugin: TaurusMochaPlugin
    """

    def __init__(self):
        super(MochaTester, self).__init__()
        self.tools_dir = "~/.bzt/selenium-taurus/mocha"
        self.mocha = None
        self.mocha_plugin = None

    def prepare(self):
        super(MochaTester, self).prepare()
        self.env.add_path({"NODE_PATH": "node_modules"}, finish=True)
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("Script not passed to runner %s" % self)

        self.install_required_tools()
        self.reporting_setup(suffix='.ldjson')

    def install_required_tools(self):
        tcl_lib = self._get_tool(TclLibrary)
        self.node = self._get_tool(Node)
        self.npm = self._get_tool(NPM)
        self.mocha = self._get_tool(Mocha, tools_dir=self.tools_dir, node_tool=self.node, npm_tool=self.npm)
        self.mocha_plugin = self._get_tool(TaurusMochaPlugin)

        web_driver = self._get_tool(
            JSSeleniumWebdriver, tools_dir=self.tools_dir, node_tool=self.node, npm_tool=self.npm)

        tools = [tcl_lib, self.node, self.npm, self.mocha, self.mocha_plugin, web_driver]
        self._check_tools(tools)

    def get_launch_cmdline(self, *args):
        return [self.node.tool_path, self.mocha_plugin.tool_path] + list(args)

    def startup(self):
        mocha_cmdline = self.get_launch_cmdline(
            "--report-file",
            self.report_file,
            "--test-suite",
            self.script
        )
        load = self.get_load()
        if load.iterations:
            mocha_cmdline += ['--iterations', str(load.iterations)]

        if load.hold:
            mocha_cmdline += ['--hold-for', str(load.hold)]

        self.process = self._execute(mocha_cmdline, cwd=self.get_launch_cwd())


class NewmanExecutor(JavaScriptExecutor):
    """
    Newman-based test runner

    :type newman: Newman
    """

    def __init__(self):
        super(NewmanExecutor, self).__init__()
        self.tools_dir = "~/.bzt/newman"
        self.newman = None

    def prepare(self):
        super(NewmanExecutor, self).prepare()
        self.env.add_path({"NODE_PATH": RESOURCES_DIR})

        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("Script not passed to executor %s" % self)

        self.tools_dir = get_full_path(self.settings.get("tools-dir", self.tools_dir))
        self.install_required_tools()
        self.reporting_setup(suffix='.ldjson')

    def install_required_tools(self):
        tcl_lib = self._get_tool(TclLibrary)
        self.node = self._get_tool(Node)
        self.npm = self._get_tool(NPM)
        self.newman = self._get_tool(Newman, tools_dir=self.tools_dir, node_tool=self.node, npm_tool=self.npm)
        taurus_newman_plugin = self._get_tool(TaurusNewmanPlugin)

        tools = [tcl_lib, self.node, self.npm, self.newman, taurus_newman_plugin]

        self._check_tools(tools)

    def get_launch_cmdline(self, *args):
        return [self.node.tool_path, self.newman.tool_path] + list(args)

    def startup(self):
        script_dir = get_full_path(self.script, step_up=1)
        script_file = os.path.basename(self.script)
        cmdline = self.get_launch_cmdline(
            "run",
            script_file,
            "--reporters", "taurus",
            "--reporter-taurus-filename", self.report_file,
            "--suppress-exit-code", "--insecure",
        )

        scenario = self.get_scenario()
        timeout = scenario.get('timeout', None)
        if timeout is not None:
            cmdline += ["--timeout-request", str(int(dehumanize_time(timeout) * 1000))]

        think = scenario.get_think_time()
        if think is not None:
            cmdline += ["--delay-request", str(int(dehumanize_time(think) * 1000))]

        cmdline += self._dump_vars("globals")
        cmdline += self._dump_vars("environment")

        load = self.get_load()
        if load.iterations:
            cmdline += ['--iteration-count', str(load.iterations)]

        self.process = self._execute(cmdline, cwd=script_dir)

    def _dump_vars(self, key):
        cmdline = []
        vals = self.get_scenario().get(key)
        if isinstance(vals, str):
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
        super(NPM, self).__init__(installable=False, **kwargs)

    def check_if_installed(self):
        candidates = ["npm"]
        if is_windows():
            candidates.append("npm.cmd")
        for candidate in candidates:
            self.log.debug("Trying '%r' as NPM Tool...", candidate)
            try:
                out, err = self.call([candidate, '--version'])
            except CALL_PROBLEMS as exc:
                self.log.debug("%r is not installed: %s", candidate, exc)
                continue

            if err:
                out += err
            self.log.debug("%s output: %s", candidate, out)
            self.tool_path = candidate
            return True

        return False


OFFLINE_INSTALL_ARGS = ("--offline", "--prefer-offline")


def _frozen_store_path(*relative_parts):
    return os.path.join(get_full_path("~/.bzt/playwright"), "node_modules", *relative_parts)


def _link_frozen_path(tools_dir, relative_parts):
    """
    Symlink <tools_dir>/node_modules/<relative_parts> to the same relative path under the
    frozen store's (~/.bzt/playwright) node_modules, replacing whatever - if anything - is
    already there. Used for packages Taurus itself owns and freezes at Docker build time:
    once installed into the frozen store once, every actual test run just links to that
    single copy instead of running npm, so it never depends on the registry - or even on
    the active npm cache - being reachable, and it's immune to whichever registry is
    configured at runtime differing from the one used at build time.
    """
    source = _frozen_store_path(*relative_parts)
    target = os.path.join(tools_dir, "node_modules", *relative_parts)
    os.makedirs(os.path.dirname(target), exist_ok=True)
    if os.path.islink(target):
        os.remove(target)
    elif os.path.isdir(target):
        shutil.rmtree(target)
    elif os.path.exists(target):
        os.remove(target)
    os.symlink(source, target)


def _is_linked_to_frozen_path(tools_dir, relative_parts):
    source = _frozen_store_path(*relative_parts)
    target = os.path.join(tools_dir, "node_modules", *relative_parts)
    return os.path.islink(target) and os.path.realpath(target) == os.path.realpath(source)


def _read_frozen_installed_version(relative_parts):
    pkg_json = _frozen_store_path(*relative_parts, "package.json")
    try:
        with open(pkg_json) as fds:
            return json.load(fds).get("version")
    except (OSError, ValueError):
        return None


def _pin_package_json_dependency(tools_dir, package_name, version):
    """
    Force <package_name> to resolve to <version> in <tools_dir>/package.json, matching what's
    actually linked into node_modules there, so a later `npm install .` for the rest of a
    customer's own declared dependencies sees an already-consistent tree and leaves this
    entry alone.
    """
    pkg_json_path = os.path.join(tools_dir, "package.json")
    try:
        with open(pkg_json_path) as fds:
            data = json.load(fds)
    except (OSError, ValueError):
        data = {}
    section = "devDependencies"
    for candidate in ("dependencies", "devDependencies"):
        if package_name in data.get(candidate, {}):
            section = candidate
            break
    data.setdefault(section, {})[package_name] = "^" + version
    with open(pkg_json_path, "w") as fds:
        json.dump(data, fds, indent=2)


class NPMPackage(RequiredTool):
    PACKAGE_NAME = ""

    def __init__(self, tools_dir, node_tool, npm_tool, **kwargs):
        super(NPMPackage, self).__init__(**kwargs)
        self.package_name = self.PACKAGE_NAME
        self.is_module_package = False
        if self.package_name.startswith("@"):
            package_name_split = self.package_name.split("@")
            self.package_name = '@{}'.format(package_name_split[1])
            if len(package_name_split) > 2:
                self.version = package_name_split[2]
        elif "@" in self.package_name:
            self.package_name, self.version = self.package_name.split("@")

        self.tools_dir = tools_dir
        self.node = node_tool
        self.npm = npm_tool

    def check_if_installed(self):
        ok_msg = "%s is installed" % self.package_name

        # NODE_PATH doesn't work for ems modules - look if symlink/node_modules are present,
        # if not, change dir to tool node_modules
        process_cwd = None
        if not self.is_module_package:
            cmdline = [self.node.tool_path, "-e",
                       "require('%s'); console.log('%s');" % (self.package_name, ok_msg)]
            self.log.debug("NODE_PATH for check: %s", self.env.get("NODE_PATH"))
        else:
            cmdline = [ self.node.tool_path, "--input-type=module", "-e",
                        "import('%s').then(() => { console.log('%s'); process.exit(0); }).catch(() => process.exit(1));" % (self.package_name, ok_msg)]
            if not os.path.exists("./node_modules"):
                process_cwd = os.path.join(self.tools_dir, "node_modules")
            self.log.debug("cwd for check: %s", "." if process_cwd is None else process_cwd)

        self.log.debug("%s check cmdline: %s", self.package_name, cmdline)

        try:
            out, _ = self.call(cmdline, cwd=process_cwd)
            return ok_msg in out
        except CALL_PROBLEMS as exc:
            self.log.debug("%s check failed: %s", self.package_name, exc)
            return False

    def install(self):
        package_name = self.package_name
        if self.version:
            package_name += "@" + self.version
        cmdline = [self.npm.tool_path, 'install', package_name, '--prefix', self.tools_dir]

        try:
            out, err = self.call(cmdline)
        except CALL_PROBLEMS as exc:
            self.log.debug("%s install failed: %s", self.package_name, exc)
            self.log.warning("Failed to install %s", self.package_name)
            return

        self.log.debug("%s install stdout: %s", self.tool_name, out)
        if err:
            self.log.warning("%s install stderr: %s", self.tool_name, err)


class FrozenPackageLink(NPMPackage):
    """
    For npm packages Taurus itself owns and freezes at Docker build time (not
    customer-declared, unpredictable content): once a real npm install has happened once
    into the frozen store (~/.bzt/playwright), every actual test run just links the
    already-installed package into its own ephemeral directory instead of running npm at
    all. No install command ever touches the registry - or whichever one is currently
    configured, if it differs from the one used at build time - for these packages again.
    """

    def _frozen_version(self):
        """Version to force-link to, or None when not running frozen."""
        raise NotImplementedError

    def check_if_installed(self):
        frozen_version = self._frozen_version()
        if frozen_version is None:
            return super().check_if_installed()
        return _is_linked_to_frozen_path(self.tools_dir, self.package_name.split("/"))

    def install(self):
        frozen_version = self._frozen_version()
        if frozen_version is None:
            super().install()
            return

        _link_frozen_path(self.tools_dir, self.package_name.split("/"))
        _pin_package_json_dependency(self.tools_dir, self.package_name, frozen_version)


class NPMModulePackage(NPMPackage):
    def __init__(self, tools_dir, node_tool, npm_tool, **kwargs):
        super(NPMModulePackage, self).__init__(tools_dir, node_tool, npm_tool, **kwargs)
        self.is_module_package = True


class NPMLocalModulePackage(NPMPackage):
    PACKAGE_LOCAL_PATH = ""
    def __init__(self, tools_dir, node_tool, npm_tool, **kwargs):
        super(NPMLocalModulePackage, self).__init__(tools_dir, node_tool, npm_tool, **kwargs)

        self.is_module_package = True
        self.package_local_path = self.PACKAGE_LOCAL_PATH
        if not os.path.isabs(self.package_local_path):
            self.package_local_path = os.path.normpath(os.path.join(RESOURCES_DIR, self.package_local_path))

    def install(self):
        # This local module rarely changes between runs, so try --offline first: if npm's
        # cache from a previous run (or, in the frozen cloud image, from the Docker build
        # itself) already has everything, this succeeds instantly with no network at all.
        # Only fall back to --prefer-offline (reuse what's cached, fetch only what's
        # genuinely missing) when something truly isn't cached yet, e.g. the very first run.
        cmdline = [self.npm.tool_path, 'install', ".", '--install-links', '--prefix', self.tools_dir]

        for i, extra_arg in enumerate(OFFLINE_INSTALL_ARGS):
            try:
                out, err = self.call(cmdline + [extra_arg], cwd=self.package_local_path)
                self.log.debug("%s install stdout: %s", self.tool_name, out)
                if err:
                    self.log.warning("%s install stderr: %s", self.tool_name, err)
                return
            except CALL_PROBLEMS as exc:
                self.log.debug("%s install with %s failed: %s", self.package_name, extra_arg, exc)
                if i + 1 < len(OFFLINE_INSTALL_ARGS):
                    self.log.info("Failed to install %s with %s, retrying with %s",
                                  self.package_name, extra_arg, OFFLINE_INSTALL_ARGS[i + 1])

        self.log.warning("%s install failed with all attempts: %s", self.package_name, ", ".join(OFFLINE_INSTALL_ARGS))


class NPMModuleInstaller(NPMLocalModulePackage):
    def __init__(self, tools_dir, node_tool, npm_tool, **kwargs):
        super(NPMModuleInstaller, self).__init__(tools_dir, node_tool, npm_tool, **kwargs)
        self.package_local_path = tools_dir


class Mocha(NPMPackage):
    PACKAGE_NAME = "mocha@11.7.5"


class JSSeleniumWebdriver(NPMPackage):
    PACKAGE_NAME = "selenium-webdriver@4.23.0"

class NodeTSXModule(NPMModulePackage):
    PACKAGE_NAME = "tsx@4.19.2"

class Newman(NPMPackage):
    PACKAGE_NAME = "newman"

    def __init__(self, tools_dir="", **kwargs):
        tool_path = "%s/node_modules/%s/bin/newman.js" % (tools_dir, self.PACKAGE_NAME)
        super(Newman, self).__init__(tool_path=tool_path, tools_dir=tools_dir, **kwargs)

class TaurusMochaPlugin(RequiredTool):
    def __init__(self, **kwargs):
        tool_path = os.path.join(RESOURCES_DIR, "mocha-taurus-plugin.js")
        super(TaurusMochaPlugin, self).__init__(tool_path=tool_path, installable=False, **kwargs)

class TaurusNewmanPlugin(RequiredTool):
    def __init__(self, **kwargs):
        tool_path = os.path.join(RESOURCES_DIR, "newman-reporter-taurus.js")
        super(TaurusNewmanPlugin, self).__init__(tool_path=tool_path, installable=False, **kwargs)

class PlaywrightTypesNodePackage(FrozenPackageLink):
    """
    Playwright's own official TypeScript scaffolding (`npm init playwright@latest`) always
    adds @types/node as a devDependency alongside @playwright/test. It has no runtime code
    at all (pure .d.ts type declarations, consulted only by the TypeScript compiler - never
    by Node's own require()/import), but it typically lives in the SAME package.json that
    PlaywrightTestPackage/PLAYWRIGHT also install into (the customer's own tools_dir). An
    unresolvable copy of it (uncached, registry unreachable) can block THEIR install too,
    since npm's reify step resolves the whole declared tree in that directory, not just the
    specifically-requested package.
    """
    PACKAGE_NAME = "@types/node"

    def _frozen_version(self):
        if os.environ.get("PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION", None) is None:
            return None
        return _read_frozen_installed_version(("@types", "node"))


class PlaywrightTestPackage(FrozenPackageLink):
    PACKAGE_NAME = "@playwright/test"

    def _frozen_version(self):
        if os.environ.get("PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION", None) is None:
            return None
        return _read_frozen_installed_version(("@playwright", "test"))


class PlaywrightBinLink(RequiredTool):
    """
    The unscoped `playwright` package declares the same `bin.playwright` entry as
    @playwright/test. If a customer's own package.json also depends on plain `playwright`
    (common - their own test code often imports it directly), NPMModuleInstaller's reify of
    that dependency overwrites node_modules/.bin/playwright to point at THAT package's
    cli.js instead, so `npx playwright test` ends up loading a second, different
    @playwright/test instance internally and fails with "did not expect test() to be
    called here". Runs last in install_required_tools() (after NPMModuleInstaller) to
    reassert the correct target every time, regardless of what ran in between.
    """

    def __init__(self, tools_dir, **kwargs):
        super(PlaywrightBinLink, self).__init__(installable=True, **kwargs)
        self.tools_dir = tools_dir

    def check_if_installed(self):
        if os.environ.get("PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION", None) is None:
            return True
        return _is_linked_to_frozen_path(self.tools_dir, (".bin", "playwright"))

    def install(self):
        _link_frozen_path(self.tools_dir, (".bin", "playwright"))


class PlaywrightCustomReporter(NPMLocalModulePackage):
    """
    Ships inside the bzt package itself (RESOURCES_DIR), so there's no registry involved -
    but `npm install . --prefix <tools_dir>` still reifies the customer's entire package.json
    in that directory, so an unrelated uncached customer dependency sharing the same
    tools_dir collaterally blocks this too. At frozen cloud runtime, link the copy already
    installed once into the frozen store (~/.bzt/playwright) at Docker build time instead,
    the same way as the registry-backed frozen packages.
    """
    PACKAGE_NAME = "@taurus/playwright-custom-reporter@1.0.1"
    PACKAGE_LOCAL_PATH = "./playwright-custom-reporter"

    def _frozen(self):
        return os.environ.get("PLAYWRIGHT_TEST_PACKAGE_FORCED_VERSION", None) is not None

    def check_if_installed(self):
        if not self._frozen():
            # always run install for local module to update to latest version
            # npm version resolving for local modules is not reliable
            return False
        return _is_linked_to_frozen_path(self.tools_dir, self.package_name.split("/"))

    def install(self):
        if not self._frozen():
            super().install()
            return
        _link_frozen_path(self.tools_dir, self.package_name.split("/"))

class PLAYWRIGHT(RequiredTool):
    def __init__(self, tools_dir, **kwargs):
        super(PLAYWRIGHT, self).__init__(installable=True, **kwargs)
        self.tools_dir = tools_dir

    def check_if_installed(self):
        # currently there seems to be no reliable way to find out whether all Playwright requirements are installed
        return False

    def install(self):
        frozen_version = os.environ.get("PLAYWRIGHT_PACKAGE_FORCED_VERSION", None)
        package_name = "playwright" if frozen_version is None else "playwright@" + frozen_version
        version_changed = False
        if frozen_version:
            # `npx --no` reads the locally-installed version without fetching from the
            # registry, run against ~/.bzt/playwright: taurus-cloud's Dockerfile-reduced
            # freezes the version by installing into that directory.
            cmdline = ["npx", "--no", "--", "playwright", "--version"]
            try:
                out, _ = self.call(cmdline, cwd=get_full_path("~/.bzt/playwright"))
                installed = (out or "").strip().split()[-1] if (out or "").strip() else ""
                version_changed = installed != frozen_version
                if version_changed:
                    self.log.warning("Frozen version not found in installed packages, will re-install %s", package_name)
            except CALL_PROBLEMS as exc:
                self.log.debug("%s check of forced version failed: %s", package_name, exc)
                version_changed = True

        # npx playwright install is not needed to run again if version did not change and is frozen
        if frozen_version is None or version_changed:
            # Do not install deps for browsers if we know it will fail because of user permissions (linux & non-root)
            if is_linux() and hasattr(os, "geteuid") and os.geteuid() != 0:
                self.install_cmd(cmdline = ["npx", package_name, "install"])
            else:
                self.install_cmd(cmdline = ["npx", package_name, "install", "--with-deps"])

    def install_cmd(self, cmdline):
        self.log.debug("Installing Playwright: %s", cmdline)
        if not os.path.exists(self.tools_dir):
            self.log.debug("Creating directory: %s", self.tools_dir)
            os.makedirs(self.tools_dir, exist_ok=True)

        try:
            out, err = self.call(cmdline,cwd=self.tools_dir)
        except CALL_PROBLEMS as exc:
            self.log.warning("'%s' install failed: %s", cmdline, exc)
            return
        if out:
            self.log.debug("%s install stdout\n: %s", self.tool_name, out)
        if err:
            self.log.warning("%s install stderr\n: %s", self.tool_name, err)