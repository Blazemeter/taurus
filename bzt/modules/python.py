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
import ast
import os
import re
import shlex
import sys
import time
from abc import abstractmethod
from collections import OrderedDict
from subprocess import CalledProcessError

import astunparse
import yaml

from bzt import ToolError, TaurusConfigError, TaurusInternalException
from bzt.engine import HavingInstallableTools, Scenario, SETTINGS
from bzt.modules import SubprocessedExecutor, ConsolidatingAggregator, FuncSamplesReader, FunctionalAggregator
from bzt.modules.aggregator import ResultsReader
from bzt.modules.functional import FunctionalResultsReader
from bzt.modules.jmeter import JTLReader
from bzt.requests_model import HTTPRequest
from bzt.six import parse, string_types, iteritems, text_type
from bzt.utils import BetterDict, ensure_is_dict, shell_exec, FileReader
from bzt.utils import get_full_path, RequiredTool, PythonGenerator, dehumanize_time

IGNORED_LINE = re.compile(r"[^,]+,Total:\d+ Passed:\d+ Failed:\d+")


class ApiritifNoseExecutor(SubprocessedExecutor):
    """
    :type _readers: list[JTLReader]
    """

    def __init__(self):
        super(ApiritifNoseExecutor, self).__init__()
        self._tailer = FileReader(file_opener=lambda _: None, parent_logger=self.log)

    def reporting_setup(self, prefix=None, suffix=None):
        if not self.reported:
            self.log.debug("Skipping reporting setup for executor %s", self)
            return

        if self.engine.is_functional_mode():
            self.reader = ApiritifFuncReader(self.engine, self.log)
        else:
            self.reader = ApiritifLoadReader(self.log)

        if not self.register_reader:
            self.log.debug("Skipping reader registration for executor %s", self)
            return

        if isinstance(self.engine.aggregator, (ConsolidatingAggregator, FunctionalAggregator)):
            self.engine.aggregator.add_underling(self.reader)

    def prepare(self):
        self.script = self.get_script_path()
        if not self.script:
            if "requests" in self.get_scenario():
                self.script = self.__tests_from_requests()
            else:
                raise TaurusConfigError("Nothing to test, no requests were provided in scenario")
        self.reporting_setup()  # no prefix/suffix because we don't fully control report file names

    def __tests_from_requests(self):
        filename = self.engine.create_artifact("test_requests", ".py")
        test_mode = self.execution.get("test-mode", None) or "apiritif"
        if test_mode == "apiritif":
            builder = ApiritifScriptGenerator(self.get_scenario(), self.log)
            builder.verbose = self.__is_verbose()
        else:
            wdlog = self.engine.create_artifact('webdriver', '.log')
            builder = SeleniumScriptBuilder(self.get_scenario(), self.log, wdlog)
        builder.build_source_code()
        builder.save(filename)
        return filename

    def startup(self):
        executable = self.settings.get("interpreter", sys.executable)

        self.env.add_path({"PYTHONPATH": get_full_path(__file__, step_up=3)})

        report_type = ".ldjson" if self.engine.is_functional_mode() else ".csv"
        report_tpl = self.engine.create_artifact("apiritif-", "") + "%s" + report_type
        cmdline = [executable, "-m", "apiritif.loadgen", '--result-file-template', report_tpl]

        load = self.get_load()
        if load.concurrency:
            cmdline += ['--concurrency', str(load.concurrency)]

        if load.iterations:
            cmdline += ['--iterations', str(load.iterations)]

        if load.hold:
            cmdline += ['--hold-for', str(load.hold)]

        if load.ramp_up:
            cmdline += ['--ramp-up', str(load.ramp_up)]

        if load.steps:
            cmdline += ['--steps', str(load.steps)]

        if self.__is_verbose():
            cmdline += ['--verbose']

        cmdline += [self.script]
        self.start_time = time.time()
        self._start_subprocess(cmdline)
        self._tailer = FileReader(filename=self.stdout_file, parent_logger=self.log)

    def has_results(self):
        if not self.reader:
            return False
        return self.reader.read_records > 0

    def check(self):
        for line in self._tailer.get_lines():
            if "Adding worker" in line:
                marker = "results="
                pos = line.index(marker)
                fname = line[pos + len(marker):].strip()
                self.log.debug("Adding result reader for %s", fname)
                self.reader.register_file(fname)

        return super(ApiritifNoseExecutor, self).check()

    def __log_lines(self):
        lines = []
        for line in self._tailer.get_lines():
            if not IGNORED_LINE.match(line):
                lines.append(line)

        if lines:
            self.log.info("\n".join(lines))

    def post_process(self):
        super(ApiritifNoseExecutor, self).post_process()
        self.__log_lines()

    def __is_verbose(self):
        engine_verbose = self.engine.config.get(SETTINGS).get("verbose", False)
        executor_verbose = self.settings.get("verbose", engine_verbose)
        return executor_verbose


class NoseTester(ApiritifNoseExecutor):
    pass


class ApiritifLoadReader(ResultsReader):
    def __init__(self, parent_log):
        super(ApiritifLoadReader, self).__init__()
        self.log = parent_log.getChild(self.__class__.__name__)
        self.filenames = []
        self.readers = []
        self.read_records = False

    def register_file(self, report_filename):
        self.filenames.append(report_filename)
        reader = JTLReader(report_filename, self.log)
        self.readers.append(reader)

    def _read(self, final_pass=False):
        for reader in self.readers:
            if not self.read_records:
                self.read_records = True
            for sample in reader._read(final_pass):
                yield sample


class ApiritifFuncReader(FunctionalResultsReader):
    def __init__(self, engine, parent_log):
        super(ApiritifFuncReader, self).__init__()
        self.engine = engine
        self.log = parent_log.getChild(self.__class__.__name__)
        self.filenames = []
        self.readers = []
        self.read_records = False

    def register_file(self, report_filename):
        self.filenames.append(report_filename)
        reader = FuncSamplesReader(report_filename, self.engine, self.log)
        self.readers.append(reader)

    def read(self, last_pass=False):
        for reader in self.readers:
            for sample in reader.read(last_pass):
                if not self.read_records:
                    self.read_records = True
                yield sample


class SeleniumScriptBuilder(PythonGenerator):
    """
    :type window_size: tuple[int,int]
    """
    IMPORTS = """import unittest
import re
from time import sleep
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait

import apiritif
"""

    def __init__(self, scenario, parent_logger, wdlog):
        super(SeleniumScriptBuilder, self).__init__(scenario, parent_logger)
        self.window_size = None
        self.wdlog = wdlog

    def build_source_code(self):
        self.log.debug("Generating Test Case test methods")
        imports = self.add_imports()
        self.root.append(imports)
        test_class = self.gen_class_definition("TestRequests", ["unittest.TestCase"])
        self.root.append(test_class)
        test_class.append(self.gen_setup_method())
        test_class.append(self.gen_teardown_method())

        requests = self.scenario.get_requests(require_url=False)
        default_address = self.scenario.get("default-address", None)
        test_method = self.gen_test_method('test_requests')
        self.gen_setup(test_method)

        for req in requests:
            if req.label:
                label = req.label
            elif req.url:
                label = req.url
            else:
                raise TaurusConfigError("You must specify at least 'url' or 'label' for each requests item")

            test_method.append(self.gen_statement('with apiritif.transaction(%r):' % label, indent=8))
            transaction_contents = []

            if req.url is not None:
                parsed_url = parse.urlparse(req.url)
                if default_address is not None and not parsed_url.netloc:
                    url = default_address + req.url
                else:
                    url = req.url
                if req.timeout is not None:
                    test_method.append(self.gen_impl_wait(req.timeout, indent=12))
                transaction_contents.append(self.gen_statement("self.driver.get(%r)" % url, indent=12))
                transaction_contents.append(self.gen_new_line(indent=0))

            actions = req.config.get("actions", [])
            for action_config in actions:
                transaction_contents.append(self.gen_action(action_config, indent=12))
            if actions:
                transaction_contents.append(self.gen_new_line(indent=0))

            if transaction_contents:
                for line in transaction_contents:
                    test_method.append(line)
            else:
                test_method.append(self.gen_statement('pass', indent=12))
            test_method.append(self.gen_new_line(indent=0))

            if "assert" in req.config:
                test_method.append(self.gen_statement("body = self.driver.page_source"))
                for assert_config in req.config.get("assert"):
                    for elm in self.gen_assertion(assert_config):
                        test_method.append(elm)
                test_method.append(self.gen_new_line(indent=0))

            think_time = req.priority_option('think-time')
            if think_time is not None:
                test_method.append(self.gen_statement("sleep(%s)" % dehumanize_time(think_time)))
                test_method.append(self.gen_new_line(indent=0))

        test_class.append(test_method)

    def _add_url_request(self, default_address, req, test_method):
        parsed_url = parse.urlparse(req.url)
        if default_address is not None and not parsed_url.netloc:
            url = default_address + req.url
        else:
            url = req.url
        if req.timeout is not None:
            test_method.append(self.gen_impl_wait(req.timeout))
        test_method.append(self.gen_statement("self.driver.get('%s')" % url))

    def gen_setup(self, test_method):
        timeout = self.scenario.get("timeout", None)
        if timeout is None:
            timeout = '30s'
        scenario_timeout = dehumanize_time(timeout)
        test_method.append(self.gen_impl_wait(scenario_timeout))
        test_method.append(self.gen_new_line(indent=0))

    def gen_setup_method(self):
        self.log.debug("Generating setUp test method")
        browsers = ["Firefox", "Chrome", "Ie", "Opera"]
        browser = self.scenario.get("browser", "Firefox")
        if browser not in browsers:
            raise TaurusConfigError("Unsupported browser name: %s" % browser)

        setup_method_def = self.gen_method_definition("setUp", ["self"])

        if browser == 'Firefox':
            setup_method_def.append(self.gen_statement("profile = webdriver.FirefoxProfile()"))
            statement = "profile.set_preference('webdriver.log.file', %s)" % repr(self.wdlog)
            log_set = self.gen_statement(statement)
            setup_method_def.append(log_set)
            setup_method_def.append(self.gen_statement("self.driver = webdriver.Firefox(profile)"))
        elif browser == 'Chrome':
            statement = "self.driver = webdriver.Chrome(service_log_path=%s)"
            setup_method_def.append(self.gen_statement(statement % repr(self.wdlog)))
        else:
            setup_method_def.append(self.gen_statement("self.driver = webdriver.%s()" % browser))

        scenario_timeout = self.scenario.get("timeout", None)
        if scenario_timeout is None:
            scenario_timeout = '30s'
        setup_method_def.append(self.gen_impl_wait(scenario_timeout))
        if self.window_size:  # FIXME: unused in fact
            statement = self.gen_statement("self.driver.set_window_position(0, 0)")
            setup_method_def.append(statement)

            args = (self.window_size[0], self.window_size[1])
            statement = self.gen_statement("self.driver.set_window_size(%s, %s)" % args)
            setup_method_def.append(statement)
        else:
            pass  # TODO: setup_method_def.append(self.gen_statement("self.driver.fullscreen()"))

        setup_method_def.append(self.gen_new_line(indent=0))
        return setup_method_def

    def gen_impl_wait(self, timeout, indent=8):
        return self.gen_statement("self.driver.implicitly_wait(%s)" % dehumanize_time(timeout), indent=indent)

    def gen_test_method(self, name):
        self.log.debug("Generating test method %s", name)
        test_method = self.gen_method_definition(name, ["self"])
        return test_method

    def gen_teardown_method(self):
        self.log.debug("Generating tearDown test method")
        tear_down_method_def = self.gen_method_definition("tearDown", ["self"])
        tear_down_method_def.append(self.gen_statement("self.driver.quit()"))
        tear_down_method_def.append(self.gen_new_line(indent=0))
        return tear_down_method_def

    def gen_assertion(self, assertion_config):
        self.log.debug("Generating assertion, config: %s", assertion_config)
        assertion_elements = []

        if isinstance(assertion_config, string_types):
            assertion_config = {"contains": [assertion_config]}

        for val in assertion_config["contains"]:
            regexp = assertion_config.get("regexp", True)
            reverse = assertion_config.get("not", False)
            subject = assertion_config.get("subject", "body")
            if subject != "body":
                raise TaurusConfigError("Only 'body' subject supported ")

            assert_message = "'%s' " % val
            if not reverse:
                assert_message += 'not '
            assert_message += 'found in BODY'

            if regexp:
                assert_method = "self.assertEqual" if reverse else "self.assertNotEqual"
                assertion_elements.append(self.gen_statement("re_pattern = re.compile(r'%s')" % val))

                method = '%s(0, len(re.findall(re_pattern, body)), "Assertion: %s")'
                method %= assert_method, assert_message
                assertion_elements.append(self.gen_statement(method))
            else:
                assert_method = "self.assertNotIn" if reverse else "self.assertIn"
                method = '%s("%s", body, "Assertion: %s")'
                method %= assert_method, val, assert_message
                assertion_elements.append(self.gen_statement(method))
        return assertion_elements

    def gen_action(self, action_config, indent=8):
        aby, atype, param, selector = self._parse_action(action_config)

        bys = {
            'byxpath': "XPATH",
            'bycss': "CSS_SELECTOR",
            'byname': "NAME",
            'byid': "ID",
            'bylinktext': "LINK_TEXT"
        }
        if atype in ('click', 'keys'):
            tpl = "self.driver.find_element(By.%s, %r).%s"
            if atype == 'click':
                action = "click()"
            else:
                action = "send_keys(%r)" % param

            return self.gen_statement(tpl % (bys[aby], selector, action), indent=indent)
        elif atype == 'wait':
            tpl = "WebDriverWait(self.driver, %s).until(econd.%s_of_element_located((By.%s, %r)), %r)"
            mode = "visibility" if param == 'visible' else 'presence'
            exc = TaurusConfigError("wait action requires timeout in scenario: \n%s" % self.scenario)
            timeout = dehumanize_time(self.scenario.get("timeout", exc))
            errmsg = "Element %r failed to appear within %ss" % (selector, timeout)
            return self.gen_statement(tpl % (timeout, mode, bys[aby], selector, errmsg), indent=indent)
        elif atype == 'pause' and aby == 'for':
            tpl = "sleep(%.f)"
            return self.gen_statement(tpl % (dehumanize_time(selector),), indent=indent)
        elif atype == 'clear' and aby == 'cookies':
            return self.gen_statement("self.driver.delete_all_cookies()", indent=indent)

        raise TaurusInternalException("Could not build code for action: %s" % action_config)

    def _parse_action(self, action_config):
        if isinstance(action_config, string_types):
            name = action_config
            param = None
        elif isinstance(action_config, dict):
            name, param = next(iteritems(action_config))
        else:
            raise TaurusConfigError("Unsupported value for action: %s" % action_config)

        actions = "click|wait|keys|pause|clear"
        bys = "byName|byID|byCSS|byXPath|byLinkText|For|Cookies"
        expr = re.compile("^(%s)(%s)\((.*)\)$" % (actions, bys), re.IGNORECASE)
        res = expr.match(name)
        if not res:
            raise TaurusConfigError("Unsupported action: %s" % name)

        atype = res.group(1).lower()
        aby = res.group(2).lower()
        selector = res.group(3)

        # hello, reviewer!
        if selector.startswith('"') and selector.endswith('"'):
            selector = selector[1:-1]
        elif selector.startswith("'") and selector.endswith("'"):
            selector = selector[1:-1]

        return aby, atype, param, selector


class ApiritifScriptGenerator(PythonGenerator):
    # Python AST docs: https://greentreesnakes.readthedocs.io/en/latest/

    def __init__(self, scenario, parent_log):
        super(ApiritifScriptGenerator, self).__init__(scenario, parent_log)
        self.scenario = scenario
        self.log = parent_log.getChild(self.__class__.__name__)
        self.tree = None
        self.verbose = False
        self.expr_compiler = JMeterExprCompiler(self.log)
        self.__access_method = None

    def gen_empty_line_stmt(self):
        return ast.Expr(value=ast.Name(id=" "))

    def gen_module(self):
        stmts = [
            ast.Import(names=[ast.alias(name='logging', asname=None)]),
            ast.Import(names=[ast.alias(name='random', asname=None)]),
            ast.Import(names=[ast.alias(name='string', asname=None)]),
            ast.Import(names=[ast.alias(name='sys', asname=None)]),
            ast.Import(names=[ast.alias(name='time', asname=None)]),
            self.gen_empty_line_stmt(),

            ast.Import(names=[ast.alias(name='apiritif', asname=None)]),  # or "from apiritif import http, utils"?
        ]

        if self.verbose:
            stmts.append(self.gen_empty_line_stmt())
            stmts.extend(ast.parse("""\
log = logging.getLogger('apiritif.http')
log.addHandler(logging.StreamHandler(sys.stdout))
log.setLevel(logging.DEBUG)
""").body)
        stmts.append(self.gen_empty_line_stmt())
        stmts.append(self.gen_empty_line_stmt())
        stmts.append(self.gen_classdef())
        return ast.Module(body=stmts)

    def gen_classdef(self):
        return ast.ClassDef(
            name='TestAPIRequests',
            bases=[],
            body=[self.gen_test_method()],
            keywords=[],
            starargs=None,
            kwargs=None,
            decorator_list=[],
        )

    def gen_test_method(self):
        return ast.FunctionDef(
            name='test_requests',
            args=ast.arguments(
                args=[ast.Name(id='self', ctx=ast.Param())],
                defaults=[],
                vararg=None,
                kwonlyargs=[],
                kw_defaults=[],
                kwarg=None,
                returns=None,
            ),
            body=self.gen_test_method_body(),
            decorator_list=[],
        )

    def gen_expr(self, value):
        return self.expr_compiler.gen_expr(value)

    def _gen_target_setup(self, key, value):
        return ast.Expr(value=ast.Call(
            func=ast.Attribute(value=ast.Name(id='target', ctx=ast.Load()),
                               attr=key, ctx=ast.Load()),
            args=[self.gen_expr(value)],
            keywords=[],
            starargs=None,
            kwargs=None
        ))

    def gen_init(self):
        keepalive = self.scenario.get("keepalive", None)
        default_address = self.scenario.get("default-address", None)
        base_path = self.scenario.get("base-path", None)
        auto_assert_ok = self.scenario.get("auto-assert-ok", True)
        store_cookie = self.scenario.get("store-cookie", None)
        timeout = self.scenario.get("timeout", None)
        follow_redirects = self.scenario.get("follow-redirects", True)

        if default_address is not None or keepalive or store_cookie:
            self.__access_method = "target"
        else:
            self.__access_method = "plain"

        if keepalive is None:
            keepalive = True
        if store_cookie is None:
            store_cookie = True

        lines = []
        if self.__access_method == "target":
            http = ast.Attribute(value=ast.Name(id='apiritif', ctx=ast.Load()), attr='http', ctx=ast.Load())
            lines = [
                ast.Assign(
                    targets=[
                        ast.Name(id="target", ctx=ast.Store())
                    ],
                    value=ast.Call(
                        func=ast.Attribute(value=http, attr='target', ctx=ast.Load()),
                        args=[self.gen_expr(default_address)],
                        keywords=[],
                        starargs=None,
                        kwargs=None
                    )
                ),
                self._gen_target_setup('keep_alive', keepalive),
                self._gen_target_setup('auto_assert_ok', auto_assert_ok),
                self._gen_target_setup('use_cookies', store_cookie),
                self._gen_target_setup('allow_redirects', follow_redirects),
            ]
            if base_path:
                lines.append(self._gen_target_setup('base_path', base_path))
            if timeout is not None:
                lines.append(self._gen_target_setup('timeout', dehumanize_time(timeout)))
        return lines

    def _extract_named_args(self, req):
        named_args = OrderedDict()
        if req.timeout is not None:
            named_args['timeout'] = dehumanize_time(req.timeout)
        if req.follow_redirects is not None:
            named_args['allow_redirects'] = req.priority_option('follow-redirects', default=True)

        headers = {}
        scenario_headers = self.scenario.get("headers", None)
        if scenario_headers:
            headers.update(scenario_headers)
        if req.headers:
            headers.update(req.headers)
        if headers:
            named_args['headers'] = self.gen_expr(headers)

        merged_headers = dict([(key.lower(), value) for key, value in iteritems(headers)])
        content_type = merged_headers.get('content-type', None)

        if content_type == 'application/json' and isinstance(req.body, (dict, list)):  # json request body
            named_args['json'] = self.gen_expr(req.body)
        elif req.method.lower() == "get" and isinstance(req.body, dict):  # request URL params (?a=b&c=d)
            named_args['params'] = self.gen_expr(req.body)
        elif isinstance(req.body, dict):  # form data
            named_args['data'] = self.gen_expr(list(iteritems(req.body)))
        elif isinstance(req.body, string_types):
            named_args['data'] = self.gen_expr(req.body)
        elif req.body:
            msg = "Cannot handle 'body' option of type %s: %s"
            raise TaurusConfigError(msg % (type(req.body), req.body))

        return named_args

    def gen_request_lines(self, req):
        apiritif_http = ast.Attribute(value=ast.Name(id='apiritif', ctx=ast.Load()),
                                      attr='http', ctx=ast.Load())
        target = ast.Name(id='target', ctx=ast.Load())
        requestor = target if self.__access_method == "target" else apiritif_http

        method = req.method.lower()
        think_time = dehumanize_time(req.priority_option('think-time', default=None))
        named_args = self._extract_named_args(req)

        if req.label:
            label = req.label
        else:
            label = req.url

        lines = []

        tran = ast.Attribute(value=ast.Name(id='apiritif', ctx=ast.Load()),
                             attr="transaction", ctx=ast.Load())
        transaction = ast.With(
            context_expr=ast.Call(
                func=tran,
                args=[self.gen_expr(label)],
                keywords=[],
                starargs=None,
                kwargs=None
            ),
            optional_vars=None,
            body=[
                ast.Assign(
                    targets=[
                        ast.Name(id="response", ctx=ast.Store())
                    ],
                    value=ast.Call(
                        func=ast.Attribute(value=requestor, attr=method, ctx=ast.Load()),
                        args=[self.gen_expr(req.url)],
                        keywords=[ast.keyword(arg=name, value=self.gen_expr(value))
                                  for name, value in iteritems(named_args)],
                        starargs=None,
                        kwargs=None
                    )
                )
            ],
        )
        lines.append(transaction)

        lines.extend(self._gen_assertions(req))
        lines.extend(self._gen_jsonpath_assertions(req))
        lines.extend(self._gen_xpath_assertions(req))
        lines.extend(self._gen_extractors(req))

        if think_time:
            lines.append(ast.Expr(ast.Call(func=ast.Attribute(value=ast.Name(id="time", ctx=ast.Load()),
                                                              attr="sleep",
                                                              ctx=ast.Load()),
                                           args=[self.gen_expr(think_time)],
                                           keywords=[],
                                           starargs=None,
                                           kwargs=None)))

        return lines

    def _gen_assertions(self, request):
        stmts = []
        assertions = request.config.get("assert", [])
        for idx, assertion in enumerate(assertions):
            assertion = ensure_is_dict(assertions, idx, "contains")
            if not isinstance(assertion['contains'], list):
                assertion['contains'] = [assertion['contains']]
            subject = assertion.get("subject", Scenario.FIELD_BODY)
            if subject in (Scenario.FIELD_BODY, Scenario.FIELD_HEADERS):
                for member in assertion["contains"]:
                    func_table = {
                        (Scenario.FIELD_BODY, False, False): "assert_in_body",
                        (Scenario.FIELD_BODY, False, True): "assert_not_in_body",
                        (Scenario.FIELD_BODY, True, False): "assert_regex_in_body",
                        (Scenario.FIELD_BODY, True, True): "assert_regex_not_in_body",
                        (Scenario.FIELD_HEADERS, False, False): "assert_in_headers",
                        (Scenario.FIELD_HEADERS, False, True): "assert_not_in_headers",
                        (Scenario.FIELD_HEADERS, True, False): "assert_regex_in_headers",
                        (Scenario.FIELD_HEADERS, True, True): "assert_regex_not_in_headers",
                    }
                    method = func_table[(subject, assertion.get('regexp', True), assertion.get('not', False))]
                    stmts.append(ast.Expr(
                        ast.Call(
                            func=ast.Attribute(
                                value=ast.Name(id="response", ctx=ast.Load()),
                                attr=method,
                                ctx=ast.Load()
                            ),
                            args=[self.gen_expr(member)],
                            keywords=[],
                            starargs=None,
                            kwargs=None
                        )
                    ))
            elif subject == Scenario.FIELD_RESP_CODE:
                for member in assertion["contains"]:
                    method = "assert_status_code" if not assertion.get('not', False) else "assert_not_status_code"
                    stmts.append(ast.Expr(
                        ast.Call(
                            func=ast.Attribute(
                                value=ast.Name(id="response", ctx=ast.Load()),
                                attr=method,
                                ctx=ast.Load()
                            ),
                            args=[self.gen_expr(member)],
                            keywords=[],
                            starargs=None,
                            kwargs=None
                        )
                    ))
        return stmts

    def _gen_jsonpath_assertions(self, request):
        stmts = []
        jpath_assertions = request.config.get("assert-jsonpath", [])
        for idx, assertion in enumerate(jpath_assertions):
            assertion = ensure_is_dict(jpath_assertions, idx, "jsonpath")
            exc = TaurusConfigError('JSON Path not found in assertion: %s' % assertion)
            query = assertion.get('jsonpath', exc)
            expected = assertion.get('expected-value', '') or None
            method = "assert_not_jsonpath" if assertion.get('invert', False) else "assert_jsonpath"
            stmts.append(ast.Expr(
                ast.Call(
                    func=ast.Attribute(
                        value=ast.Name(id="response", ctx=ast.Load()),
                        attr=method,
                        ctx=ast.Load()
                    ),
                    args=[self.gen_expr(query)],
                    keywords=[ast.keyword(arg="expected_value", value=self.gen_expr(expected))],
                    starargs=None,
                    kwargs=None
                )
            ))

        return stmts

    def _gen_xpath_assertions(self, request):
        stmts = []
        jpath_assertions = request.config.get("assert-xpath", [])
        for idx, assertion in enumerate(jpath_assertions):
            assertion = ensure_is_dict(jpath_assertions, idx, "xpath")
            exc = TaurusConfigError('XPath not found in assertion: %s' % assertion)
            query = assertion.get('xpath', exc)
            parser_type = 'html' if assertion.get('use-tolerant-parser', True) else 'xml'
            validate = assertion.get('validate-xml', False)
            method = "assert_not_xpath" if assertion.get('invert', False) else "assert_xpath"
            stmts.append(ast.Expr(
                ast.Call(
                    func=ast.Attribute(
                        value=ast.Name(id="response", ctx=ast.Load()),
                        attr=method,
                        ctx=ast.Load()
                    ),
                    args=[self.gen_expr(query)],
                    keywords=[ast.keyword(arg="parser_type", value=self.gen_expr(parser_type)),
                              ast.keyword(arg="validate", value=self.gen_expr(validate))],
                    starargs=None,
                    kwargs=None
                )
            ))

        return stmts

    def _gen_extractors(self, request):
        stmts = []
        jextractors = request.config.get("extract-jsonpath", BetterDict())
        for varname in jextractors:
            cfg = ensure_is_dict(jextractors, varname, "jsonpath")
            stmts.append(ast.Assign(
                targets=[ast.Name(id=varname, ctx=ast.Store())],
                value=ast.Call(
                    func=ast.Attribute(
                        value=ast.Name(id="response", ctx=ast.Load()),
                        attr="extract_jsonpath",
                        ctx=ast.Load()
                    ),
                    args=[self.gen_expr(cfg['jsonpath']), self.gen_expr(cfg.get('default', 'NOT_FOUND'))],
                    keywords=[],
                    starargs=None,
                    kwargs=None
                )
            ))

        extractors = request.config.get("extract-regexp", BetterDict())
        for varname in extractors:
            cfg = ensure_is_dict(extractors, varname, "regexp")
            # TODO: support non-'body' value of 'subject'
            stmts.append(ast.Assign(
                targets=[ast.Name(id=varname, ctx=ast.Store())],
                value=ast.Call(
                    func=ast.Attribute(
                        value=ast.Name(id="response", ctx=ast.Load()),
                        attr="extract_regex",
                        ctx=ast.Load()
                    ),
                    args=[self.gen_expr(cfg['regexp']), self.gen_expr(cfg.get('default', 'NOT_FOUND'))],
                    keywords=[],
                    starargs=None,
                    kwargs=None
                )
            ))

        # TODO: css/jquery extractor?

        xpath_extractors = request.config.get("extract-xpath", BetterDict())
        for varname in xpath_extractors:
            cfg = ensure_is_dict(xpath_extractors, varname, "xpath")
            parser_type = 'html' if cfg.get('use-tolerant-parser', True) else 'xml'
            validate = cfg.get('validate-xml', False)
            stmts.append(ast.Assign(
                targets=[ast.Name(id=varname, ctx=ast.Store())],
                value=ast.Call(
                    func=ast.Attribute(
                        value=ast.Name(id="response", ctx=ast.Load()),
                        attr="extract_xpath",
                        ctx=ast.Load()
                    ),
                    args=[self.gen_expr(cfg['xpath'])],
                    keywords=[ast.keyword(arg="default", value=cfg.get('default', 'NOT_FOUND')),
                              ast.keyword(arg="parser_type", value=parser_type),
                              ast.keyword(arg="validate", value=validate)],
                    starargs=None,
                    kwargs=None,
                )
            ))

        return stmts

    def gen_test_method_body(self):
        var_defs = [
            ast.Assign(targets=[ast.Name(id=var, ctx=ast.Store())],
                       value=self.gen_expr(init))
            for var, init in iteritems(self.scenario.get("variables"))
        ]
        if var_defs:
            var_defs.append(self.gen_empty_line_stmt())

        init = self.gen_init()
        if init:
            init.append(self.gen_empty_line_stmt())

        requests = []
        for req in self.scenario.get_requests():
            if not isinstance(req, HTTPRequest):
                msg = "Apiritif script generator doesn't support '%s' blocks, skipping"
                self.log.warning(msg, req.NAME)
                continue
            requests.extend(self.gen_request_lines(req))
            requests.append(self.gen_empty_line_stmt())

        return var_defs + init + requests

    def build_tree(self):
        mod = self.gen_module()
        mod.lineno = 0
        mod.col_offset = 0
        mod = ast.fix_missing_locations(mod)
        return mod

    def build_source_code(self):
        self.tree = self.build_tree()

    def save(self, filename):
        with open(filename, 'wt') as fds:
            source = astunparse.unparse(self.tree)
            # because astunparse on Python 2 adds empty parens
            source = source.replace('class TestAPIRequests()', 'class TestAPIRequests')
            fds.write(source)


class JMeterFunction(object):
    def __init__(self, arg_names, compiler):
        self.arg_names = arg_names
        self.compiler = compiler

    def to_python(self, arguments):
        """arguments -> (expression, stmts)"""
        args = dict(zip(self.arg_names, arguments))
        return self._compile(args)

    @abstractmethod
    def _compile(self, args):
        pass


class RandomFunction(JMeterFunction):
    def __init__(self, compiler):
        super(RandomFunction, self).__init__(["min", "max", "varname"], compiler)

    def _compile(self, args):
        if args.get("min") is None or args.get("max") is None:
            return None

        # TODO: handle `varname` arg

        return ast.Call(
            func=ast.Attribute(
                value=ast.Name(id="apiritif", ctx=ast.Load()),
                attr='random_uniform',
                ctx=ast.Load(),
            ),
            args=[self.compiler.gen_expr(int(args["min"])), self.compiler.gen_expr(int(args["max"]))],
            keywords=[],
            starargs=None,
            kwargs=None
        )


class RandomStringFunction(JMeterFunction):
    def __init__(self, compiler):
        super(RandomStringFunction, self).__init__(["size", "chars", "varname"], compiler)

    def _compile(self, args):
        if args.get("size") is None:
            return None

        # TODO: handle `varname`

        size = int(args.get("size"))
        arguments = [self.compiler.gen_expr(size)]
        if "chars" in args:
            arguments.append(self.compiler.gen_expr(args["chars"]))

        return ast.Call(
            func=ast.Attribute(
                value=ast.Name(id="apiritif", ctx=ast.Load()),
                attr='random_string',
                ctx=ast.Load(),
            ),
            args=arguments,
            keywords=[],
            starargs=None,
            kwargs=None
        )


class TimeFunction(JMeterFunction):
    def __init__(self, compiler):
        super(TimeFunction, self).__init__(["format", "varname"], compiler)

    def _compile(self, args):
        # TODO: handle varname
        arguments = []
        if "format" in args:
            arguments.append(self.compiler.gen_expr(args["format"]))
        return ast.Call(
            func=ast.Attribute(
                value=ast.Name(id="apiritif", ctx=ast.Load()),
                attr='format_date',
                ctx=ast.Load(),
            ),
            args=arguments,
            keywords=[],
            starargs=None,
            kwargs=None
        )


class JMeterExprCompiler(object):
    def __init__(self, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)

    def gen_expr(self, value):
        if isinstance(value, bool):
            return ast.Name(id="True" if value else "False", ctx=ast.Load())
        elif isinstance(value, (int, float)):
            return ast.Num(n=value)
        elif isinstance(value, string_types):
            # if is has interpolation - break it into either a `"".format(args)` form or a Name node
            # otherwise - it's a string literal
            parts = re.split(r'(\$\{.*?\})', value)
            format_args = []
            for item in parts:
                if item:
                    if item.startswith("${") and item.endswith("}"):
                        value = value.replace(item, "{}")
                        compiled = self.translate_jmeter_expr(item[2:-1])
                        format_args.append(compiled)
            if format_args:
                if len(format_args) == 1 and value == "{}":
                    result = format_args[0]
                else:
                    result = ast.Call(
                        func=ast.Attribute(
                            value=ast.Str(s=value),
                            attr='format',
                            ctx=ast.Load(),
                        ),
                        args=format_args,
                        keywords=[],
                        starargs=None,
                        kwargs=None
                    )
            else:
                result = ast.Str(s=value)
            return result
        elif isinstance(value, type(None)):
            return ast.Name(id="None", ctx=ast.Load())
        elif isinstance(value, dict):
            items = sorted(list(iteritems(value)))
            return ast.Dict(keys=[self.gen_expr(k) for k, _ in items],
                            values=[self.gen_expr(v) for _, v in items])
        elif isinstance(value, list):
            return ast.List(elts=[self.gen_expr(val) for val in value], ctx=ast.Load())
        elif isinstance(value, ast.AST):
            return value
        else:
            return value

    def translate_jmeter_expr(self, expr):
        """
        Translates JMeter expression into Apiritif-based Python expression.
        :type expr: str
        :return:
        """
        self.log.debug("Attempting to translate JMeter expression %r", expr)
        functions = {
            '__time': TimeFunction,
            '__Random': RandomFunction,
            '__RandomString': RandomStringFunction,
        }
        regexp = r"(\w+)\((.*?)\)"
        args_re = r'(?<!\\),'
        match = re.match(regexp, expr)
        if match is None:  # doesn't look like JMeter expression, do not translate
            return ast.Name(id=expr, ctx=ast.Load())

        varname, arguments = match.groups()

        if arguments is None:  # plain variable
            result = ast.Name(id=varname, ctx=ast.Load())
        else:  # function call
            if not arguments:
                args = []
            else:
                # parse arguments: split by ',' but not '\,'
                args = [arg.strip() for arg in re.split(args_re, arguments)]

            if varname not in functions:  # unknown function
                return ast.Name(id=varname, ctx=ast.Load())

            self.log.debug("Translating function %s with arguments %s", varname, arguments)
            func = functions[varname](self)
            result = func.to_python(args)
            if result is None:
                result = ast.Name(id=varname, ctx=ast.Load())
        self.log.debug("Compile: %r -> %r", expr, result)
        return result


class PyTestExecutor(SubprocessedExecutor, HavingInstallableTools):
    def __init__(self):
        super(PyTestExecutor, self).__init__()
        self.runner_path = os.path.join(get_full_path(__file__, step_up=2), "resources", "pytest_runner.py")
        self._tailer = FileReader('', file_opener=lambda _: None, parent_logger=self.log)
        self._additional_args = []

    def prepare(self):
        self.install_required_tools()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("'script' should be present for pytest executor")

        scenario = self.get_scenario()
        if "additional-args" in scenario:
            argv = scenario.get("additional-args")
            self._additional_args = shlex.split(argv)

        self.reporting_setup(suffix=".ldjson")

    def __is_verbose(self):
        engine_verbose = self.engine.config.get(SETTINGS).get("verbose", False)
        executor_verbose = self.settings.get("verbose", engine_verbose)
        return executor_verbose

    def install_required_tools(self):
        """
        we need installed nose plugin
        """
        if sys.version >= '3':
            self.log.warning("You are using Python 3, make sure that your scripts are able to run in Python 3")

        self._check_tools([TaurusPytestRunner(self.runner_path, "")])

    def startup(self):
        """
        run python tests
        """
        executable = self.settings.get("interpreter", sys.executable)

        self.env.add_path({"PYTHONPATH": get_full_path(__file__, step_up=3)})

        cmdline = [executable, self.runner_path, '--report-file', self.report_file]
        cmdline += self._additional_args

        load = self.get_load()
        if load.iterations:
            cmdline += ['-i', str(load.iterations)]

        if load.hold:
            cmdline += ['-d', str(load.hold)]

        cmdline += [self.script]
        self._start_subprocess(cmdline)

        if self.__is_verbose():
            self._tailer = FileReader(filename=self.stdout_file, parent_logger=self.log)

    def check(self):
        self.__log_lines()
        return super(PyTestExecutor, self).check()

    def post_process(self):
        super(PyTestExecutor, self).post_process()
        self.__log_lines()

    def __log_lines(self):
        lines = []
        for line in self._tailer.get_lines():
            if not IGNORED_LINE.match(line):
                lines.append(line)

        if lines:
            self.log.info("\n".join(lines))


class TaurusPytestRunner(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusPytestRunner, self).__init__("TaurusPytestRunner", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus pytest runner isn't implemented")


class RobotExecutor(SubprocessedExecutor, HavingInstallableTools):
    def __init__(self):
        super(RobotExecutor, self).__init__()
        self.runner_path = os.path.join(get_full_path(__file__, step_up=2), "resources", "robot_runner.py")
        self.variables_file = None

    def resource_files(self):
        files = super(RobotExecutor, self).resource_files()
        scenario = self.get_scenario()
        if "variables" in scenario and isinstance(scenario["variables"], (string_types, text_type)):
            files.append(scenario["variables"])
        return files

    def prepare(self):
        self.install_required_tools()
        self.script = self.get_script_path()
        if not self.script:
            raise TaurusConfigError("'script' should be present for robot executor")

        self.reporting_setup(suffix=".ldjson")

        scenario = self.get_scenario()
        variables = scenario.get("variables")
        if variables:
            if isinstance(variables, (string_types, text_type)):
                self.variables_file = get_full_path(variables)
            elif isinstance(variables, dict):
                self.variables_file = self.engine.create_artifact("robot-vars", ".yaml")
                with open(self.variables_file, 'wb') as fds:
                    yml = yaml.dump(variables,
                                    default_flow_style=False, explicit_start=True, canonical=False, allow_unicode=True,
                                    encoding='utf-8', width=float("inf"))
                    fds.write(yml)
            else:
                raise TaurusConfigError("`variables` is neither file nor dict")

    def install_required_tools(self):
        self._check_tools([Robot(self.settings.get("interpreter", sys.executable), self.log),
                           TaurusRobotRunner(self.runner_path, "")])

    def startup(self):
        executable = self.settings.get("interpreter", sys.executable)

        self.env.add_path({"PYTHONPATH":  get_full_path(__file__, step_up=3)})

        cmdline = [executable, self.runner_path, '--report-file', self.report_file]

        load = self.get_load()
        if load.iterations:
            cmdline += ['--iterations', str(load.iterations)]

        if load.hold:
            cmdline += ['--duration', str(load.hold)]

        if self.variables_file is not None:
            cmdline += ['--variablefile', self.variables_file]

        cmdline += [self.script]
        self._start_subprocess(cmdline)


class TaurusRobotRunner(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusRobotRunner, self).__init__("TaurusRobotRunner", tool_path, download_link)

    def install(self):
        raise ToolError("Robot Taurus runner should've been included in Taurus distribution")


class Robot(RequiredTool):
    def __init__(self, python_executable, parent_logger):
        super(Robot, self).__init__("RobotFramework", "")
        self.python_executable = python_executable
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug('Checking Robot Framework: %s' % self.tool_path)
        try:
            checker = shell_exec([self.python_executable, '-c', 'import robot; print(robot.__version__)'])
            output = checker.communicate()
            self.log.debug("Robot output: %s", output)
            if checker.returncode != 0:
                return False
        except (CalledProcessError, OSError):
            return False
        return True

    def install(self):
        raise ToolError("You must install robot framework")
