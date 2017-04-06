import os
import re
import sys
from collections import OrderedDict

from bzt import ToolError, TaurusConfigError, TaurusInternalException
from bzt.engine import SubprocessedExecutor, HavingInstallableTools, Scenario
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.functional import FunctionalAggregator
from bzt.modules.selenium import LoadSamplesReader, FuncSamplesReader
from bzt.requests_model import HTTPRequest
from bzt.six import parse, string_types, iteritems
from bzt.utils import get_full_path, TclLibrary, RequiredTool, PythonGenerator, dehumanize_time, BetterDict, \
    ensure_is_dict


class NoseTester(SubprocessedExecutor, HavingInstallableTools):
    """
    Python selenium tests runner
    """

    def __init__(self):
        super(NoseTester, self).__init__()
        self.plugin_path = os.path.join(get_full_path(__file__, step_up=2),
                                        "resources",
                                        "nose_plugin.py")
        self._script = None
        self.generated_methods = BetterDict()

    def prepare(self):
        super(NoseTester, self).prepare()
        self.install_required_tools()
        self._script = self.get_script_path()
        if not self._script:
            if "requests" in self.get_scenario():
                self._script = self.__tests_from_requests()
            else:
                raise TaurusConfigError("Nothing to test, no requests were provided in scenario")

    def __tests_from_requests(self):
        filename = self.engine.create_artifact("test_requests", ".py")
        test_mode = self.execution.get("test-mode", None) or "selenium"
        if test_mode == "apiritif":
            builder = ApiritifScriptBuilder(self.get_scenario(), self.log)
        else:
            wdlog = self.engine.create_artifact('webdriver', '.log')
            builder = SeleniumScriptBuilder(self.get_scenario(), self.log, wdlog)
        self.generated_methods.merge(builder.build_source_code())
        builder.save(filename)
        return filename

    def install_required_tools(self):
        """
        we need installed nose plugin
        """
        if sys.version >= '3':
            self.log.warning("You are using python3, make sure that your scripts are able to run in python3!")

        self._check_tools([TclLibrary(self.log), TaurusNosePlugin(self.plugin_path, "")])

    def startup(self):
        """
        run python tests
        """
        executable = self.settings.get("interpreter", sys.executable)

        if "report-file" in self.execution:
            report_file = self.execution.get("report-file")
        else:
            report_file = self.engine.create_artifact("report", ".ldjson")
            if self.engine.is_functional_mode():
                self.reader = FuncSamplesReader(report_file, self.engine, self.log, [])
                if isinstance(self.engine.aggregator, FunctionalAggregator):
                    self.engine.aggregator.add_underling(self.reader)
            else:
                self.reader = LoadSamplesReader(report_file, self.log, [])
                if isinstance(self.engine.aggregator, ConsolidatingAggregator):
                    self.engine.aggregator.add_underling(self.reader)

        nose_command_line = [executable, self.plugin_path, '--report-file', report_file]

        load = self.get_load()
        if load.iterations:
            nose_command_line += ['-i', str(load.iterations)]

        if load.hold:
            nose_command_line += ['-d', str(load.hold)]

        nose_command_line += [self._script]
        self._start_subprocess(nose_command_line)

    def has_results(self):
        if isinstance(self.reader, LoadSamplesReader):
            return bool(self.reader) and bool(self.reader.buffer)
        elif isinstance(self.reader, FuncSamplesReader):
            return bool(self.reader) and bool(self.reader.read_records)


class TaurusNosePlugin(RequiredTool):
    def __init__(self, tool_path, download_link):
        super(TaurusNosePlugin, self).__init__("TaurusNosePlugin", tool_path, download_link)

    def install(self):
        raise ToolError("Automatic installation of Taurus nose plugin isn't implemented")


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
        test_class.append(self.gen_statement("driver = None", indent=4))
        test_class.append(self.gen_new_line())
        test_class.append(self.gen_setupclass_method())
        test_class.append(self.gen_teardownclass_method())
        test_class.append(self.gen_setup_method())

        counter = 0
        methods = {}
        requests = self.scenario.get_requests(require_url=False)
        default_address = self.scenario.get("default-address", None)

        for req in requests:
            if req.label:
                label = req.label
            elif req.url:
                label = req.url
            else:
                raise TaurusConfigError("You must specify at least 'url' or 'label' for each requests item")
            mod_label = re.sub('[^0-9a-zA-Z]+', '_', label[:30])
            method_name = 'test_%05d_%s' % (counter, mod_label)
            test_method = self.gen_test_method(method_name)
            methods[method_name] = label
            counter += 1
            test_class.append(test_method)

            if req.url is not None:
                self._add_url_request(default_address, req, test_method)

            for action_config in req.config.get("actions", []):
                test_method.append(self.gen_action(action_config))

            if "assert" in req.config:
                test_method.append(self.gen_statement("body = self.driver.page_source"))
                for assert_config in req.config.get("assert"):
                    for elm in self.gen_assertion(assert_config):
                        test_method.append(elm)

            think_time = req.priority_option('think-time')
            if think_time is not None:
                test_method.append(self.gen_statement("sleep(%s)" % dehumanize_time(think_time)))

            test_method.append(self.gen_statement("pass"))  # just to stub empty case
            test_method.append(self.gen_new_line())

        return methods

    def _add_url_request(self, default_address, req, test_method):
        parsed_url = parse.urlparse(req.url)
        if default_address is not None and not parsed_url.netloc:
            url = default_address + req.url
        else:
            url = req.url
        if req.timeout is not None:
            test_method.append(self.gen_impl_wait(req.timeout))
        test_method.append(self.gen_statement("self.driver.get('%s')" % url))

    def gen_setup_method(self):
        timeout = self.scenario.get("timeout", None)
        if timeout is None:
            timeout = '30s'
        scenario_timeout = dehumanize_time(timeout)
        setup_method_def = self.gen_method_definition('setUp', ['self'])
        setup_method_def.append(self.gen_impl_wait(scenario_timeout))
        setup_method_def.append(self.gen_new_line())
        return setup_method_def

    def gen_setupclass_method(self):
        self.log.debug("Generating setUp test method")
        browsers = ["Firefox", "Chrome", "Ie", "Opera"]
        browser = self.scenario.get("browser", "Firefox")
        if browser not in browsers:
            raise TaurusConfigError("Unsupported browser name: %s" % browser)

        setup_method_def = self.gen_decorator_statement('classmethod')
        setup_method_def.append(self.gen_method_definition("setUpClass", ["cls"]))

        if browser == 'Firefox':
            setup_method_def.append(self.gen_statement("profile = webdriver.FirefoxProfile()"))
            statement = "profile.set_preference('webdriver.log.file', %s)" % repr(self.wdlog)
            log_set = self.gen_statement(statement)
            setup_method_def.append(log_set)
            setup_method_def.append(self.gen_statement("cls.driver = webdriver.Firefox(profile)"))
        elif browser == 'Chrome':
            statement = "cls.driver = webdriver.Chrome(service_log_path=%s)"
            setup_method_def.append(self.gen_statement(statement % repr(self.wdlog)))
        else:
            setup_method_def.append(self.gen_statement("cls.driver = webdriver.%s()" % browser))

        scenario_timeout = self.scenario.get("timeout", None)
        if scenario_timeout is None:
            scenario_timeout = '30s'
        setup_method_def.append(self.gen_impl_wait(scenario_timeout, target='cls'))
        if self.window_size:
            args = (self.window_size[0], self.window_size[1])  # to force tuple
            statement = self.gen_statement("cls.driver.set_window_size(%s, %s)" % args)
            setup_method_def.append(statement)
        else:
            setup_method_def.append(self.gen_statement("cls.driver.maximize_window()"))
        setup_method_def.append(self.gen_new_line())
        return setup_method_def

    def gen_impl_wait(self, timeout, target='self'):
        return self.gen_statement("%s.driver.implicitly_wait(%s)" % (target, dehumanize_time(timeout)))

    def gen_test_method(self, name):
        self.log.debug("Generating test method %s", name)
        test_method = self.gen_method_definition(name, ["self"])
        return test_method

    def gen_teardownclass_method(self):
        self.log.debug("Generating tearDown test method")
        tear_down_method_def = self.gen_decorator_statement('classmethod')
        tear_down_method_def.append(self.gen_method_definition("tearDownClass", ["cls"]))
        tear_down_method_def.append(self.gen_statement("cls.driver.quit()"))
        tear_down_method_def.append(self.gen_new_line())
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

    def gen_action(self, action_config):
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

            return self.gen_statement(tpl % (bys[aby], selector, action))
        elif atype == 'wait':
            tpl = "WebDriverWait(self.driver, %s).until(econd.%s_of_element_located((By.%s, %r)), %r)"
            mode = "visibility" if param == 'visible' else 'presence'
            exc = TaurusConfigError("wait action requires timeout in scenario: \n%s" % self.scenario)
            timeout = dehumanize_time(self.scenario.get("timeout", exc))
            errmsg = "Element %r failed to appear within %ss" % (selector, timeout)
            return self.gen_statement(tpl % (timeout, mode, bys[aby], selector, errmsg))

        raise TaurusInternalException("Could not build code for action: %s" % action_config)

    def _parse_action(self, action_config):
        if isinstance(action_config, string_types):
            name = action_config
            param = None
        elif isinstance(action_config, dict):
            name, param = next(iteritems(action_config))
        else:
            raise TaurusConfigError("Unsupported value for action: %s" % action_config)

        expr = re.compile("^(click|wait|keys)(byName|byID|byCSS|byXPath|byLinkText)\((.+)\)$", re.IGNORECASE)
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


class ApiritifScriptBuilder(PythonGenerator):
    IMPORTS = """\
import time
import unittest

import apiritif

"""

    def __init__(self, scenario, parent_logger):
        super(ApiritifScriptBuilder, self).__init__(scenario, parent_logger)

    def gen_setup_method(self):
        keepalive = self.scenario.get("keepalive", None)
        if keepalive is None:
            keepalive = True

        exc = TaurusConfigError("Apiritif required 'default-address' to be set")
        default_address = self.scenario.get("default-address", exc)
        base_path = self.scenario.get("base-path", None)
        auto_assert_ok = self.scenario.get("auto-assert-ok", True)

        setup_method_def = self.gen_decorator_statement('classmethod')
        setup_method_def.append(self.gen_method_definition("setUpClass", ["cls"]))
        target_line = "cls.target = apiritif.http.target(%r)" % default_address
        setup_method_def.append(self.gen_statement(target_line, indent=8))

        if base_path:
            setup_method_def.append(self.gen_statement("cls.target.base_path(%r)" % base_path, indent=8))
        setup_method_def.append(self.gen_statement("cls.target.keep_alive(%r)" % keepalive, indent=8))
        setup_method_def.append(self.gen_statement("cls.target.auto_assert_ok(%r)" % auto_assert_ok, indent=8))
        setup_method_def.append(self.gen_new_line(indent=0))
        return setup_method_def

    def build_source_code(self):
        methods = {}
        self.log.debug("Generating Test Case test methods")
        imports = self.add_imports()
        self.root.append(imports)
        test_class = self.gen_class_definition("TestRequests", ["unittest.TestCase"])
        self.root.append(test_class)
        test_class.append(self.gen_setup_method())

        for index, req in enumerate(self.scenario.get_requests()):
            if not isinstance(req, HTTPRequest):
                msg = "Apiritif script generator doesn't support '%s' blocks, skipping"
                self.log.warning(msg, req.NAME)
                continue

            if req.label:
                label = req.label
            elif req.url:
                label = req.url
            else:
                raise TaurusConfigError("You must specify at least 'url' or 'label' for each requests item")

            mod_label = re.sub('[^0-9a-zA-Z]+', '_', req.url[:30])
            method_name = 'test_%05d_%s' % (index, mod_label)
            test_method = self.gen_test_method(method_name)
            methods[method_name] = label

            self._add_url_request(req, test_method)

            test_class.append(test_method)
            test_method.append(self.gen_new_line(indent=0))

        return methods

    def _add_url_request(self, request, test_method):
        """
        :type request: bzt.requests_model.HTTPRequest
        """
        named_args = OrderedDict()

        method = request.method.lower()
        think_time = dehumanize_time(request.priority_option('think-time', default=None))

        named_args['timeout'] = dehumanize_time(request.priority_option('timeout', default='30s'))
        named_args['allow_redirects'] = request.priority_option('follow-redirects', default=True)

        headers = {}
        scenario_headers = self.scenario.get("headers", None)
        if scenario_headers:
            headers.update(scenario_headers)
        if request.headers:
            headers.update(request.headers)
        if headers:
            named_args['headers'] = headers

        merged_headers = dict([(key.lower(), value) for key, value in iteritems(headers)])
        content_type = merged_headers.get('content-type', None)
        if content_type == 'application/json' and isinstance(request.body, (dict, list)):  # json request body
            named_args['json'] = {key: value for key, value in iteritems(request.body)}
        elif method == "get" and isinstance(request.body, dict):  # request URL params (?a=b&c=d)
            named_args['params'] = {key: value for key, value in iteritems(request.body)}
        elif isinstance(request.body, dict):  # form data
            named_args['data'] = list(iteritems(request.body))
        elif isinstance(request.body, string_types):
            named_args['data'] = request.body
        elif request.body:
            msg = "Cannot handle 'body' option of type %s: %s"
            raise TaurusConfigError(msg % (type(request.body), request.body))

        kwargs = ", ".join("%s=%r" % (name, value) for name, value in iteritems(named_args))

        request_line = "response = self.target.{method}({url}, {kwargs})".format(
            method=method,
            url=repr(request.url),
            kwargs=kwargs,
        )
        test_method.append(self.gen_statement(request_line))
        self._add_assertions(request, test_method)
        self._add_jsonpath_assertions(request, test_method)
        self._add_xpath_assertions(request, test_method)
        if think_time:
            test_method.append(self.gen_statement('time.sleep(%s)' % think_time))

    def _add_assertions(self, request, test_method):
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
                    line = "response.{method}({member})".format(method=method, member=repr(member))
                    test_method.append(self.gen_statement(line))
            elif subject == Scenario.FIELD_RESP_CODE:
                for member in assertion["contains"]:
                    method = "assert_status_code" if not assertion.get('not', False) else "assert_not_status_code"
                    line = "response.{method}({member})".format(method=method, member=repr(member))
                    test_method.append(self.gen_statement(line))

    def _add_jsonpath_assertions(self, request, test_method):
        jpath_assertions = request.config.get("assert-jsonpath", [])
        for idx, assertion in enumerate(jpath_assertions):
            assertion = ensure_is_dict(jpath_assertions, idx, "jsonpath")
            exc = TaurusConfigError('JSON Path not found in assertion: %s' % assertion)
            query = assertion.get('jsonpath', exc)
            expected = assertion.get('expected-value', '') or None
            method = "assert_not_jsonpath" if assertion.get('invert', False) else "assert_jsonpath"
            line = "response.{method}({query}, expected_value={expected})".format(
                method=method,
                query=repr(query),
                expected=repr(expected) if expected else None
            )
            test_method.append(self.gen_statement(line))

    def _add_xpath_assertions(self, request, test_method):
        jpath_assertions = request.config.get("assert-xpath", [])
        for idx, assertion in enumerate(jpath_assertions):
            assertion = ensure_is_dict(jpath_assertions, idx, "xpath")
            exc = TaurusConfigError('XPath not found in assertion: %s' % assertion)
            query = assertion.get('xpath', exc)
            parser_type = 'html' if assertion.get('use-tolerant-parser', True) else 'xml'
            validate = assertion.get('validate-xml', False)
            method = "assert_not_xpath" if assertion.get('invert', False) else "assert_xpath"
            line = "response.{method}({query}, parser_type={parser_type}, validate={validate})".format(
                method=method,
                query=repr(query),
                validate=repr(validate),
                parser_type=repr(parser_type),
            )
            test_method.append(self.gen_statement(line))

    def gen_test_method(self, name):
        self.log.debug("Generating test method %s", name)
        test_method = self.gen_method_definition(name, ["self"])
        return test_method
