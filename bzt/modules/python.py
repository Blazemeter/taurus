import re
import sys

import os
from bzt import ToolError, TaurusConfigError, TaurusInternalException

from bzt.engine import SubprocessedExecutor, HavingInstallableTools
from bzt.six import parse, string_types, iteritems
from bzt.utils import get_full_path, TclLibrary, RequiredTool, PythonGenerator, dehumanize_time, BetterDict


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
        wdlog = self.engine.create_artifact('webdriver', '.log')
        nose_test = SeleniumScriptBuilder(self.get_scenario(), self.log, wdlog)
        self.generated_methods.merge(nose_test.build_source_code())
        nose_test.save(filename)
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
        nose_command_line = [executable, self.plugin_path, '--report-file', self.execution.get("report-file")]

        load = self.get_load()
        if load.iterations:
            nose_command_line += ['-i', str(load.iterations)]

        if load.hold:
            nose_command_line += ['-d', str(load.hold)]

        nose_command_line += [self._script]
        self._start_subprocess(nose_command_line)


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
