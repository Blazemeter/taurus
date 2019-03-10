"""
Copyright 2018 BlazeMeter Inc.

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
import re

from bzt import TaurusConfigError, TaurusInternalException
from bzt.six import parse, string_types, iteritems, text_type, etree
from bzt.utils import dehumanize_time
from .python import PythonGenerator


class SeleniumScriptBuilder(PythonGenerator):
    """
    :type window_size: tuple[int,int]
    """

    IMPORTS_SELENIUM = """import unittest
import os
import re
from time import sleep, time
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys

import apiritif
"""
    IMPORTS_APPIUM = """import unittest
import os
import re
from time import sleep, time
from appium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys

import apiritif
"""

    TAGS = ("byName", "byID", "byCSS", "byXPath", "byLinkText")

    def __init__(self, scenario, parent_logger, wdlog, utils_file, ignore_unknown_actions=False, generate_markers=None):
        super(SeleniumScriptBuilder, self).__init__(scenario, parent_logger)
        self.label = ''
        self.webdriver_address = None
        self.capabilities_from_outside = {}
        self.window_size = None
        self.wdlog = wdlog
        self.appium = False
        self.utils_file = utils_file
        self.ignore_unknown_actions = ignore_unknown_actions
        self.generate_markers = generate_markers

    def gen_asserts(self, config, indent=None):
        test_method = []
        if "assert" in config:
            test_method.append(self.gen_statement("body = self.driver.page_source", indent=indent))
            for assert_config in config.get("assert"):
                for elm in self.gen_assertion(assert_config, indent=indent):
                    test_method.append(elm)
        return test_method

    def gen_think_time(self, think_time, indent=None):
        test_method = []
        if think_time is not None:
            delay = dehumanize_time(think_time)
            if delay > 0:
                test_method.append(self.gen_statement("sleep(%s)" % dehumanize_time(think_time), indent=indent))
                test_method.append(self.gen_new_line())
        return test_method

    def gen_request(self, req, indent=None):
        default_address = self.scenario.get("default-address")
        transaction_contents = []
        if req.url is not None:
            parsed_url = parse.urlparse(req.url)
            if default_address and not parsed_url.netloc:
                url = default_address + req.url
            else:
                url = req.url
            transaction_contents.append(
                self.gen_statement("self.driver.get(self.template(%r))" % url, indent=indent))
            transaction_contents.append(self.gen_new_line())
        return transaction_contents

    def build_source_code(self):
        self.log.debug("Generating Test Case test methods")

        test_class = self.gen_class_definition("TestRequests", ["unittest.TestCase"])
        test_class.append(self.gen_setup_method())
        test_class.append(self.gen_teardown_method())

        requests = self.scenario.get_requests(require_url=False)
        test_method = self.gen_test_method('test_requests')
        self.gen_setup(test_method)

        for i, req in enumerate(requests, 1):
            self._fill_test_method(req, test_method)
            if i != len(requests):
                test_method.append(self.gen_new_line())

        test_class.append(test_method)

        self.root.append(self.gen_statement("# coding=utf-8", indent=0))
        self.root.append(self.add_imports())
        self.root.append(test_class)
        self.root.append(self.add_utilities())

    def _fill_test_method(self, req, test_method):
        if req.label:
            label = req.label
        elif req.url:
            label = req.url
        else:
            raise TaurusConfigError("You must specify at least 'url' or 'label' for each requests item")

        if self.generate_markers:
            test_method.append(self.gen_statement("try:", indent=self.INDENT_STEP * 2))
            indent = 3
            marker = "self.driver.execute_script('/* FLOW_MARKER test-case-start */', " \
                     "{'testCaseName': %r, 'testSuiteName': %r})" % (label, self.label)
            test_method.append(self.gen_statement(marker, indent=self.INDENT_STEP * indent))
            test_method.append(self.gen_new_line())
        else:
            indent = 2

        test_method.append(self.gen_statement('with apiritif.transaction_logged(self.template(%r)):' % label,
                                              indent=self.INDENT_STEP * indent))
        transaction_contents = []

        transaction_contents.extend(self.gen_request(req, indent=self.INDENT_STEP * (indent + 1)))
        if req.url is not None and req.timeout is not None:
            test_method.append(self.gen_impl_wait(req.timeout, indent=self.INDENT_STEP * (indent + 1)))

        action_append = False
        for action_config in req.config.get("actions", []):
            action = self.gen_action(action_config, indent=self.INDENT_STEP * (indent + 1))
            if action:
                transaction_contents.extend(action)
                action_append = True
        if action_append:
            transaction_contents.append(self.gen_new_line())

        transaction_contents.extend(self.gen_asserts(req.config, indent=self.INDENT_STEP * (indent + 1)))

        if transaction_contents:
            test_method.extend(transaction_contents)
        else:
            test_method.append(self.gen_statement('pass', indent=self.INDENT_STEP * (indent + 1)))
        test_method.append(self.gen_new_line())

        test_method.extend(self.gen_think_time(req.priority_option('think-time'), indent=self.INDENT_STEP * indent))

        if self.generate_markers:
            marker = "self.driver.execute_script('/* FLOW_MARKER test-case-stop */', " \
                     "{'status': %s, 'message': %s})"

            test_method.append(self.gen_statement("except AssertionError as exc:", indent=self.INDENT_STEP * 2))
            test_method.append(self.gen_statement(marker % (repr('failed'), 'str(exc)'), indent=self.INDENT_STEP * 3))
            test_method.append(self.gen_statement("raise", indent=self.INDENT_STEP * 3))

            test_method.append(self.gen_statement("except BaseException as exc:", indent=self.INDENT_STEP * 2))
            test_method.append(self.gen_statement(marker % (repr('broken'), 'str(exc)'), indent=self.INDENT_STEP * 3))
            test_method.append(self.gen_statement("raise", indent=self.INDENT_STEP * 3))

            test_method.append(self.gen_statement("else:", indent=self.INDENT_STEP * 2))
            test_method.append(self.gen_statement(marker % (repr('success'), repr('')), indent=self.INDENT_STEP * 3))

    def add_imports(self):
        imports = super(SeleniumScriptBuilder, self).add_imports()
        if self.appium:
            imports.text = self.IMPORTS_APPIUM
        else:
            imports.text = self.IMPORTS_SELENIUM
        return imports

    def add_utilities(self):
        with open(self.utils_file) as fds:
            utilities_source_lines = fds.read()
        utils = etree.Element("utilities")
        utils.text = "\n" + utilities_source_lines
        return utils

    def gen_global_vars(self):
        variables = self.scenario.get("variables")
        stmts = [
            "self.vars = {}",
            "self.template = Template(self.vars)"
        ]

        for key in sorted(variables.keys()):
            stmts.append("self.vars['%s'] = %r" % (key, variables[key]))
        stmts.append("")
        return [self.gen_statement(stmt) for stmt in stmts]

    def _add_url_request(self, default_address, req, test_method):
        parsed_url = parse.urlparse(req.url)
        if default_address is not None and not parsed_url.netloc:
            url = default_address + req.url
        else:
            url = req.url
        if req.timeout is not None:
            test_method.append(self.gen_impl_wait(req.timeout))
        test_method.append(self.gen_statement("self.driver.get(self.template(%r))" % url))

    def gen_setup(self, test_method):
        timeout = self.scenario.get("timeout", "30s")
        scenario_timeout = dehumanize_time(timeout)
        test_method.append(self.gen_impl_wait(scenario_timeout))
        test_method.append(self.gen_new_line())

    def _check_platform(self):
        inherited_capabilities = [{x: y} for x, y in iteritems(self.capabilities_from_outside)]
        mobile_browsers = ["Chrome", "Safari"]
        mobile_platforms = ["Android", "iOS"]
        remote_executor = self.scenario.get("remote", self.webdriver_address)

        browser = self.scenario.get("browser", None)

        browser_platform = None
        if browser:
            browser_split = browser.split("-")
            browser = browser_split[0]
            browsers = ["Firefox", "Chrome", "Ie", "Opera", "Remote"]
            if browser not in browsers:
                raise TaurusConfigError("Unsupported browser name: %s" % browser)
            if len(browser_split) > 1:
                browser_platform = browser_split[1]

        if remote_executor:
            if browser:
                self.log.warning("Forcing browser to Remote, because of remote webdriver address")
            inherited_capabilities.append({"browser": browser})
            browser = "Remote"
            if self.generate_markers is None:  # if not set by user - set to true
                self.generate_markers = True
        elif browser in mobile_browsers and browser_platform in mobile_platforms:
            self.appium = True
            inherited_capabilities.append({"platform": browser_platform})
            inherited_capabilities.append({"browser": browser})
            browser = "Remote"  # Force to use remote web driver
        elif not browser:
            browser = "Firefox"

        return browser, inherited_capabilities, remote_executor

    def gen_setup_method(self):
        self.log.debug("Generating setUp test method")
        browser, inherited_capabilities, remote_executor = self._check_platform()

        headless = self.scenario.get("headless", False)
        if headless:
            self.log.info("Headless mode works only with Selenium 3.8.0+, be sure to have it installed")

        setup_method_def = self.gen_method_definition("setUp", ["self"])
        setup_method_def.extend(self.gen_global_vars())

        if browser == 'Firefox':
            setup_method_def.append(self.gen_statement("options = webdriver.FirefoxOptions()"))
            if headless:
                setup_method_def.append(self.gen_statement("options.set_headless()"))
            setup_method_def.append(self.gen_statement("profile = webdriver.FirefoxProfile()"))
            statement = "profile.set_preference('webdriver.log.file', %s)" % repr(self.wdlog)
            log_set = self.gen_statement(statement)
            setup_method_def.append(log_set)
            tmpl = "self.driver = webdriver.Firefox(profile, firefox_options=options)"
            setup_method_def.append(self.gen_statement(tmpl))
        elif browser == 'Chrome':
            setup_method_def.append(self.gen_statement("options = webdriver.ChromeOptions()"))
            if headless:
                setup_method_def.append(self.gen_statement("options.set_headless()"))
            statement = "self.driver = webdriver.Chrome(service_log_path=%s, chrome_options=options)"
            setup_method_def.append(self.gen_statement(statement % repr(self.wdlog)))
        elif browser == 'Remote':
            setup_method_def.append(self._gen_remote_driver(inherited_capabilities, remote_executor))
        else:
            if headless:
                self.log.warning("Browser %r doesn't support headless mode")
            setup_method_def.append(self.gen_statement("self.driver = webdriver.%s()" % browser))

        scenario_timeout = self.scenario.get("timeout", "30s")
        setup_method_def.append(self.gen_impl_wait(scenario_timeout))

        setup_method_def.append(self.gen_statement("self.wnd_mng = WindowManager(self.driver)"))
        setup_method_def.append(self.gen_statement("self.frm_mng = FrameManager(self.driver)"))

        if self.window_size:  # FIXME: unused in fact
            statement = self.gen_statement("self.driver.set_window_position(0, 0)")
            setup_method_def.append(statement)

            args = (self.window_size[0], self.window_size[1])
            statement = self.gen_statement("self.driver.set_window_size(%s, %s)" % args)
            setup_method_def.append(statement)
        else:
            pass  # TODO: setup_method_def.append(self.gen_statement("self.driver.maximize_window()"))
            # but maximize_window does not work on virtual displays. Bummer

        setup_method_def.append(self.gen_new_line())
        return setup_method_def

    def _gen_remote_driver(self, inherited_caps, remote_executor):
        desired_caps = {}
        remote_caps = self.scenario.get("capabilities", [])
        if not isinstance(remote_caps, list):
            remote_caps = [remote_caps]
        capabilities = remote_caps + inherited_caps

        for capability in capabilities:
            for cap_key in capability.keys():
                if cap_key == "browser":
                    desired_caps["browserName"] = capability[cap_key]
                elif cap_key == "version":
                    desired_caps["version"] = str(capability[cap_key])
                elif cap_key == "selenium":
                    desired_caps["seleniumVersion"] = str(capability[cap_key])
                elif cap_key == "javascript":
                    desired_caps["javascriptEnabled"] = capability[cap_key]
                elif cap_key == "platform":
                    desired_caps["platformName"] = str(capability[cap_key])
                elif cap_key == "os_version":
                    desired_caps["platformVersion"] = str(capability[cap_key])
                elif cap_key == "device":
                    desired_caps["deviceName"] = str(capability[cap_key])
                else:
                    desired_caps[cap_key] = capability[cap_key]

        tpl = "self.driver = webdriver.Remote(command_executor={command_executor}, desired_capabilities={des_caps})"

        if not remote_executor:
            if self.appium:
                remote_executor = "http://localhost:4723/wd/hub"
            else:
                remote_executor = "http://localhost:4444/wd/hub"

        cmd = tpl.format(command_executor=repr(remote_executor), des_caps=json.dumps(desired_caps, sort_keys=True))

        return self.gen_statement(cmd)

    def gen_impl_wait(self, timeout, indent=None):
        return self.gen_statement("self.driver.implicitly_wait(%s)" % dehumanize_time(timeout), indent=indent)

    def gen_test_method(self, name):
        self.log.debug("Generating test method %s", name)
        test_method = self.gen_method_definition(name, ["self"])
        return test_method

    def gen_teardown_method(self):
        self.log.debug("Generating tearDown test method")
        tear_down_method_def = self.gen_method_definition("tearDown", ["self"])
        tear_down_method_def.append(self.gen_statement("self.driver.quit()"))
        tear_down_method_def.append(self.gen_new_line())
        return tear_down_method_def

    def gen_assertion(self, assertion_config, indent=None):
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
                assertion_elements.append(self.gen_statement("re_pattern = re.compile(r'%s')" % val, indent=indent))

                method = '%s(0, len(re.findall(re_pattern, body)), "Assertion: %s")'
                method %= assert_method, assert_message
                assertion_elements.append(self.gen_statement(method, indent=indent))
            else:
                assert_method = "self.assertNotIn" if reverse else "self.assertIn"
                method = '%s("%s", body, "Assertion: %s")'
                method %= assert_method, val, assert_message
                assertion_elements.append(self.gen_statement(method, indent=indent))
        return assertion_elements

    def gen_action(self, action_config, indent=None):
        action = self._parse_action(action_config)
        if action:
            atype, tag, param, selector = action
        else:
            return

        action_elements = []

        bys = {
            'byxpath': "XPATH",
            'bycss': "CSS_SELECTOR",
            'byname': "NAME",
            'byid': "ID",
            'bylinktext': "LINK_TEXT"
        }
        action_chains = {
            'doubleclick': "double_click",
            'mousedown': "click_and_hold",
            'mouseup': "release",
            'mousemove': "move_to_element"
        }

        if tag == "window":
            if atype == "switch":
                cmd = 'self.wnd_mng.switch(self.template(%r))' % selector
                action_elements.append(self.gen_statement(cmd, indent=indent))
            elif atype == "open":
                script = "window.open('%s');" % selector
                cmd = 'self.driver.execute_script(self.template(%r))' % script
                action_elements.append(self.gen_statement(cmd, indent=indent))
            elif atype == "close":
                if selector:
                    cmd = 'self.wnd_mng.close(self.template(%r))' % selector
                else:
                    cmd = 'self.wnd_mng.close()'
                action_elements.append(self.gen_statement(cmd, indent=indent))

        elif atype == "switchframe":
            if tag == "byidx":
                cmd = "self.frm_mng.switch(%r)" % int(selector)
            elif selector.startswith("index=") or selector in ["relative=top", "relative=parent"]:
                cmd = "self.frm_mng.switch(%r)" % selector
            else:
                frame = "self.driver.find_element(By.%s, self.template(%r))" % (bys[tag], selector)
                cmd = "self.frm_mng.switch(%s)" % frame

            action_elements.append(self.gen_statement(cmd, indent=indent))

        elif atype in action_chains:
            tpl = "self.driver.find_element(By.%s, self.template(%r))"
            action = action_chains[atype]
            action_elements.append(self.gen_statement(
                "ActionChains(self.driver).%s(%s).perform()" % (action, (tpl % (bys[tag], selector))),
                indent=indent))
        elif atype == 'drag':
            drop_action = self._parse_action(param)
            if drop_action and drop_action[0] == "element" and not drop_action[2]:
                drop_tag, drop_selector = (drop_action[1], drop_action[3])
                tpl = "self.driver.find_element(By.%s, self.template(%r))"
                action = "drag_and_drop"
                drag_element = tpl % (bys[tag], selector)
                drop_element = tpl % (bys[drop_tag], drop_selector)
                action_elements.append(self.gen_statement(
                    "ActionChains(self.driver).%s(%s, %s).perform()" % (action, drag_element, drop_element),
                    indent=indent))
        elif atype == 'select':
            tpl = "self.driver.find_element(By.%s, self.template(%r))"
            action = "select_by_visible_text(self.template(%r))" % param
            action_elements.append(self.gen_statement("Select(%s).%s" % (tpl % (bys[tag], selector), action),
                                                      indent=indent))
        elif atype.startswith('assert') or atype.startswith('store'):
            if tag == 'title':
                if atype.startswith('assert'):
                    action_elements.append(
                        self.gen_statement("self.assertEqual(self.driver.title, self.template(%r))"
                                           % selector, indent=indent))
                else:
                    action_elements.append(self.gen_statement(
                        "self.vars['%s'] = self.template(self.driver.title)" % param.strip(), indent=indent
                    ))
            elif atype == 'store' and tag == 'string':
                action_elements.append(self.gen_statement(
                    "self.vars['%s'] = self.template('%s')" % (param.strip(), selector.strip()), indent=indent
                ))
            else:
                tpl = "self.driver.find_element(By.%s, self.template(%r)).%s"
                if atype in ['asserttext', 'storetext']:
                    action = "get_attribute('innerText')"
                elif atype in ['assertvalue', 'storevalue']:
                    action = "get_attribute('value')"
                if atype.startswith('assert'):
                    action_elements.append(
                        self.gen_statement("self.assertEqual(self.template(%s).strip(), self.template(%r).strip())" %
                                           (tpl % (bys[tag], selector, action), param),
                                           indent=indent))
                elif atype.startswith('store'):
                    action_elements.append(
                        self.gen_statement("self.vars['%s'] = self.template(%s)" %
                                           (param.strip(), tpl % (bys[tag], selector, action)),
                                           indent=indent))
        elif atype in ('click', 'type', 'keys', 'submit'):
            tpl = "self.driver.find_element(By.%s, self.template(%r)).%s"
            action = None
            if atype == 'click':
                action = "click()"
            elif atype == 'submit':
                action = "submit()"
            elif atype in ['keys', 'type']:
                if atype == 'type':
                    action_elements.append(self.gen_statement(
                        tpl % (bys[tag], selector, "clear()"), indent=indent))
                action = "send_keys(self.template(%r))" % str(param)
                if isinstance(param, (string_types, text_type)) and param.startswith("KEY_"):
                    action = "send_keys(Keys.%s)" % param.split("KEY_")[1]

            action_elements.append(self.gen_statement(tpl % (bys[tag], selector, action), indent=indent))

        elif atype == "script" and tag == "eval":
            cmd = 'self.driver.execute_script(self.template(%r))' % selector
            action_elements.append(self.gen_statement(cmd, indent=indent))
        elif atype == "rawcode":
            lines = param.split('\n')
            for line in lines:
                action_elements.append(self.gen_statement(line, indent=indent))
        elif atype == 'go':
            if selector and not param:
                cmd = "self.driver.get(self.template(%r))" % selector.strip()
                action_elements.append(self.gen_statement(cmd, indent=indent))
        elif atype == "editcontent":
            element = "self.driver.find_element(By.%s, %r)" % (bys[tag], selector)
            editable_error = "The element (By.%s, %r) " \
                             "is not contenteditable element" % (bys[tag], selector)
            editable_script_tpl = "arguments[0].innerHTML = %s;"
            editable_script_tpl_argument = "self.template.str_repr(self.template(%r))" % param.strip()
            editable_script = "%r %% %s" % \
                              (editable_script_tpl, editable_script_tpl_argument)
            action_elements.extend([
                self.gen_statement(
                    "if %s.get_attribute('contenteditable'):" % element,
                    indent=indent),
                self.gen_statement(
                    "self.driver.execute_script(",
                    indent=indent + self.INDENT_STEP),
                self.gen_statement(
                    "%s," % editable_script,
                    indent=indent + self.INDENT_STEP * 2),
                self.gen_statement(
                    element,
                    indent=indent + self.INDENT_STEP * 2),
                self.gen_statement(
                    ")",
                    indent=indent + self.INDENT_STEP),
                self.gen_statement(
                    "else:", indent=indent),
                self.gen_statement(
                    "raise NoSuchElementException(%r)" % editable_error,
                    indent=indent + self.INDENT_STEP)
            ])
        elif atype == 'echo' and tag == 'string':
            if len(selector) > 0 and not param:
                action_elements.append(
                    self.gen_statement("print(self.template(%r))" % selector.strip(), indent=indent))
        elif atype == 'wait':
            tpl = "WebDriverWait(self.driver, %s).until(econd.%s_of_element_located((By.%s, self.template(%r))), %r)"
            mode = "visibility" if param == 'visible' else 'presence'
            exc = TaurusConfigError("wait action requires timeout in scenario: \n%s" % self.scenario)
            timeout = dehumanize_time(self.scenario.get("timeout", exc))
            errmsg = "Element %r failed to appear within %ss" % (selector, timeout)
            action_elements.append(self.gen_statement(tpl % (timeout, mode, bys[tag], selector, errmsg), indent=indent))
        elif atype == 'pause' and tag == 'for':
            tpl = "sleep(%g)"
            action_elements.append(self.gen_statement(tpl % (dehumanize_time(selector),), indent=indent))
        elif atype == 'clear' and tag == 'cookies':
            action_elements.append(self.gen_statement("self.driver.delete_all_cookies()", indent=indent))
        elif atype == 'screenshot':
            if selector:
                filename = selector
                action_elements.append(self.gen_statement('self.driver.save_screenshot(self.template(%r))' % filename,
                                                          indent=indent))
            else:
                filename = "filename = os.path.join(os.getenv('TAURUS_ARTIFACTS_DIR'), " \
                           "'screenshot-%d.png' % (time() * 1000))"
                action_elements.append(self.gen_statement(filename, indent=indent))
                action_elements.append(self.gen_statement('self.driver.save_screenshot(filename)', indent=indent))

        if not action_elements:
            raise TaurusInternalException("Could not build code for action: %s" % action_config)

        return action_elements

    def _parse_action(self, action_config):
        if isinstance(action_config, string_types):
            name = action_config
            param = None
        elif isinstance(action_config, dict):
            name, param = next(iteritems(action_config))
        else:
            raise TaurusConfigError("Unsupported value for action: %s" % action_config)

        actions = "|".join(['click', 'doubleClick', 'mouseDown', 'mouseUp', 'mouseMove', 'select', 'wait', 'keys',
                            'pause', 'clear', 'assert', 'assertText', 'assertValue', 'submit', 'close', 'script',
                            'editcontent', 'switch', 'switchFrame', 'go', 'echo', 'type', 'element', 'drag',
                            'storeText', 'storeValue', 'store', 'open', 'screenshot', 'rawCode'
                            ])

        tag = "|".join(self.TAGS) + "|For|Cookies|Title|Window|Eval|ByIdx|String"
        expr = re.compile("^(%s)(%s)?(\(([\S\s]*)\))?$" % (actions, tag), re.IGNORECASE)
        res = expr.match(name)
        if not res:
            msg = "Unsupported action: %s" % name
            if self.ignore_unknown_actions:
                self.log.warning(msg)
                return
            else:
                raise TaurusConfigError(msg)

        atype = res.group(1).lower()
        tag = res.group(2).lower() if res.group(2) else ""
        selector = res.group(4)

        # hello, reviewer!
        if selector:
            if selector.startswith('"') and selector.endswith('"'):
                selector = selector[1:-1]
            elif selector.startswith("'") and selector.endswith("'"):
                selector = selector[1:-1]
        else:
            selector = ""
        return atype, tag, param, selector
