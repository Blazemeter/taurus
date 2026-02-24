# coding=utf-8

import ast
import astunparse
import os

import bzt
import bzt.utils
import bzt.modules._apiritif.generator
import bzt.modules._selenium
from bzt.modules._selenium import SeleniumExecutor
from bzt import TaurusConfigError
from tests.unit import RESOURCES_DIR, ExecutorTestCase, local_paths_config
from tests.unit.modules._selenium import SeleniumTestCase, MockPythonTool


class TestSeleniumScriptGeneration(ExecutorTestCase):
    EXECUTOR = SeleniumExecutor

    def __init__(self, methodName='runTest'):
        super(TestSeleniumScriptGeneration, self).__init__(methodName)
        self.tmp_selenium = None
        self.tmp_apiritif = None
        self.tmp_selenium_apiritif = None

    def setUp(self):
        super(TestSeleniumScriptGeneration, self).setUp()
        self.tmp_selenium = bzt.modules._selenium.Selenium
        self.tmp_apiritif = bzt.modules._apiritif.executor.Apiritif
        self.tmp_selenium_apiritif = bzt.modules._apiritif.executor.Selenium
        bzt.modules._selenium.Selenium = MockPythonTool
        bzt.modules._apiritif.executor.Apiritif = MockPythonTool
        bzt.modules._apiritif.executor.Selenium = MockPythonTool

        paths = [local_paths_config()]
        self.engine.configure(paths)  # FIXME: avoid using whole engine in particular module test!

        self.obj.settings = self.engine.config.get("modules").get("selenium")
        self.obj.install_required_tools = lambda: None

    def tearDown(self):
        if self.obj and self.obj.runner:
            if self.obj.runner.stdout:
                self.obj.runner.stdout.close()
            if self.obj.runner.stderr:
                self.obj.runner.stderr.close()
        bzt.modules._selenium.Selenium = self.tmp_selenium
        bzt.modules._apiritif.executor.Apiritif = self.tmp_apiritif
        bzt.modules._apiritif.executor.Selenium = self.tmp_selenium_apiritif
        super(TestSeleniumScriptGeneration, self).tearDown()

    def test_nfc(self):
        # nose flow control: setup/teardown + graceful
        self.obj.engine.config.load([RESOURCES_DIR + 'selenium/test_nfc.yml'])
        self.configure(self.obj.engine.config['execution'][0])
        self.obj.settings['verbose'] = True
        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/test_nfc.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)

    def test_self_test(self):
        f1 = RESOURCES_DIR + "selenium/f1.py"
        f2 = RESOURCES_DIR + "selenium/f2.py"
        suspicious_method = self.assertFilesEqual
        different_files = f1, f2
        self.assertRaises(AssertionError, suspicious_method, *different_files)

    def test_modern_actions_generator(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "default-address": "http://blazedemo.com",
                    "variables": {
                        "red_pill": "take_it",
                        "name": "Name"
                    },
                    "timeout": "3.5s",
                    "requests": [{
                        "label": "la-la",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "actions": [
                            # windows
                            "switchWindow(0)",
                            "openWindow(some.url)",
                            "resizeWindow(750, 750)",
                            "maximizeWindow()",
                            "closeWindow()",
                            "closeWindow('win_ser_local')",

                            # frames
                            "switchFrameByIdx(1)",
                            "switchFrame(relative=parent)",
                            "switchFrameByName('my_frame')",
                            "switchFrame('name=my_frame')",
                            "switchFrame('id=my_frame_id')",
                            "switchFrame('xpath=//xpath')",
                            "switchFrame('css=my_frame_cls')",

                            # chains
                            "mouseDownByXPath(/html/body/div[3]/form/select[1])",
                            "mouseOutById(id_abc)",
                            "mouseOverByName(name_abc)",

                            # drag, select, assert, store
                            {"dragByID(address)": "elementByName(toPort)"},
                            {"selectByName(my_frame)": "London"},
                            "assertTitle(BlazeDemo)",
                            {"storeTitle()": "hEaDeR"},
                            {"storeString(Title_Basic_By)": "Final"},
                            {"assertValueByID(address)": "123 Beautiful st."},
                            {"storeTextByID(address)": "Basic"},

                            # click, type, keys, submit
                            {"typeByName(\"toPort\")": "B"},
                            {"typeSecretByName(\"toPort\")": "mysecret"},

                            # exec, rawcode, go, edit
                            "scriptEval(\"{alert('This is ${sparta}');}\")",
                            "scriptEval(\"{alert('Alert');}\")",
                            {"rawCode": "for i in range(10):\n  if i % 2 == 0:\n    print(i)"},
                            "go(http:\\blazemeter.com)",
                            {"editContentById(editor)": "lo-la-lu"},

                            # print, wait, pause, clearcookies, screenshot
                            "echoString(${red_pill})",
                            {"waitForByName('toPort', visible)": "3.5s"},
                            "pauseFor(4.6s)",
                            "clearCookies()",
                            "screenshot('screen.png')",
                            "screenshot()"
                        ]}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        print_i = "print(i)"

        self.assertNotIn(content, "self.dlg_mng = DialogsManager(self.driver)")

        target_lines = [
            "switch_window('0')",
            "open_window('some.url')",
            "close_window()",
            "close_window('win_ser_local')",
            "switch_frame('index=1')",
            "switch_frame('relative=parent')",
            "switch_frame(self.driver.find_element(By.NAME, 'my_frame'))",
            "switch_frame(self.driver.find_element(By.CSS_SELECTOR, 'my_frame_cls'))",
            "switch_frame(self.driver.find_element(By.ID, 'my_frame_id'))",
            "switch_frame(self.driver.find_element(By.XPATH, '//xpath'))",
            "ActionChains(self.driver).click_and_hold(self.driver.find_element(var_loc_chain[0], "
            "var_loc_chain[1])).perform()",
            "ActionChains(self.driver).move_to_element_with_offset(self.driver.find_element(var_loc_chain[0],"
            "var_loc_chain[1])",
            "ActionChains(self.driver).move_to_element(self.driver.find_element(var_loc_chain[0],"
            "var_loc_chain[1])).perform()",
            "ActionChains(self.driver).drag_and_drop(self.driver.find_element(source[0], source[1]),"
            "self.driver.find_element(target[0],target[1])).perform()",
            "Select(self.driver.find_element(var_loc_select[0],var_loc_select[1])).select_by_visible_text",
            "self.assertEqual(self.driver.title,'BlazeDemo')",
            "self.vars['hEaDeR'] = self.driver.title",
            "self.vars['Final'] = 'Title_Basic_By'",
            "self.vars['Basic'] = self.driver.find_element(var_loc_as[0],var_loc_as[1])."
            "get_attribute('innerText')",
            "self.assertEqual(self.driver.find_element(var_loc_as[0],var_loc_as[1])."
            "get_attribute('value').strip(),\'123 Beautiful st.\'.strip())",
            "self.driver.find_element(var_loc_keys[0],var_loc_keys[1]).clear()",
            "self.driver.find_element(var_loc_keys[0],var_loc_keys[1]).send_keys('B')",
            "self.driver.find_element(var_loc_keys[0],var_loc_keys[1]).send_keys('mysecret')",
            "self.driver.execute_script(\"{{alert(\'Thisis{}\');}}\".format(self.vars[\'sparta\']))",
            "for i in range(10):",
            "if ((i % 2) == 0):",
            print_i,
            "self.driver.get(\'http:\\\\blazemeter.com\')",
            "ifself.driver.find_element(var_edit_content[0], var_edit_content[1])."
            "get_attribute('contenteditable'):"
            "self.driver.execute_script((\"arguments[0].innerHTML=\'%s\';\"%\'lo-la-lu\'),"
            "self.driver.find_element(var_edit_content[0],var_edit_content[1]))"
            "else:",
            "raiseNoSuchElementException((\'The element (%s : %r)is not a contenteditable element\'%"
            "(var_edit_content[0], var_edit_content[1])))"
            "print(self.vars['red_pill'])",
            "wait_for('visible', [{'name': 'toPort'}], 3.5)"
            "sleep(4.6)",
            "self.driver.delete_all_cookies()",
            "self.driver.save_screenshot('screen.png')",
            "filename = os.path.join(os.getenv('TAURUS_ARTIFACTS_DIR'), "
            "('screenshot-%d.png' % (time() * 1000)))",
            "self.driver.save_screenshot(filename)"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(TestSeleniumScriptGeneration.clear_spaces(target_lines[idx]),
                          TestSeleniumScriptGeneration.clear_spaces(content),
                          msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_actionid_try_except_inside_step_method(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"
            }],
            "scenarios": {
                "loc_sc": {
                    "default-address": "https://example.com",
                    "requests": [{
                        "label": "tx",
                        "actions": [{
                            "type": "go",
                            "param": "https://example.com",
                            "value": None,
                            "actionId": "aid-1"
                        }]
                    }]
                }
            }
        })

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertIn(
            TestSeleniumScriptGeneration.clear_spaces("with apiritif.smart_transaction('tx'):\n            try:"),
            TestSeleniumScriptGeneration.clear_spaces(content)
        )
        self.assertIn(
            TestSeleniumScriptGeneration.clear_spaces("raise type(exc)"),
            TestSeleniumScriptGeneration.clear_spaces(content)
        )
        self.assertIn(
            TestSeleniumScriptGeneration.clear_spaces("actionId: "),
            TestSeleniumScriptGeneration.clear_spaces(content)
        )
        self.assertIn(
            TestSeleniumScriptGeneration.clear_spaces("def test_locsc(self):\n        self._1_tx()"),
            TestSeleniumScriptGeneration.clear_spaces(content)
        )

    @staticmethod
    def clear_spaces(content):
        return content.replace(" ", "").replace("\t", "").replace("\n", "")

    def test_firefox_setup_generator(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "headless": True,
                    "default-address": "http://blazedemo.com",
                    "variables": {
                        "red_pill": "take_it",
                        "name": "Name"
                    },
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "bla.com",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],

                    }]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "options = webdriver.FirefoxOptions()",
            "options.headless = True",
            "profile = webdriver.FirefoxProfile()",
            "profile.set_preference('webdriver.log.file', '",
            "self.driver = webdriver.Firefox(profile, options=options)",
            "options.set_capability('unhandledPromptBehavior', 'ignore')"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_chrome_setup_generator(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "hold-for": "4m",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    'generate-flow-markers': True,
                    "browser": "Chrome",
                    "default-address": "http://blazedemo.com",
                    "variables": {
                        "red_pill": "take_it",
                        "name": "Name"
                    },
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "bla.com",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],

                    }]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertNotIn("options.set_headless()", content)

        target_lines = [
            "options = webdriver.ChromeOptions()",
            "self.driver = webdriver.Chrome(service_log_path='",
            "', options=options)",
            "options.set_capability('unhandledPromptBehavior', 'ignore')"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_edge_setup_generator(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "edge",
                    "requests": [{
                        "url": "bla.com"}],
                }}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertIn("options = webdriver.EdgeOptions()", content)
        self.assertIn("self.driver = webdriver.Edge()", content)

    def test_other_setup_generator(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "opera",
                    "requests": [{
                        "url": "bla.com"}],
                }}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertIn("options = ArgOptions()", content)
        self.assertIn("self.driver = webdriver.", content)

    def test_arguments_option_generator_ff(self):
        # Option arguments is only available for Firefox and Chrome
        # Option arguments is available for other browsers starting from Selenium version 4
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Firefox",
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "arguments": ["one", "two"]}}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "options.add_argument('one')",
            "options.add_argument('two')"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_convert_url_to_action(self):
        # in order to get appropriate logging for 'actionless' request would be great
        # handle it as 'go' action. (enclose with action_start/action_end methods)
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "blazedemo_test-Selenium"}],
            "scenarios": {
                "blazedemo_test-Selenium": {
                    "requests": [{
                        "label": "open blazedemo",
                        "url": "https://blazedemo.com/"
                    }, {
                        "label": "just_go",
                        "actions": [{"go('https//blazemeter.com')": None}]
                    }]
                }
            },
            "modules": {
                "selenium": {"version": "3"},
                "apiritif": {
                    "plugins-path": "something_similar_to_path"
                }
            }
        })

        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/test_action_start.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)

    def test_options_generator_browser_chrome(self):
        # Selenium version 3. Browser Chrome.
        # Supported options: arguments, experimental-options
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Chrome",
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "version": "3",
                    "options": {
                        "ignore-proxy": True,  # Option ignore-proxy is only available starting from Selenium version 4
                        "arguments": ["one", "two"],
                        "experimental-options": {  # Option experimental-options is only available in Chrome
                            "key1": "value1",
                            "key2": {"key22": "value22"}},
                        "preferences": {  # Option preferences is only available in Firefox
                            "key1": "value1",
                            "key2": {"key22": "value22"}}}}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertNotIn("options.set_preference", content)
        self.assertNotIn("options.ignore_local_proxy_environment_variables", content)

        target_lines = [
            "options.add_argument('one')",
            "options.add_argument('two')",
            "options.add_experimental_option('key1', 'value1')",
            "options.add_experimental_option('key2', {'key22': 'value22'})"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_options_generator_browser_firefox(self):
        # Selenium version 3. Browser Firefox.
        # Supported options: arguments, preferences
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Firefox",
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "version": "3",
                    "options": {
                        "ignore-proxy": True,  # Option ignore-proxy is only available starting from Selenium version 4
                        "arguments": ["one", "two"],
                        "experimental-options": {  # Option experimental-options is only available in Chrome
                            "key1": "value1",
                            "key2": {"key22": "value22"}},
                        "preferences": {  # Option preferences is only available in Firefox
                            "key1": "value1",
                            "key2": {"key22": "value22"}}}}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertNotIn("options.add_experimental_option", content)
        self.assertNotIn("options.ignore_local_proxy_environment_variables", content)

        target_lines = [
            "options.add_argument('one')",
            "options.add_argument('two')",
            "options.set_preference('key1', 'value1')",
            "options.set_preference('key2', {'key22': 'value22'})"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_options_generator_browser_ie(self):
        # Selenium version 3. Browser Ie.
        # Supported options: None
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Ie",
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "version": "3",
                    "options": {
                        "ignore-proxy": True,  # Option ignore-proxy is only available starting from Selenium version 4
                        "arguments": ["one", "two"],  # Option arguments is only available starting from Selenium 4
                        "experimental-options": {  # Option experimental-options is only available in Chrome
                            "key1": "value1"},
                        "preferences": {  # Option preferences is only available in Firefox
                            "key1": "value1"}}}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertNotIn("options.ignore_local_proxy_environment_variables", content)
        self.assertNotIn("options.add_argument('one')", content)
        self.assertNotIn("options.add_experimental_option", content)
        self.assertNotIn("options.set_preference", content)

    def test_options_generator_remote_firefox(self):
        # Selenium version 3. Remote webdriver. Browser Firefox.
        # Supported options: arguments, preferences
        self.configure({
            "execution": [{
                "scenario": "loc_sc_remote"}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {
                        "browserName": "firefox"},
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "version": "3",
                    "options": {
                        "ignore-proxy": True,  # Option ignore-proxy is only available starting from Selenium version 4
                        "arguments": ["one", "two"],
                        "experimental-options": {  # Option experimental-options is only available in Chrome
                            "key1": "value1",
                            "key2": {"key22": "value22"}},
                        "preferences": {  # Option preferences is only available in Firefox
                            "key1": "value1",
                            "key2": {"key22": "value22"}}}}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertNotIn("options.add_experimental_option", content)
        self.assertNotIn("options.ignore_local_proxy_environment_variables", content)

        target_lines = [
            "options.add_argument('one')",
            "options.add_argument('two')",
            "options.set_preference('key1', 'value1')",
            "options.set_preference('key2', {'key22': 'value22'})"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_options_generator_firefox(self):
        self.configure({
            "execution": [{
                "scenario": "loc_sc",
                "capabilities": {
                    "name2": "val2"}}],
            "scenarios": {
                "loc_sc": {
                    "capabilities": {
                        "name1": "val1"},
                    "browser": "Firefox",
                    "requests": [{
                        "url": "bla.com"}]}}, })

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertIn("options.set_capability('name1', 'val1')", content)
        self.assertIn("options.set_capability('name2', 'val2')", content)

    def test_options_generator_chrome(self):
        self.configure({
            "execution": [{
                "scenario": "loc_sc",
                "capabilities": {
                    "name2": "val2"}}],
            "scenarios": {
                "loc_sc": {
                    "capabilities": {
                        "name1": "val1"},
                    "browser": "Chrome",
                    "requests": [{
                        "url": "bla.com"}]}}, })

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertIn("options.set_capability('name1', 'val1')", content)
        self.assertIn("options.set_capability('name2', 'val2')", content)

    def test_build_script(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "data-sources": [{"path": "first.csv", "loop": True}, "second.csv"],
                    "default-address": "http://blazedemo.com",
                    "variables": {
                        "red_pill": "take_it",
                        "name": "Name"
                    },
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "actions": [
                            {"waitForByXPath(//input[@type='submit'], present)": "3.5s"},
                            {"waitForByXPath(//input[@name='test,name'], present)": "1m20s"},
                            "assertTitle(BlazeDemo)",
                            "mouseMoveByXPath(/html/body/div[2]/div/p[2]/a)",
                            "doubleClickByXPath(/html/body/div[3]/h2)",
                            "contextClickByXPath(/html/body/div[3]/form/select[1])",
                            "mouseDownByXPath(/html/body/div[3]/form/select[1])",
                            "mouseUpByXPath(/html/body/div[3]/form/select[1]/option[6])",
                            {"selectByName(toPort)": "London"},
                            {"keysByCSS(body input.btn.btn-primary)": "KEY_ENTER"},
                            {"assertValueByID(address)": "123 Beautiful st."},
                            {"assertTextByXPath(/html/body/div[2]/form/div[1]/label)": "${name}"},
                            {"waitForByName('toPort', visible)": "3.5s"},
                            {"keysByName(\"toPort\")": "B"},
                            {"typeByName(\"toPort\")": "B"},
                            {"keysByName(\"toPort\")": u"KEY_ENTER"},
                            {"typeByName(\"toPort\")": "KEY_ENTER"},
                            {"typeByName(\"toPort\")": "mypassword"},
                            "clickByXPath(//div[3]/form/select[1]//option[3])",
                            "clickByXPath(//div[3]/form/select[2]//option[6])",
                            "switchWindow(0)",
                            "openWindow(some.url)",
                            "switchWindow('win_ser_local')",
                            "switchWindow('win_ser_1')",
                            "switchWindow('that_window')",
                            "closeWindow(1)",
                            "closeWindow('win_ser_local')",
                            "closeWindow('win_ser_1')",
                            "closeWindow('that_window')",
                            "submitByName(\"toPort\")",
                            "scriptEval(\"alert('This is Sparta');\")",
                            "scriptEval(\"{alert('This is ${red_pill}');}\")",
                            {"rawCode": "for i in range(10):\n  if i % 2 == 0:\n    print(i)"},
                            {"dragByID(address)": "elementByName(toPort)"},
                            "switchFrameByName('my_frame')",
                            "switchFrame('top_frame')",
                            "switchFrameByXpath(//*[@id='result'])",
                            "switchFrameByCSS(.my_class)",
                            "switchFrameById(frame_id)",
                            "switchFrameByIdx(1)",
                            "switchFrame(relative=parent)",
                            {"editContentById(editor)": "lo-la-lu"},
                            "pauseFor(3.5s)",
                            "clearCookies()",
                            "clickByLinkText(destination of the week! The Beach!)",
                            {"storeTitle()": "Title"},
                            {"storeTextByXPath(//*[@id='basics']/h2)": "Basic"},
                            {"storeValueByXPath(//*[@id='basics']/h1)": "World"},
                            {"storeString(${Title} ${Basic} by ${By})": "Final"},
                            {"storeEval(0 == false)": "var_eval"},
                            "assertEval(10 === 2*5)",
                            "go(http:\\blazemeter.com)",
                            {"assertDialog(alert)": "Alert Message"},
                            {"assertDialog(prompt)": "Enter value"},
                            {"assertDialog(confirm)": "Are you sure?"},
                            {"answerDialog(prompt)": "myvalue"},
                            {"answerDialog(confirm)": "#Ok"},
                            {"answerDialog(alert)": "#Ok"},
                            "echoString(${red_pill})",
                            "screenshot(screen.png)",
                            "screenshot()",
                        ],
                    },
                        {"label": "empty"}
                    ]
                },
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {
                        "browserName": "firefox",
                        "version": "54.0",
                        "platformName": "linux",
                        "javascriptEnabled": "True",
                        "platformVersion": "",
                        "seleniumVersion": "",
                        "deviceName": "",
                        "app": ""
                    },
                    "default-address": "http://blazedemo.com",
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "actions": [
                            "waitByXPath(//input[@type='submit'])",
                            "assertTitle(BlazeDemo)"
                        ],
                    },
                        {"label": "empty"}
                    ]
                }
            }
        })

        # it changes default of data-source loop parameter to 'false' (see second.csv params)
        self.obj.engine.aggregator.is_functional = True

        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/generated_from_requests.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)
        with open(self.obj.script) as script:
            self.assertIn("bzt.resources.selenium_extras", script.read())


    def test_build_script_report_inside_actions(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "data-sources": [{"path": "first.csv", "loop": True}, "second.csv"],
                    "default-address": "http://blazedemo.com",
                    "variables": {
                        "red_pill": "take_it",
                        "name": "Name"
                    },
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "report-inside-actions": True,
                        "actions": [
                            {"waitForByXPath(//input[@type='submit'], present)": "3.5s"},
                            {"waitForByXPath(//input[@name='test,name'], present)": "1m20s"},
                            "assertTitle(BlazeDemo)",
                            "mouseMoveByXPath(/html/body/div[2]/div/p[2]/a)",
                            "doubleClickByXPath(/html/body/div[3]/h2)",
                            "contextClickByXPath(/html/body/div[3]/form/select[1])",
                            "mouseDownByXPath(/html/body/div[3]/form/select[1])",
                            "mouseUpByXPath(/html/body/div[3]/form/select[1]/option[6])",
                            {"selectByName(toPort)": "London"},
                            {"keysByCSS(body input.btn.btn-primary)": "KEY_ENTER"},
                            {"assertValueByID(address)": "123 Beautiful st."},
                            {"assertTextByXPath(/html/body/div[2]/form/div[1]/label)": "${name}"},
                            {"waitForByName('toPort', visible)": "3.5s"},
                            {"keysByName(\"toPort\")": "B"},
                            {"typeByName(\"toPort\")": "B"},
                            {"keysByName(\"toPort\")": u"KEY_ENTER"},
                            {"typeByName(\"toPort\")": "KEY_ENTER"},
                            {"typeByName(\"toPort\")": "mypassword"},
                            "clickByXPath(//div[3]/form/select[1]//option[3])",
                            "clickByXPath(//div[3]/form/select[2]//option[6])",
                            "switchWindow(0)",
                            "openWindow(some.url)",
                            "switchWindow('win_ser_local')",
                            "switchWindow('win_ser_1')",
                            "switchWindow('that_window')",
                            "closeWindow(1)",
                            "closeWindow('win_ser_local')",
                            "closeWindow('win_ser_1')",
                            "closeWindow('that_window')",
                            "submitByName(\"toPort\")",
                            "scriptEval(\"alert('This is Sparta');\")",
                            "scriptEval(\"{alert('This is ${red_pill}');}\")",
                            {"rawCode": "for i in range(10):\n  if i % 2 == 0:\n    print(i)"},
                            {"dragByID(address)": "elementByName(toPort)"},
                            "switchFrameByName('my_frame')",
                            "switchFrame('top_frame')",
                            "switchFrameByXpath(//*[@id='result'])",
                            "switchFrameByCSS(.my_class)",
                            "switchFrameById(frame_id)",
                            "switchFrameByIdx(1)",
                            "switchFrame(relative=parent)",
                            {"editContentById(editor)": "lo-la-lu"},
                            "pauseFor(3.5s)",
                            "clearCookies()",
                            "clickByLinkText(destination of the week! The Beach!)",
                            {"storeTitle()": "Title"},
                            {"storeTextByXPath(//*[@id='basics']/h2)": "Basic"},
                            {"storeValueByXPath(//*[@id='basics']/h1)": "World"},
                            {"storeString(${Title} ${Basic} by ${By})": "Final"},
                            {"storeEval(0 == false)": "var_eval"},
                            "assertEval(10 === 2*5)",
                            "go(http:\\blazemeter.com)",
                            {"assertDialog(alert)": "Alert Message"},
                            {"assertDialog(prompt)": "Enter value"},
                            {"assertDialog(confirm)": "Are you sure?"},
                            {"answerDialog(prompt)": "myvalue"},
                            {"answerDialog(confirm)": "#Ok"},
                            {"answerDialog(alert)": "#Ok"},
                            {
                                "loop" : "i",
                                "start": 1,
                                "end": 1,
                                "do" : ["echoString(${red_pill})"]
                            },
                            "screenshot(screen.png)",
                            "screenshot()",
                        ],
                    },
                        {"label": "empty"}
                    ]
                },
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {
                        "browserName": "firefox",
                        "version": "54.0",
                        "platformName": "linux",
                        "javascriptEnabled": "True",
                        "platformVersion": "",
                        "seleniumVersion": "",
                        "deviceName": "",
                        "app": ""
                    },
                    "default-address": "http://blazedemo.com",
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "report-inside-actions": True,
                        "actions": [
                            "waitByXPath(//input[@type='submit'])",
                            "assertTitle(BlazeDemo)"
                        ],
                    },
                        {
                            "label": "empty",
                            "report-inside-actions": True
                        }
                    ]
                }
            }
        })

        # it changes default of data-source loop parameter to 'false' (see second.csv params)
        self.obj.engine.aggregator.is_functional = True

        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/generated_from_requests_report_inside_actions.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)
        with open(self.obj.script) as script:
            self.assertIn("bzt.resources.selenium_extras", script.read())

    def test_headless_default(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Chrome",
                    "requests": ["http://blazedemo.com/"]
                }}})

        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.read()

        self.assertNotIn("options.set_headless()", gen_contents)

    def test_headless_chrome(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Chrome",
                    "headless": True,
                    "requests": ["http://blazedemo.com/"]
                }}})

        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.read()

        self.assertIn("options.headless = True", gen_contents)

    def test_headless_firefox(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Firefox",
                    "headless": True,
                    "requests": ["http://blazedemo.com/"]
                }}})

        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.read()

        self.assertIn("options.headless = True", gen_contents)

    def test_headless_safari(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "safari",
                    "headless": True,
                    "requests": ["http://blazedemo.com/"]
                }}})

        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.read()

        self.assertNotIn("options.headless = True", gen_contents)

    def test_capabilities_update(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc_remote"}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {
                        "proxy": {"key": "val"},
                        "string_cap": "string_val"},
                    "requests": [{
                        "url": "https://blazedemo.com"}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "options.set_capability('proxy', {'key': 'val'})",
            "options.set_capability('string_cap', 'string_val')"
        ]

        wrong_line = "desired_capabilities={'proxy': {'key': 'val'}, 'string_cap': 'string_val'}"

        for line in target_lines:
            self.assertIn(line, content)

        self.assertNotIn(wrong_line, content)

    def test_capabilities_order(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "hold-for": "4m",
                "scenario": "loc_sc_remote",
                "capabilities": {
                    "name2": "execution",
                    "name4": "execution",
                    "name5": "execution"}}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {
                        "name3": "scenario",
                        "name4": "scenario",
                        "name6": "scenario"},
                    "default-address": "http://blazedemo.com",
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "actions": ["assertTitle(BlazeDemo)"]}]}},
            "modules": {
                "selenium": {
                    "capabilities": {
                        "name1": "settings",
                        "name2": "settings",
                        "name3": "settings"}}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "options.set_capability('name1', 'settings')",
            "options.set_capability('name2', 'execution')",
            "options.set_capability('name3', 'scenario')",
            "options.set_capability('name4', 'execution')",
            "options.set_capability('name5', 'execution')",
            "options.set_capability('name6', 'scenario')"]

        for line in target_lines:
            self.assertIn(line, content)

    def test_build_script_remote(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc_remote"}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {
                        "browserName": "firefox",
                        "version": "54.0",
                        "platformName": "linux",
                        "javascriptEnabled": "True",
                        "platformVersion": "",
                        "seleniumVersion": "",
                        "deviceName": "",
                        "app": ""
                    },
                    "default-address": "http://blazedemo.com",
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "actions": [
                            {"waitForByXPath(//input[@type='submit'], present)": "3.5s"},
                            "assertTitle(BlazeDemo)"
                        ],
                    },
                        {"label": "empty"}
                    ]
                }
            }
        })

        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/generated_from_requests_remote.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_remote_without_browser_validation(self):
        browser_name = "some strange browser"
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc_remote"}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {"browserName": browser_name},
                    "requests": [{"url": "/"}]}}})

        self.obj.prepare()
        with open(self.obj.script) as script:
            content = script.read()

        sample = "options.set_capability('browserName', '%s')" % browser_name
        self.assertIn(sample, content)

    def test_build_script_appium_browser(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc_appium"}],
            "scenarios": {
                "loc_sc_appium": {
                    "browser": "Chrome-Android",
                    "capabilities": {
                        "deviceName": "",
                    },
                    "default-address": "http://blazedemo.com",
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "actions": [
                            {"waitForByXPath(//input[@type='submit'], present)": "3.5s"},
                            "assertTitle(BlazeDemo)"
                        ],
                    },
                        {"label": "empty"}
                    ]
                }
            }
        })

        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/generated_from_requests_appium_browser.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_build_script_remote_chrome_browser(self):
        """ taurus should not wipe browserName (from capabilities) """
        self.configure({
            "execution": [{
                "executor": "selenium",
                "remote": "http://addr-of-remote-server.com",
                "scenario": "remote_sc"}],
            "scenarios": {
                "remote_sc": {
                    "capabilities": {
                        "browserName": "chrome"},  # must be set among other capabilities
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "http://blazedemo.com",
                        "actions": [
                            {"waitForByXPath(//input[@type='submit'], present)": "3.5s"}]},
                        {"label": "empty"}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target = "options.set_capability('browserName', 'chrome')"
        self.assertIn(target, content)

    def test_build_script_remote_firefox_browser(self):
        """ check usage of 'browser' scenario options as browserName (from capabilities) """
        self.configure({
            "execution": [{
                "executor": "selenium",
                "remote": "http://addr-of-remote-server.com",
                "scenario": "remote_sc"}],
            "scenarios": {
                "remote_sc": {
                    "browser": "Firefox",  # must be set among other capabilities
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "http://blazedemo.com",
                        "actions": [
                            {"waitForByXPath(//input[@type='submit'], present)": "3.5s"}]},
                        {"label": "empty"}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target = "options.set_capability('browserName', 'firefox')"
        self.assertIn(target, content)

    def test_build_script_remote_edge_browser(self):
        """ check usage of 'browser' scenario options as browserName (from capabilities) """
        self.configure({
            "execution": [{
                "executor": "selenium",
                "remote": "http://addr-of-remote-server.com",
                "scenario": "remote_sc"}],
            "scenarios": {
                "remote_sc": {
                    "browser": "edge",
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "http://blazedemo.com",
                        "actions": [
                            {"waitForByXPath(//input[@type='submit'], present)": "3.5s"}]},
                        {"label": "empty"}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target = "options.set_capability('browserName', 'MicrosoftEdge')"
        self.assertIn(target, content)

    def test_build_script_flow_markers(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "hold-for": "4m",
                "ramp-up": "3m",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "generate-flow-markers": True,
                    "browser": "Chrome",
                    "default-address": "http://blazedemo.com",
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "/",
                        "assert": [{
                            "contains": ['contained_text'],
                            "not": True
                        }],
                        "actions": [
                            {"waitForByXPath(//input[@type='submit'], present)": "3.5s"},
                            "assertTitle(BlazeDemo)"
                        ],
                    },
                        {"label": "empty"}
                    ]
                }
            }
        })
        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/generated_from_requests_flow_markers.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)

    def test_add_external_logging(self):
        self.configure({
            "modules": {
                "apiritif": {
                    "plugins-path": "/tmp"
                }
            },
            "execution": [{
                "executor": "selenium",
                "scenario": "sample"}],
            "scenarios": {
                "sample": {
                    "browser": "Chrome",
                    "requests": [{
                        "label": "Test",
                        "actions": [
                            "go(http://blazedemo.com/)",
                            "log(leaving blazedemo)",
                        ],
                    }]
                }
            }
        })
        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/external_logging.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)

    def test_resize_window(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "concurrency": "1",
                "iterations": "1",
                "scenario": "window"}],
            "scenarios": {
                "window": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "actions": [
                            "resizeWindow(450, 450)",
                            "maximizeWindow()",
                            "closeWindow()"
                        ],
                    }, ]
                },
            }
        })

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "self.driver.set_window_size('450', '450')",
            "self.driver.maximize_window()"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_open_window_var(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": {
                    "requests": [{
                        "actions": [
                            {"storeString(test_string)": "test"},
                            "openWindow('${test}')"]}]}}]})
        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()
        self.assertIn("open_window(self.vars['test'])", content)

    def test_alert(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "concurrency": "1",
                "iterations": "1",
                "scenario": "window"}],
            "scenarios": {
                "window": {
                    "default-address": "http://blazedemo.com",
                    "requests": [{
                        "url": "/",
                        "actions": [
                            "alert('OK')",
                            "alert('Dismiss')"
                        ],
                    }, ]
                },
            }
        })

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "self.driver.switch_to.alert.accept()",
            "self.driver.switch_to.alert.dismiss()"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_non_utf(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "simple"}],
            "scenarios": {
                "simple": {
                    "requests": [{
                        "url": "http://blazedemo.com/测试",
                    }, ]
                },
            }
        })

        self.obj.prepare()
        with open(self.obj.script, encoding='utf8') as fds:
            content = fds.read()

        target_lines = [
            "with apiritif.smart_transaction('http://blazedemo.com/测试')",
            "self.driver.get(\'http://blazedemo.com/测试')"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_mix_syntax(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "click",
                                "locators": [
                                    {"name": "btn1"},
                                ]
                            },
                            {"typeById(Id_123)": "London"}
                        ]}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "var_loc_keys=get_locator([{'name':'btn1',}])",
            "self.driver.find_element(var_loc_keys[0],var_loc_keys[1]).click()",
            "var_loc_keys=get_locator([{'id':'Id_123',}])",
            "self.driver.find_element(var_loc_keys[0],var_loc_keys[1]).clear()",
            "self.driver.find_element(var_loc_keys[0],var_loc_keys[1]).send_keys('London')"
        ]

        for idx in range(len(target_lines)):
            target_lines[idx] = astunparse.unparse(ast.parse(target_lines[idx]))
            self.assertIn(TestSeleniumScriptGeneration.clear_spaces(target_lines[idx]),
                          TestSeleniumScriptGeneration.clear_spaces(content),
                          msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_syntax2_drag_drop(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "drag",
                                "source": [
                                    {"xpath": "/xpath/to"}
                                ],
                                "target": [
                                    {"css": "mycss"},
                                    {"id": "ID"}
                                ]
                            }
                        ]}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "source=get_locator([{'xpath':'/xpath/to'}])",
            "target=get_locator([{'css':'mycss'},{'id':'ID'}])",
            "ActionChains(self.driver).drag_and_drop(self.driver.find_element(source[0],source[1]),"
            "self.driver.find_element(target[0],target[1])).perform()"
        ]
        for idx in range(len(target_lines)):
            target_lines[idx] = astunparse.unparse(ast.parse(target_lines[idx]))
            self.assertIn(TestSeleniumScriptGeneration.clear_spaces(target_lines[idx]),
                          TestSeleniumScriptGeneration.clear_spaces(content),
                          msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_syntax2_drag_drop_missing_source(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "drag",
                                "source": {

                                },
                                "target": {
                                    "locators": [
                                        {"css": "mycss"},
                                        {"id": "ID"}
                                    ]
                                }
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Can not generate action for \'drag\'. Source is empty.' in str(context.exception))

    def test_syntax2_missing_param_assert_store(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "assertText",
                                "locators": [
                                    {"css": "classname"}
                                ]
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Missing param' in str(context.exception))

    def test_syntax2_missing_param_edit(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "editContent",
                                "locators": [
                                    {"css": "classname"}
                                ]
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Missing param' in str(context.exception))

    def test_syntax2_build_script(self):
        self.configure(
            {
                "modules": {
                    "apiritif": {
                        "plugins-path": "/tmp"
                    }
                },
                "execution": [
                    {
                        "executor": "apiritif",
                        "scenario": "loc_sc"
                    }
                ],
                "scenarios": {
                    "loc_sc": {
                        "default-address": "http://blazedemo.com,",
                        "variables": {
                            "my_xpath_locator": "/html/body/div[3]",
                            "red_pill": "take_it,",
                            "name": "Name",
                            "pos": 100
                        },
                        "timeout": "3.5s",
                        "requests": [
                            {
                                "label": "Test V2",
                                "actions": [
                                    {
                                        "type": "go",
                                        "param": "http://blazedemo.com"
                                    },
                                    {
                                        "type": "resizeWindow",
                                        "param": "750, 750"
                                    },
                                    {
                                        "type": "switchWindow",
                                        "param": 0
                                    },
                                    {
                                        "type": "mouseDown",
                                        "locators": [
                                            {"id": "invalid_id"},
                                            {"xpath": "${my_xpath_locator}"}
                                        ]
                                    },
                                    {
                                        "type": "mouseOut",
                                        "locators": [{"id": "id_123"}]
                                    },
                                    {
                                        "type": "mouseOver",
                                        "locators": [{"name": "name_123"}]
                                    },
                                    {
                                        "type": "drag",
                                        "source": [
                                            {"name": "invalid_name"},
                                            {"xpath": "/html/body/div[2]/div/p[2]/a"}
                                        ],
                                        "target": [
                                            {"css": "invalid_css"},
                                            {"xpath": "/html/body/div[3]/form/div"}
                                        ]
                                    },
                                    {
                                        "type": "assertText",
                                        "param": "Choose your departure city:",
                                        "locators": [
                                            {"css": "myclass"},
                                            {"xpath": "/html/body/div[3]/h2"}
                                        ]
                                    },
                                    {
                                        "type": "assertValue",
                                        "param": "Find Flights",
                                        "locators": [
                                            {"css": "myclass"},
                                            {"xpath": "/html/body/div[3]/form/div/input"}
                                        ]
                                    },
                                    {
                                        "type": "assertTitle",
                                        "param": "BlazeDemo"
                                    },
                                    {
                                        "type": "storeTitle",
                                        "param": "hEaDeR"
                                    },
                                    {
                                        "type": "storeString",
                                        "param": "final_var",
                                        "value": "test_text"
                                    },
                                    {
                                        "type": "storeText",
                                        "param": "Basic",
                                        "locators": [{"xpath": "/html/body/div[3]/h2"}]
                                    },
                                    {
                                        "type": "assertEval",
                                        "param": "10 === 2*5"
                                    },
                                    {
                                        "type": "assertEval",
                                        "param": "var_assert",
                                        "value": "myFunction();\nfunction myFunction(){\n "
                                                 "btnNameVar=\"${btnName1}\";\n return \"support\";\n}"
                                    },
                                    {
                                        "type": "storeEval",
                                        "param": "var_eval",
                                        "value": "myFunction();\nfunction myFunction(){\n "
                                                 "btnNameVar=\"${btnName1}\";\n return \"support\";\n}"
                                    },
                                    {
                                        "type": "storeEval",
                                        "param": "var_eval",
                                        "value": '["${id1}", "${id2}", "${id3}", "${id4}"]'
                                    },
                                    {
                                        "type": "click",
                                        "locators": [
                                            {"xpath": "/wrong/one"},
                                            {"xpath": "/html/body/div[3]/form/div/input"}
                                        ]
                                    },
                                    {
                                        "type": "keys",
                                        "param": "KEY_ENTER",
                                        "locators": [
                                            {"xpath": "/doc/abc"},
                                            {"css": "body > div.container > table > tbody > tr:nth-child(1) "
                                                    "> td:nth-child(2) > input"}
                                        ]
                                    },
                                    {
                                        "type": "type",
                                        "param": "myusername",
                                        "locators": [
                                            {"id": "fjkafjk"},
                                            {"css": "testCss"}
                                        ]
                                    },
                                    {
                                        "type": "typeSecret",
                                        "param": "mysecret",
                                        "locators": [
                                            {"id": "fjkafjk"},
                                            {"css": "testCss"}
                                        ]
                                    },
                                    {
                                        "type": "select",
                                        "param": "American Express",
                                        "locators": [
                                            {"css": "myclass"},
                                            {"xpath": "//*[@id=\"cardType\"]"}
                                        ]
                                    },
                                    {
                                        "type": "scriptEval",
                                        "param": "{window.scrollTo(0, document.body.scrollHeight);}"
                                    },
                                    {
                                        "type": "scriptEval",
                                        "param": "{window.scrollTo(0, ${pos});}"
                                    },
                                    {
                                        "type": "scriptEval",
                                        "param": "{"
                                                 "var event = new InputEvent('input', { "
                                                 "bubbles: true,"
                                                 "cancelable: false,"
                                                 "data: true });"
                                                 "}"
                                    },
                                    {
                                        "type": "rawCode",
                                        "param": "for i in range(10):\n  if i % 2 == 0:\n    print(i)"
                                    },
                                    {
                                        "type": "echoString",
                                        "param": "${red_pill}"
                                    },
                                    {
                                        "type": "pauseFor",
                                        "param": "4.6s"
                                    },
                                    {
                                        "type": "clearCookies"
                                    },
                                    {
                                        "type": "screenshot",
                                        "param": "screen.png"
                                    },
                                    {
                                        "type": "screenshot"
                                    },
                                    {
                                        "type": "waitFor",
                                        "param": "visible",
                                        "locators": [
                                            {"css": "invalid_css"},
                                            {"name": "inputName"}
                                        ],
                                        "value": "2h30m20s"
                                    },
                                    {
                                        "type": "editContent",
                                        "param": "lo-la-lu",
                                        "locators": [{"id": "editor"}]
                                    },
                                    {
                                        "type": "pauseFor",
                                        "param": "4.6s"
                                    },
                                    {
                                        "type": "clearCookies"
                                    },
                                    {
                                        "type": "screenshot",
                                        "param": "screen.png"
                                    },
                                    {
                                        "type": "screenshot"
                                    },
                                    {
                                        "type": "openWindow",
                                        "param": "vacation.html"
                                    },
                                    {
                                        "type": "maximizeWindow"
                                    },
                                    {
                                        "type": "switchFrameByIdx",
                                        "param": 1
                                    },
                                    {
                                        "type": "switchFrame",
                                        "param": "relative=parent"
                                    },
                                    {
                                        "type": "switchFrameByName",
                                        "param": "my_frame"
                                    },
                                    {
                                        "type": "switchFrame",
                                        "param": "name=my_frame"
                                    },
                                    {
                                        "type": "switchFrame",
                                        "param": "css=my_frame_cls"
                                    },
                                    {
                                        "type": "switchFrame",
                                        "param": "id=my_frame_id"
                                    },
                                    {
                                        "type": "switchFrame",
                                        "param": "xpath='//xpath'"
                                    },
                                    {
                                        "type": "closeWindow"
                                    },
                                    {
                                        "type": "answerDialog",
                                        "param": "prompt",
                                        "value": "my input"
                                    },
                                    {
                                        "type": "answerDialog",
                                        "param": "confirm",
                                        "value": '#Ok'
                                    },
                                    {
                                        "type": "answerDialog",
                                        "param": "alert",
                                        "value": '#Ok'
                                    },
                                    {
                                        "type": "assertDialog",
                                        "param": "alert",
                                        "value": "Exception occurred!"
                                    },
                                    {
                                        "type": "assertDialog",
                                        "param": "confirm",
                                        "value": "Are you sure?"
                                    },
                                    {
                                        "type": "assertDialog",
                                        "param": "prompt",
                                        "value": "What is your age?"
                                    },
                                ]
                            }
                        ]
                    }
                }
            }
        )

        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/generated_from_requests_v2.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)
        with open(self.obj.script) as script:
            self.assertIn("bzt.resources.selenium_extras", script.read())

    def test_switch_frame_no_locator(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "switchFrame",
                                "param": ""
                            },
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue("Can not generate action for 'switchFrame'. Selector is empty." in str(context.exception))

    def test_conditions(self):
        self.configure(
            {
                "execution": [
                    {
                        "executor": "apiritif",
                        "scenario": "loc_sc"
                    }
                ],
                "scenarios": {
                    "loc_sc": {
                        "default-address": "http://blazedemo.com,",
                        "browser": "Chrome",
                        "variables": {
                            "city_select_name": "fromPort",
                            "input_name_id": "inputName"
                        },
                        "timeout": "3.5s",
                        "requests": [
                            {
                                "label": "Conditions test",
                                "actions": [
                                    "go(http://blazedemo.com)",
                                    {
                                        "if": "document.getElementsByName(\"fromPort\")[0].length > 0",
                                        "then": [
                                            {
                                                "type": "click",
                                                "locators": [
                                                    {
                                                        "id": "wrong_id"
                                                    },
                                                    {
                                                        "xpath": "/html/body/div[3]/form/div/input"
                                                    }
                                                ]
                                            },
                                            "pauseFor(1s)",
                                            {
                                                "if": "document.getElementsByClassName(\"table\")[0].rows.length > 5",
                                                "then": [
                                                    "clickByXPath(/html/body/div[2]/table/tbody/tr[5]/td[1]/input)",
                                                    {
                                                        "if": "document.getElementById(\"${input_name_id}\").value "
                                                              "=== ''",
                                                        "then": [
                                                            {
                                                                "typeById(${input_name_id})": "John Doe"
                                                            }
                                                        ],
                                                        "else": [
                                                            {
                                                                "typeById(${input_name_id})": "Jack Green"
                                                            }
                                                        ]
                                                    },
                                                    "clickByXPath(/html/body/div[2]/form/div[11]/div/input)",
                                                    "pauseFor(5s)"
                                                ]
                                            }
                                        ],
                                        "else": [
                                            {
                                                "if": "document.getElementsByClassName(\"table\")[0].rows.length > 5",
                                                "then": [
                                                    {
                                                        "typeById(${elem2_id})": "my text"
                                                    },
                                                    {
                                                        "if": "window.screen.width > 1000",
                                                        "then": [
                                                            "screenshot(file_1000)"
                                                        ],
                                                        "else": [
                                                            "screenshot(file)"
                                                        ]
                                                    }
                                                ],
                                                "else": [
                                                    "clickByXPath(/html/body/div[3]/input)"
                                                ]
                                            }
                                        ]
                                    }
                                ]
                            }
                        ]
                    }
                }
            }
        )

        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/generated_from_requests_if_then_else.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)

    def test_conditions_missing_then(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "if": "document.getElementsByName(\"fromPort\")[0].length > 0"
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Missing then' in str(context.exception))

    def test_loop_missing_end(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "loop": "i",
                                "start": 1,
                                "do": [
                                    "clickById(dd)"
                                ]
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Loop must contain' in str(context.exception))

    def test_loop_missing_start(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "loop": "i",
                                "end": 10,
                                "do": [
                                    "clickById(dd)"
                                ]
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Loop must contain' in str(context.exception))

    def test_loop_missing_do(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "loop": "i",
                                "start": 1,
                                "end": 10,
                                "do": []
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Loop must contain' in str(context.exception))

    def test_loop_step_defaults_to_1(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "actions": [
                            {
                                "loop": "i",
                                "start": 1,
                                "end": 10,
                                "do": [
                                    "clickById(${i})"
                                ]
                            }
                        ]}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "for i in get_loop_range(1, 10, 1)",
            "self.vars['i'] = str(i)",
            "get_locator([{'id': self.vars['i']"

        ]
        for idx in range(len(target_lines)):
            self.assertIn(TestSeleniumScriptGeneration.clear_spaces(target_lines[idx]),
                          TestSeleniumScriptGeneration.clear_spaces(content),
                          msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_loop_step_2(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "actions": [
                            {
                                "loop": "i",
                                "start": 1,
                                "end": 10,
                                "step": 2,
                                "do": [
                                    "clickById(id)"
                                ]
                            }
                        ]}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "for i in get_loop_range(1, 10, 2)",
            "self.vars['i'] = str(i)"
        ]
        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_loop_step_negative(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "actions": [
                            {
                                "loop": "i",
                                "start": 10,
                                "end": 0,
                                "step": -1,
                                "do": [
                                    "clickById(id)"
                                ]
                            }
                        ]}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "for i in get_loop_range(10, 0, -1)",
            "self.vars['i'] = str(i)"
        ]
        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_loop_w_variables(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "variables": {
                        "start": 10,
                        "end": 20,
                        "step": 1
                    },
                    "requests": [{
                        "actions": [
                            {
                                "loop": "i",
                                "start": "${start}",
                                "end": "${end}",
                                "step": "${step}",
                                "do": [
                                    "clickById(id_${i})"
                                ]
                            }
                        ]}]}}})

        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/generated_from_requests_loop_variables.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)

    def test_loop_str_var_fields(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "variables": {
                        "step": 1
                    },
                    "requests": [{
                        "actions": [
                            {
                                "loop": "i",
                                "start": "1",
                                "end": "10",
                                "step": '1${step}',
                                "do": [
                                    "clickById(id)"
                                ]
                            }
                        ]}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "for i in get_loop_range(1, 10, '1{}'.format(self.vars['step']))",
            "self.vars['i'] = str(i)"
        ]
        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_assert_dialog_wrong_type(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "assertDialog(wrong)": "test"
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue("assertDialog type must be one of the following: 'alert', 'prompt' or 'confirm'"
                        in str(context.exception))

    def test_answer_dialog_wrong_type(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "answerDialog(wrong)": "test"
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue("answerDialog type must be one of the following: 'alert', 'prompt' or 'confirm'"
                        in str(context.exception))

    def test_answer_confirm_incorrect_type(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "answerDialog(confirm)": "value"
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue("answerDialog of type confirm must have value either '#Ok' or '#Cancel'"
                        in str(context.exception))

    def test_answer_alert_incorrect_type(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "answerDialog(alert)": "value"
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue("answerDialog of type alert must have value '#Ok'"
                        in str(context.exception))

    def test_wait_for(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "waitFor",
                                "param": "visible",
                                "locators": [
                                    {"css": "invalid_css"},
                                    {"id": "input_id"}
                                ],
                                "value": "2h30m20s"
                            }, {
                                "type": "waitFor",
                                "param": "visible",
                                "locators": [
                                    {"css": "invalid_css"},
                                    {"id": "input_id"}
                                ],
                            },
                            {"waitForById(myId, present)": "5s"},
                            {"waitForById(myId, clickable)": "5s"},
                            {"waitForById(myId, notvisible)": "5s"},
                            "waitForById(myId, notpresent)",
                            "waitForById(myId, notclickable)"
                        ]}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "wait_for('visible', [{'css':'invalid_css'}, {'id':'input_id'}], 9020.0)",
            "wait_for('visible', [{'css':'invalid_css'}, {'id':'input_id'}], 10.0)",
            "wait_for('present', [{'id':'myId'}], 5.0)",
            "wait_for('clickable', [{'id':'myId'}], 5.0)",
            "wait_for('notvisible', [{'id':'myId'}], 5.0)",
            "wait_for('notpresent', [{'id':'myId'}], 10.0)",
            "wait_for('notclickable', [{'id':'myId'}], 10.0)"
        ]
        for idx in range(len(target_lines)):
            target_lines[idx] = astunparse.unparse(ast.parse(target_lines[idx]))
            self.assertIn(TestSeleniumScriptGeneration.clear_spaces(target_lines[idx]),
                          TestSeleniumScriptGeneration.clear_spaces(content),
                          msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_wait_for_invalid_cond(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {"waitForById(myId, invisible)": "10s"},
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Invalid condition' in str(context.exception),
                        "Given string was not found in '%s'" % str(context.exception))

    def test_foreach_all_by_element_actions(self):
        self.configure(
            {
                "execution": [
                    {
                        "executor": "apiritif",
                        "scenario": "loc_sc"
                    }
                ],
                "scenarios": {
                    "loc_sc": {
                        "default-address": "http://blazedemo.com,",
                        "browser": "Chrome",
                        "variables": {
                            "city_select_name": "fromPort",
                            "input_name_id": "inputName"
                        },
                        "timeout": "3.5s",
                        "requests": [
                            {
                                "label": "Foreach test",
                                "actions": [
                                    {
                                        "foreach": "el",
                                        "locators": [
                                            {"css": "input"},
                                            {"xpath": "/table/input/"},
                                        ],
                                        "do": [
                                            {"assertTextByElement(el)": "text"},
                                            {
                                                "type": "assertText",
                                                "element": "el",
                                                "param": "text",
                                                "locators": [
                                                    {"css": "style"},
                                                    {"xpath": "//tr"}
                                                ]
                                            },
                                            {"assertValueByElement(el)": "value"},
                                            {
                                                "type": "assertValue",
                                                "element": "el",
                                                "param": "value"
                                            },
                                            {"editContentByElement(el)": "new text"},
                                            {
                                                "type": "editContent",
                                                "element": "el",
                                                "param": "new text"
                                            },
                                            "clickByElement(el)",
                                            {
                                                "type": "click",
                                                "element": "el",
                                                "locators": [
                                                    {"css": "input-cls"},
                                                    {"xpath": "//input"}
                                                ]
                                            },
                                            "doubleClickByElement(el)",
                                            {
                                                "type": "doubleClick",
                                                "element": "el",
                                            },
                                            "contextClickByElement(el)",
                                            {
                                                "type": "contextClick",
                                                "element": "el",
                                            },
                                            "mouseDownByElement(el)",
                                            {
                                                "type": "mouseDown",
                                                "element": "el",
                                            },
                                            "mouseUpByElement(el)",
                                            {
                                                "type": "mouseUp",
                                                "element": "el",
                                            },
                                            "mouseOutByElement(el)",
                                            {
                                                "type": "mouseOut",
                                                "element": "el",
                                            },
                                            "mouseOverByElement(el)",
                                            {
                                                "type": "mouseOver",
                                                "element": "el",
                                            },
                                            {"dragByElement(el)": "elementById(id12)"},
                                            {"dragById(id34)": "elementByElement(el)"},
                                            {
                                                "type": "drag",
                                                "source": [
                                                    {"element": "el"}
                                                ],
                                                "target": [
                                                    {"id": "id12"}
                                                ]
                                            },
                                            {
                                                "type": "drag",
                                                "source": [
                                                    {"id": "id34"}
                                                ],
                                                "target": [
                                                    {"element": "el"}
                                                ]
                                            },
                                            {"selectByElement(el)": "value"},
                                            {
                                                "type": "select",
                                                "element": "el",
                                                "param": "value"
                                            },
                                            {"storeTextByElement(el)": "my_var"},
                                            {
                                                "type": "storeText",
                                                "element": "el",
                                                "param": "my_var"
                                            },
                                            {"storeValueByElement(el)": "my_var"},
                                            {
                                                "type": "storeValue",
                                                "element": "el",
                                                "param": "my_var"
                                            },
                                            {"typeByElement(el)": "text"},
                                            {
                                                "type": "type",
                                                "element": "el",
                                                "param": "text"
                                            },
                                            {"typeSecretByElement(el)": "passwd"},
                                            {
                                                "type": "typeSecret",
                                                "element": "el",
                                                "param": "passwd"
                                            },
                                            "submitByElement(el)",
                                            {
                                                "type": "submit",
                                                "element": "el"
                                            },
                                            {"keysByElement(el)": "KEY_ENTER"},
                                            {
                                                "type": "keys",
                                                "element": "el",
                                                "param": "KEY_ENTER"
                                            },
                                        ]
                                    }
                                ]
                            }
                        ]
                    }
                }
            }
        )

        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/generated_from_requests_foreach.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)

    def test_foreach_missing_locators(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "foreach": "element",
                                "do": [
                                    "clickByElement(element)"
                                ]
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue("Foreach loop must contain locators and do" in str(context.exception))

    def test_all_by_shadow_actions(self):
        self.configure(
            {
                "execution": [
                    {
                        "executor": "apiritif",
                        "scenario": "loc_sc"
                    }
                ],
                "scenarios": {
                    "loc_sc": {
                        "default-address": "http://blazedemo.com,",
                        "browser": "Chrome",
                        "variables": {
                            "city_select_name": "fromPort",
                            "input_name_id": "inputName",
                            "button_name": "test_btn"
                        },
                        "timeout": "3.5s",
                        "requests": [
                            {
                                "label": "Shadow locators test",
                                "actions": [
                                    {"assertTextByShadow(c-basic, lightning-accordion-section, .slds-button)": "text"},
                                    {
                                        "type": "assertText",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button[name=${button_name}]",
                                        "param": "text"
                                    },
                                    {
                                        "assertValueByShadow(c-basic, lightning-accordion-section, .slds-button)": "value"},
                                    {
                                        "type": "assertValue",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                        "param": "value"
                                    },
                                    {
                                        "editContentByShadow(c-basic, lightning-accordion-section, .slds-button)": "new text"},
                                    {
                                        "type": "editContent",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                        "param": "new text"
                                    },
                                    "clickByShadow(c-basic, lightning-accordion-section, .slds-button)",
                                    {
                                        "type": "click",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                    },
                                    "doubleClickByShadow(c-basic, lightning-accordion-section, .slds-button)",
                                    {
                                        "type": "doubleClick",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                    },
                                    "contextClickByShadow(c-basic, lightning-accordion-section, .slds-button)",
                                    {
                                        "type": "contextClick",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                    },
                                    "mouseDownByShadow(c-basic, lightning-accordion-section, .slds-button)",
                                    {
                                        "type": "mouseDown",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                    },
                                    "mouseUpByShadow(c-basic, lightning-accordion-section, .slds-button)",
                                    {
                                        "type": "mouseUp",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                    },
                                    "mouseOutByShadow(c-basic, lightning-accordion-section, .slds-button)",
                                    {
                                        "type": "mouseOut",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                    },
                                    "mouseOverByShadow(c-basic, lightning-accordion-section, .slds-button)",
                                    {
                                        "type": "mouseOver",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                    },
                                    {
                                        "dragByShadow(c-basic, lightning-accordion-section, .slds-button)": "elementById(id12)"},
                                    {
                                        "dragById(id34)": "elementByShadow(c-basic, lightning-accordion-section, .slds-button)"},
                                    {
                                        "type": "drag",
                                        "source": [
                                            {"shadow": "c-basic, lightning-accordion-section, .slds-button"}
                                        ],
                                        "target": [
                                            {"id": "id12"}
                                        ]
                                    },
                                    {
                                        "type": "drag",
                                        "source": [
                                            {"id": "id34"}
                                        ],
                                        "target": [
                                            {"shadow": "c-basic, lightning-accordion-section, .slds-button"}
                                        ]
                                    },
                                    {"selectByShadow(c-basic, lightning-accordion-section, .slds-button)": "value"},
                                    {
                                        "type": "select",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                        "param": "value"
                                    },
                                    {"storeTextByShadow(c-basic, lightning-accordion-section, .slds-button)": "my_var"},
                                    {
                                        "type": "storeText",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                        "param": "my_var"
                                    },
                                    {
                                        "storeValueByShadow(c-basic, lightning-accordion-section, .slds-button)": "my_var"},
                                    {
                                        "type": "storeValue",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                        "param": "my_var"
                                    },
                                    {"typeByShadow(c-basic, lightning-accordion-section, .slds-button)": "text"},
                                    {
                                        "type": "type",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                        "param": "text"
                                    },
                                    {"typeSecretByShadow(c-basic, lightning-accordion-section, .slds-button)": "password"},
                                    {
                                        "type": "typeSecret",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                        "param": "password"
                                    },
                                    "submitByShadow(c-basic, lightning-accordion-section, .slds-button)",
                                    {
                                        "type": "submit",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button"
                                    },
                                    {"keysByShadow(c-basic, lightning-accordion-section, .slds-button)": "KEY_ENTER"},
                                    {
                                        "type": "keys",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                        "param": "KEY_ENTER"
                                    },
                                    {"waitForByShadow('toPort', visible)": "5s"},
                                    {
                                        "type": "waitFor",
                                        "shadow": "c-basic, lightning-accordion-section, .slds-button",
                                        "param": "notvisible",
                                        "value": "2s"
                                    }
                                ]
                            }
                        ]
                    }
                }
            }
        )

        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/generated_from_requests_shadow.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)

    def test_testdata_publish(self):
        self.configure({
            "settings": {
                "master_publish_url": "https://tdm.blazemeter.com/api/v1/publish?signature=8UJR9hHfsdjg9032nkvx"
            },
            "execution": [{
                "executor": "apiritif",
                "scenario": "publish_sc"}],
            "scenarios": {
                "publish_sc": {
                    "requests": [{
                        "actions": [
                            {
                                "type": "rawCode",
                                "param": "do_testdata_orchestration('publish', 'entity1', 'target1', True)",
                            },
                            {
                                "type": "rawCode",
                                "param": "do_testdata_orchestration('un-publish', 'entity1', 'target1')",
                            }
                        ]}]}}})
        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/generated_from_requests_td_publish.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)

    def test_loop_over_data_all(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "scenario1"}],
            "scenarios": {
                "scenario1": {
                    "data-sources": [{
                        "path": "/somewhere/cities.csv",
                        "delimiter": ",",
                        "quoted": False,
                        "encoding": "utf-8",
                        "loop": True,
                        "variable-names": "name, country"
                    }],
                    "requests": [{
                        "actions": [
                            {
                                "loopOverData": "cities.csv",
                                "variable": "city",
                                "from": 2,
                                "to": 5,
                                "do": [{
                                    "type": "go",
                                    "param": "https://www.google.com?q=${city.name}"
                                }]
                            }
                        ]}]}}})
        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/loop_over_data_all.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)

    def test_loop_over_data_no_indexes(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "scenario1"}],
            "scenarios": {
                "scenario1": {
                    "data-sources": [{
                        "path": "/somewhere/cities.csv",
                        "delimiter": ",",
                        "quoted": False,
                        "encoding": "utf-8",
                        "loop": True,
                        "variable-names": "name, country"
                    }],
                    "requests": [{
                        "actions": [
                            {
                                "loopOverData": "cities.csv",
                                "variable": "city",
                                "do": [{
                                    "type": "go",
                                    "param": "https://www.google.com?q=${city.name}"
                                }]
                            }
                        ]}]}}})
        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/loop_over_data_no_indexes.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)

    def test_loop_over_data_only_from_ix(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "scenario1"}],
            "scenarios": {
                "scenario1": {
                    "data-sources": [{
                        "path": "/somewhere/cities.csv",
                        "delimiter": ",",
                        "quoted": False,
                        "encoding": "utf-8",
                        "loop": True,
                        "variable-names": "name, country"
                    }],
                    "requests": [{
                        "actions": [
                            {
                                "loopOverData": "cities.csv",
                                "variable": "city",
                                "from": 3,
                                "do": [{
                                    "type": "go",
                                    "param": "https://www.google.com?q=${city.name}"
                                }]
                            }
                        ]}]}}})
        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/loop_over_data_only_from_ix.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)

    def test_loop_over_data_only_to_ix(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "scenario": "scenario1"}],
            "scenarios": {
                "scenario1": {
                    "data-sources": [{
                        "path": "/somewhere/cities.csv",
                        "delimiter": ",",
                        "quoted": False,
                        "encoding": "utf-8",
                        "loop": True,
                        "variable-names": "name, country"
                    }],
                    "requests": [{
                        "actions": [
                            {
                                "loopOverData": "cities.csv",
                                "variable": "city",
                                "to": 100,
                                "do": [{
                                    "type": "go",
                                    "param": "https://www.google.com?q=${city.name}"
                                }]
                            }
                        ]}]}}})
        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/loop_over_data_only_to_ix.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "/somewhere/", python_files=True)


class TestSelenium4Only(SeleniumTestCase):
    def obj_prepare(self):
        tmp_tool = bzt.modules._apiritif.executor.Apiritif
        try:
            bzt.modules._apiritif.executor.Apiritif = MockPythonTool
            self.obj.install_required_tools = lambda: None
            self.obj.prepare()
        finally:
            bzt.modules._apiritif.executor.Apiritif = tmp_tool

    def test_ignore_proxy_option_generator_selenium_4(self):
        # Option ignore_proxy is only available starting from Selenium version 4
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True}}}})

        self.obj_prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target = "options.ignore_local_proxy_environment_variables()"
        self.assertIn(target, content)

    def test_arguments_option_generator_ff_selenium_4(self):
        # Option arguments is only available for Firefox and Chrome
        # Option arguments is available for other browsers starting from Selenium version 4
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Firefox",
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "arguments": ["one", "two"]}}}})
        self.obj_prepare()
        with open(self.obj.script) as fds:
            content = fds.read()
        target_lines = [
            "options.add_argument('one')",
            "options.add_argument('two')"
        ]
        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_arguments_option_generator_edge_selenium_4(self):
        # Option arguments is only available for Firefox and Chrome
        # Option arguments is available for other browsers starting from Selenium version 4
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "edge",
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "arguments": ["one", "two"]}}}})

        self.obj_prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "options.add_argument('one')",
            "options.add_argument('two')",
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_arguments_option_generator_ie_selenium_4(self):
        # Option arguments is only available for Firefox and Chrome
        # Option arguments is available for other browsers starting from Selenium version 4
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Ie",
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "arguments": ["one", "two"]}}}})

        self.obj_prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "options.add_argument('one')",
            "options.add_argument('two')"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_headless_chrome_selenium_4(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Chrome",
                    "headless": True,
                    "requests": ["http://blazedemo.com/"]
                }}})

        self.obj_prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.read()
        self.assertIn("options.headless = True", gen_contents)

    def test_capabilities_options_for_remote_chrome(self):
        # Selenium version 4. Remote webdriver. Browser Chrome.
        # Supported options: arguments, experimental-options
        self.configure({
            "execution": [{
                "scenario": "loc_sc_remote"}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {
                        "browserName": "chrome",
                        "cap1": "val1",
                        "cap2": "val2"},
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True,
                        "arguments": ["one", "two"],
                        "experimental-options": {  # Option experimental-options is only available in Chrome
                            "key1": "value1",
                            "key2": {"key22": "value22"}},
                        "preferences": {  # Option preferences is only available in Firefox
                            "key1": "value1",
                            "key2": {"key22": "value22"}}}}}})

        self.obj_prepare()
        exp_file = RESOURCES_DIR + "selenium/capabilities_options_for_remote_chrome.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_capabilities_options_for_remote_firefox(self):
        # Selenium version 4. Remote webdriver. Browser Firefox.
        # Supported options: arguments, preferences
        self.configure({
            "execution": [{
                "scenario": "loc_sc_remote"}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {
                        "browserName": "firefox",
                        "cap1": "val1",
                        "cap2": "val2"},
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True,
                        "arguments": ["one", "two"],
                        "experimental-options": {  # Option experimental-options is only available in Chrome
                            "key1": "value1",
                            "key2": {"key22": "value22"}},
                        "preferences": {  # Option preferences is only available in Firefox
                            "key1": "value1",
                            "key2": {"key22": "value22"}}}}}})
        self.obj_prepare()
        exp_file = RESOURCES_DIR + "selenium/capabilities_options_for_remote_firefox.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_capabilities_options_for_remote_edge(self):
        # EdgeOptions is available for MicrosoftEdge starting from Selenium version 4
        self.configure({
            "execution": [{
                "executor": "selenium",
                "remote": "http://addr-of-remote-server.com",
                "scenario": "remote_sc"}],
            "scenarios": {
                "remote_sc": {
                    "browser": "edge",
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True,
                        "arguments": ["one", "two"]}}}})

        self.obj_prepare()
        exp_file = RESOURCES_DIR + "selenium/capabilities_options_for_remote_edge.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_capabilities_options_for_remote_other(self):
        # Selenium version 4. Remote webdriver. Unknown browser.
        # Supported options: none
        self.configure({
            "execution": [{
                "scenario": "loc_sc_remote"}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {
                        "cap1": "val1",
                        "cap2": "val2"},
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True,
                        "arguments": ["one", "two"],
                        "experimental-options": {"key1": "value1"},
                        "preferences": {"key1": "value1"}}}}})
        self.obj_prepare()
        exp_file = RESOURCES_DIR + "selenium/capabilities_options_for_remote_other.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_capabilities_options_for_remote_safari(self):
        # Selenium version 4. Remote webdriver. Browser Safari.
        # Supported options: arguments
        self.configure({
            "execution": [{
                "executor": "selenium",
                "remote": "http://addr-of-remote-server.com/api/v4/grid/wd/hub",
                "scenario": "remote_sc"}],
            "scenarios": {
                "remote_sc": {
                    "browser": "safari",
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True,
                        "arguments": ["one", "two"]}}}})

        self.obj_prepare()
        exp_file = RESOURCES_DIR + "selenium/capabilities_options_for_remote_safari.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)


class TestSelenium3Only(SeleniumTestCase):
    def obj_prepare(self):
        tmp_tool = bzt.modules._apiritif.executor.Apiritif
        try:
            bzt.modules._apiritif.executor.Apiritif = MockPythonTool
            self.obj.settings["version"] = "3"
            self.obj.install_required_tools = lambda: None
            self.obj.prepare()
        finally:
            bzt.modules._apiritif.executor.Apiritif = tmp_tool

    def test_ignore_proxy_option(self):
        # Option ignore-proxy is only available starting from Selenium version 4
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True}}}})

        self.obj_prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target = "options.ignore_local_proxy_environment_variables"
        self.assertNotIn(target, content)

    def test_options_for_chrome(self):
        # Selenium version 3. Browser Chrome.
        # Supported options: arguments, experimental-options
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Chrome",
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True,  # Option ignore-proxy is only available starting from Selenium version 4
                        "arguments": ["one", "two"],
                        "experimental-options": {  # Option experimental-options is only available in Chrome
                            "key1": "value1",
                            "key2": {"key22": "value22"}},
                        "preferences": {  # Option preferences is only available in Firefox
                            "key1": "value1",
                            "key2": {"key22": "value22"}}}}}})

        self.obj_prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertNotIn("options.set_preference", content)
        self.assertNotIn("options.ignore_local_proxy_environment_variables", content)

        target_lines = [
            "options.add_argument('one')",
            "options.add_argument('two')",
            "options.add_experimental_option('key1', 'value1')",
            "options.add_experimental_option('key2', {'key22': 'value22'})"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_options_for_firefox(self):
        # Selenium version 3. Browser Firefox.
        # Supported options: arguments, preferences
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Firefox",
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True,  # Option ignore-proxy is only available starting from Selenium version 4
                        "arguments": ["one", "two"],
                        "experimental-options": {  # Option experimental-options is only available in Chrome
                            "key1": "value1",
                            "key2": {"key22": "value22"}},
                        "preferences": {  # Option preferences is only available in Firefox
                            "key1": "value1",
                            "key2": {"key22": "value22"}}}}}})

        self.obj_prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertNotIn("options.add_experimental_option", content)
        self.assertNotIn("options.ignore_local_proxy_environment_variables", content)

        target_lines = [
            "options.add_argument('one')",
            "options.add_argument('two')",
            "options.set_preference('key1', 'value1')",
            "options.set_preference('key2', {'key22': 'value22'})"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_options_for_edge(self):
        # Selenium version 3. Browser Edge.
        # Supported options: None
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "edge",
                    "capabilities": {
                        "browserName": "chrome",
                        "cap1": "val1",
                        "cap2": "val2"},
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True,  # Option ignore-proxy is only available starting from Selenium version 4
                        "arguments": ["one", "two"],  # Option arguments is only available starting from Selenium 4
                        "experimental-options": {  # Option experimental-options is only available in Chrome
                            "key1": "value1"},
                        "preferences": {  # Option preferences is only available in Firefox
                            "key1": "value1"}}}}})

        self.obj_prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertNotIn("options.ignore_local_proxy_environment_variables", content)
        self.assertNotIn("options.add_argument('one')", content)
        self.assertNotIn("options.add_experimental_option", content)
        self.assertNotIn("options.set_preference", content)

    def test_options_for_ie(self):
        # Selenium version 3. Browser Ie.
        # Supported options: None
        self.configure({
            "execution": [{
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Ie",
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True,  # Option ignore-proxy is only available starting from Selenium version 4
                        "arguments": ["one", "two"],  # Option arguments is only available starting from Selenium 4
                        "experimental-options": {  # Option experimental-options is only available in Chrome
                            "key1": "value1"},
                        "preferences": {  # Option preferences is only available in Firefox
                            "key1": "value1"}}}}})

        self.obj_prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        self.assertNotIn("options.ignore_local_proxy_environment_variables", content)
        self.assertNotIn("options.add_argument('one')", content)
        self.assertNotIn("options.add_experimental_option", content)
        self.assertNotIn("options.set_preference", content)

    def test_capabilities_options_for_remote_chrome(self):
        # Selenium version 3. Remote webdriver. Browser Chrome.
        # Supported options: arguments, experimental-options
        self.configure({
            "execution": [{
                "scenario": "loc_sc_remote"}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {
                        "browserName": "chrome",
                        "cap1": "val1",
                        "cap2": "val2"},
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True,  # Option ignore-proxy is only available starting from Selenium version 4
                        "arguments": ["one", "two"],
                        "experimental-options": {  # Option experimental-options is only available in Chrome
                            "key1": "value1",
                            "key2": {"key22": "value22"}},
                        "preferences": {  # Option preferences is only available in Firefox
                            "key1": "value1",
                            "key2": {"key22": "value22"}}}}}})

        self.obj_prepare()
        exp_file = RESOURCES_DIR + "selenium/capabilities_options_for_remote_chrome_s3.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_capabilities_options_for_remote_firefox(self):
        # Selenium version 3. Remote webdriver. Browser Firefox.
        # Supported options: arguments, preferences
        self.configure({
            "execution": [{
                "scenario": "loc_sc_remote"}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {
                        "browserName": "firefox",
                        "cap1": "val1",
                        "cap2": "val2"},
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True,  # Option ignore-proxy is only available starting from Selenium version 4
                        "arguments": ["one", "two"],
                        "experimental-options": {  # Option experimental-options is only available in Chrome
                            "key1": "value1",
                            "key2": {"key22": "value22"}},
                        "preferences": {  # Option preferences is only available in Firefox
                            "key1": "value1",
                            "key2": {"key22": "value22"}}}}}})
        self.obj_prepare()
        exp_file = RESOURCES_DIR + "selenium/capabilities_options_for_remote_firefox_s3.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_capabilities_options_for_remote_other(self):
        # Selenium version 3. Remote webdriver. Unknown browser.
        # Supported options: none
        self.configure({
            "execution": [{
                "scenario": "loc_sc_remote"}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver_host:port/wd/hub",
                    "capabilities": {
                        "cap1": "val1",
                        "cap2": "val2"},
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "ignore-proxy": True,  # Option ignore-proxy is only available starting from Selenium version 4
                        "arguments": ["one", "two"],
                        "experimental-options": {"key1": "value1"},
                        "preferences": {"key1": "value1"}}}}})
        self.obj_prepare()
        exp_file = RESOURCES_DIR + "selenium/capabilities_options_for_remote_other_s3.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)

    def test_capabilities_options_for_remote_safari(self):
        # Selenium version 3. Remote webdriver. Browser Safari.
        # Supported options: arguments
        self.configure({
            "execution": [{
                "scenario": "loc_sc_remote"}],
            "scenarios": {
                "loc_sc_remote": {
                    "remote": "http://user:key@remote_web_driver:port/api/v4/grid/wd/hub",
                    "capabilities": {
                        "browserName": "safari",
                        "cap1": "val1",
                        "cap2": "val2"},
                    "requests": [{
                        "url": "bla.com"}]}},
            "modules": {
                "selenium": {
                    "options": {
                        "arguments": ["one", "two"]}}}})
        self.obj_prepare()
        exp_file = RESOURCES_DIR + "selenium/capabilities_options_for_remote_safari_s3.py"
        self.assertFilesEqual(exp_file, self.obj.script, python_files=True)
