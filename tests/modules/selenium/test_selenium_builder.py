# coding=utf-8

import ast
import astunparse
import os
import unittest

from bzt import TaurusConfigError
from bzt.six import PY2
from tests import RESOURCES_DIR
from tests.modules.selenium import SeleniumTestCase


class TestSeleniumScriptGeneration(SeleniumTestCase):

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

                            # exec, rawcode, go, edit
                            "scriptEval(\"alert('This is Sparta');\")",
                            {"rawCode": "for i in range(10):\n  if i % 2 == 0:\n    print(i)"},
                            "go(http:\\blazemeter.com)",
                            {"editContentById(editor)": "lo-la-lu"},

                            # print, wait, pause, clearcookies, screenshot
                            "echoString(${red_pill})",
                            {"waitByName('toPort')": "visible"},
                            "pauseFor(4.6s)",
                            "clearCookies()",
                            "screenshot('screen.png')",
                            "screenshot()"
                        ]}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        if PY2:
            print_i = "print i"
        else:
            print_i = "print(i)"

        target_lines = [
            "self.wnd_mng.switch('0')",
            """self.driver.execute_script("window.open('some.url');")""",
            "self.wnd_mng.close()",
            "self.wnd_mng.close('win_ser_local')",
            "self.frm_mng.switch('index=1')",
            "self.frm_mng.switch('relative=parent')",
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
            "self.driver.execute_script(\"alert('This is Sparta');\")",
            "for i in range(10):",
            "if ((i % 2) == 0):",
            print_i,
            "self.driver.get('http:\\\\blazemeter.com')",
            "ifself.driver.find_element(var_edit_content[0], var_edit_content[1])."
            "get_attribute('contenteditable'):"
            "self.driver.execute_script((\"arguments[0].innerHTML=\'%s\';\"%\'lo-la-lu\'),"
            "self.driver.find_element(var_edit_content[0],var_edit_content[1]))"
            "else:",
            "raiseNoSuchElementException((\'The element (%s : %r)is not a contenteditable element\'%"
            "(var_edit_content[0], var_edit_content[1])))"
            "print(self.vars['red_pill'])",
            "WebDriverWait(self.driver, 3.5).until(econd.visibility_of_element_located((var_loc_wait[0],"
            "var_loc_wait[1])), \"Element 'name':'toPort' failed to appear within 3.5s\")",
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
            "options.set_headless()",
            "profile = webdriver.FirefoxProfile()",
            "profile.set_preference('webdriver.log.file', '",
            "driver = webdriver.Firefox(profile, firefox_options=options)"
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
            "driver = webdriver.Chrome(service_log_path='",
            "', chrome_options=options)"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

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
                            "waitByXPath(//input[@type='submit'])",
                            "assertTitle(BlazeDemo)",
                            "mouseMoveByXPath(/html/body/div[2]/div/p[2]/a)",
                            "doubleClickByXPath(/html/body/div[3]/h2)",
                            "mouseDownByXPath(/html/body/div[3]/form/select[1])",
                            "mouseUpByXPath(/html/body/div[3]/form/select[1]/option[6])",
                            {"selectByName(toPort)": "London"},
                            {"keysByCSS(body input.btn.btn-primary)": "KEY_ENTER"},
                            {"assertValueByID(address)": "123 Beautiful st."},
                            {"assertTextByXPath(/html/body/div[2]/form/div[1]/label)": "${name}"},
                            {"waitByName('toPort')": "visible"},
                            {"keysByName(\"toPort\")": "B"},
                            {"typeByName(\"toPort\")": "B"},
                            {"keysByName(\"toPort\")": u"KEY_ENTER"},
                            {"typeByName(\"toPort\")": "KEY_ENTER"},
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
                            {"rawCode": "for i in range(10):\n  if i % 2 == 0:\n    print(i)"},
                            {"dragByID(address)": "elementByName(toPort)"},
                            "switchFrameByName('my_frame')",
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
                            "go(http:\\blazemeter.com)",
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

        self.assertIn("options.set_headless()", gen_contents)

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

        self.assertIn("options.set_headless()", gen_contents)

    def test_headless_safari(self):
        self.configure({
            "execution": [{
                "executor": "selenium",
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "browser": "Opera",
                    "headless": True,
                    "requests": ["http://blazedemo.com/"]
                }}})

        self.obj.prepare()
        with open(self.obj.script) as generated:
            gen_contents = generated.read()

        self.assertNotIn("options.set_headless()", gen_contents)

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
            "'name1': 'settings'",
            "'name2': 'execution'",
            "'name3': 'scenario'",
            "'name4': 'execution'",
            "'name5': 'execution'",
            "'name6': 'scenario'"]

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
                            "waitByXPath(//input[@type='submit'])",
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
                            "waitByXPath(//input[@type='submit'])",
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

    def test_build_script_remote_empty_browser(self):
        """ taurus should not wipe browserName (from capabilities) """
        self.configure({
            "execution": [{
                "executor": "selenium",
                "remote": "http://addr-of-remote-server.com",
                "scenario": "remote_sc"}],
            "scenarios": {
                "remote_sc": {  # no 'browser' element
                    "capabilities": {
                        "browserName": "chrome"},  # must be faced in desired_capabilities
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "http://blazedemo.com",
                        "actions": [
                            "waitByXPath(//input[@type='submit'])"]},
                        {"label": "empty"}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target = "'browserName': 'chrome'"
        self.assertIn(target, content)

    def test_build_script_remote_browser(self):
        """ taurus should not wipe browserName (from capabilities) """
        self.configure({
            "execution": [{
                "executor": "selenium",
                "remote": "http://addr-of-remote-server.com",
                "scenario": "remote_sc"}],
            "scenarios": {
                "remote_sc": {
                    "capabilities": {
                        "browserName": "chrome"},  # must be faced in desired_capabilities
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "http://blazedemo.com",
                        "actions": [
                            "waitByXPath(//input[@type='submit'])"]},
                        {"label": "empty"}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target = "'browserName': 'chrome'"
        self.assertIn(target, content)

    def test_build_script_remote_Firefox_browser(self):
        """ check usage of 'browser' scenario options as browserName (from capabilities) """
        self.configure({
            "execution": [{
                "executor": "selenium",
                "remote": "http://addr-of-remote-server.com",
                "scenario": "remote_sc"}],
            "scenarios": {
                "remote_sc": {
                    "browser": "Firefox",  # must be faced in desired_capabilities (in lower case)
                    "timeout": "3.5s",
                    "requests": [{
                        "url": "http://blazedemo.com",
                        "actions": [
                            "waitByXPath(//input[@type='submit'])"]},
                        {"label": "empty"}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target = "'browserName': 'firefox'"
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
                            "waitByXPath(//input[@type='submit'])",
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

    @unittest.skipIf(PY2, "py3 only")
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
            "self.driver.get('http://blazedemo.com/测试')"
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
            "var_loc_keys=self.loc_mng.get_locator([{'name':'btn1',}])",
            "self.driver.find_element(var_loc_keys[0],var_loc_keys[1]).click()",
            "var_loc_keys=self.loc_mng.get_locator([{'id':'Id_123',}])",
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
            "source=self.loc_mng.get_locator([{'xpath':'/xpath/to'}])",
            "target=self.loc_mng.get_locator([{'css':'mycss'},{'id':'ID'}])",
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
                            "name": "Name"
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
                                        "type": "select",
                                        "param": "American Express",
                                        "locators": [
                                            {"css": "myclass"},
                                            {"xpath": "//*[@id=\"cardType\"]"}
                                        ]
                                    },
                                    {
                                        "type": "scriptEval",
                                        "param": "window.scrollTo(0, document.body.scrollHeight);"
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
                                        "type": "wait",
                                        "param": "visible",
                                        "locators": [
                                            {"css": "invalid_css"},
                                            {"name": "inputName"}
                                        ]
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
                                        "type": "closeWindow"
                                    }
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
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "<somewhere>", python_files=True)

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
                                "end": 10
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
            "for i in range(1, 11)",
            "self.vars['i'] = str(i)",
            "self.loc_mng.get_locator([{'id': self.vars['i']"

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
            "for i in range(1, 11, 2)",
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
            "for i in range(10, -1, -1)",
            "self.vars['i'] = str(i)"
        ]
        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))
