import os

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
                            "closeWindow()",
                            "closeWindow('win_ser_local')",

                            # frames
                            "switchFrameByIdx(1)",
                            "switchFrame(relative=parent)",
                            "switchFrameByName('my_frame')",

                            # chains
                            "mouseDownByXPath(/html/body/div[3]/form/select[1])",

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

        locator0 = "self.driver.find_element(By.NAME, 'my_frame')"
        locator1 = "self.driver.find_element(By.XPATH, '/html/body/div[3]/form/select[1]')"
        locator2_1 = "self.driver.find_element(By.ID, 'address')"
        locator2_2 = "self.driver.find_element(By.NAME, 'toPort')"

        msg = '"The element (By.%s, %r) is not contenteditable element"' % ('ID', 'editor')
        no_such_elt = "raise NoSuchElementException(%s)" % msg

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
            "self.frm_mng.switch(%s)" % locator0,
            "ActionChains(self.driver).click_and_hold(%s).perform()" % locator1,
            "ActionChains(self.driver).drag_and_drop(%s, %s).perform()" % (locator2_1, locator2_2),
            "Select(%s).select_by_visible_text('London')" % locator0,
            "self.assertEqual(self.driver.title, 'BlazeDemo')",
            "self.vars['hEaDeR'] = self.driver.title",
            "self.vars['Final'] = 'Title_Basic_By'",
            "self.vars['Basic'] = %s.get_attribute('innerText')" % locator2_1,
            "self.assertEqual(self.driver.find_element(By.ID, 'address')."
            "get_attribute('value').strip(), '123 Beautiful st.'.strip())",
            "self.driver.find_element(By.NAME, 'toPort').clear()",
            "self.driver.find_element(By.NAME, 'toPort').send_keys('B')",
            "self.driver.execute_script(\"alert('This is Sparta');\")",
            "for i in range(10):",
            "if ((i % 2) == 0):",
            print_i,
            "self.driver.get('http:\\\\blazemeter.com')",
            "if self.driver.find_element(By.ID, 'editor').get_attribute('contenteditable'):",
            "self.driver.execute_script(('arguments[0].innerHTML = %s;' % 'lo-la-lu'), "
            "self.driver.find_element(By.ID, 'editor'))",
            "else:",
            no_such_elt,
            "print(self.vars['red_pill'])",
            "WebDriverWait(self.driver, 3.5).until(econd.visibility_of_element_located((By.NAME, "
            "'toPort')), \"Element 'toPort' failed to appear within 3.5s\")",
            "sleep(4.6)",
            "self.driver.delete_all_cookies()",
            "self.driver.save_screenshot('screen.png')",
            "filename = os.path.join(os.getenv('TAURUS_ARTIFACTS_DIR'), ('screenshot-%d.png' % (time() * 1000)))",
            "self.driver.save_screenshot(filename)"
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

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

        self.obj.prepare()
        exp_file = RESOURCES_DIR + "selenium/generated_from_requests.py"
        str_to_replace = (self.obj.engine.artifacts_dir + os.path.sep).replace('\\', '\\\\')
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "<somewhere>", python_files=True)

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
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "<somewhere>", python_files=True)
