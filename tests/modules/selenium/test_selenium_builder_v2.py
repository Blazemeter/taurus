import os

from bzt import TaurusConfigError
from tests import RESOURCES_DIR
from tests.modules.selenium import SeleniumTestCase


class TestSeleniumScriptGenerationV2(SeleniumTestCase):

    def test_all_multiple_locators_case_insensitive(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "version": 2,
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "type",
                                "param": "text_to_type",
                                "target": {
                                    "locators": [
                                        {"css": "mycss"},
                                        {"CSS": "anothercss"},
                                        {"id": "myid"},
                                        {"Xpath": "/xpath/"},
                                        {"xPatH": "/another/Xpath"},
                                        {"name": "myname"},
                                        {"linkText": "linkText"}
                                    ]
                                }
                            }
                        ]}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        target_lines = [
            "self.driver.find_element(By.CSS_SELECTOR, 'mycss').clear()",
            "self.driver.find_element(By.CSS_SELECTOR, 'mycss').send_keys('text_to_type')",
            "WebDriverWait(self.driver, 0).until(econd.presence_of_element_located((By.CSS_SELECTOR, 'anothercss')), '').clear()",
            "WebDriverWait(self.driver, 0).until(econd.presence_of_element_located((By.ID, 'myid')), '').send_keys('text_to_type')",
            "WebDriverWait(self.driver, 0).until(econd.presence_of_element_located((By.XPATH, '/xpath/')), '').send_keys('text_to_type')",
            "WebDriverWait(self.driver, 0).until(econd.presence_of_element_located((By.XPATH, '/another/Xpath')), '').send_keys('text_to_type')",
            "WebDriverWait(self.driver, 0).until(econd.presence_of_element_located((By.NAME, 'myname')), '').send_keys('text_to_type')",
            "WebDriverWait(self.driver, 0).until(econd.presence_of_element_located((By.NAME, 'myname')), '').send_keys('text_to_type')",
            "WebDriverWait(self.driver, 0).until(econd.presence_of_element_located((By.LINK_TEXT, 'linkText')), '').send_keys('text_to_type')",
        ]

        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_drag_drop(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "version": 2,
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "drag",
                                "source": {
                                    "locators": [
                                        {"xpath": "/xpath/to"}
                                    ]
                                },
                                "target": {
                                    "locators": [
                                        {"css": "mycss"},
                                        {"id": "ID"}
                                    ]
                                }
                            }
                        ]}]}}})

        self.obj.prepare()
        with open(self.obj.script) as fds:
            content = fds.read()

        content = content.replace(" ", "").replace("\t", "").replace("\n", "")

        target_lines = [
            "source=self.loc_mng.get_locator([{'xpath':'/xpath/to',}])",
            "target=self.loc_mng.get_locator([{'css':'mycss',},{'id':'ID',}])"
            "ActionChains(self.driver).drag_and_drop(self.driver.find_element(source[0],source[1]),self.driver.find_element(target[0],target[1])).perform()"
        ]
        for idx in range(len(target_lines)):
            self.assertIn(target_lines[idx], content, msg="\n\n%s. %s" % (idx, target_lines[idx]))

    def test_drag_drop_missing_source(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "version": 2,
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

    def test_missing_action_type(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "version": 2,
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "param": "text_to_type",
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Missing required \'type\' of action' in str(context.exception))

    def test_missing_locators(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "version": 2,
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "select",
                                "param": "text",
                                "target": {
                                    "locators": {}
                                }
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Missing locators for action' in str(context.exception))

    def test_missing_param_assert_store(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "version": 2,
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "assertText",
                                "target": {
                                    "locators": {
                                        "css": "classname"
                                    }
                                }
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Missing param' in str(context.exception))

    def test_missing_param_edit(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "version": 2,
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "editContent",
                                "target": {
                                    "locators": {
                                        "css": "classname"
                                    }
                                }
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Missing param' in str(context.exception))

    def test_unsupported_action(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "version": 2,
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "type": "selectText"
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Unsupported action' in str(context.exception))

    def test_invalid_action_def(self):
        self.configure({
            "execution": [{
                "executor": "apiritif",
                "version": 2,
                "scenario": "loc_sc"}],
            "scenarios": {
                "loc_sc": {
                    "requests": [{
                        "label": "la-la",
                        "actions": [
                            {
                                "whatever"
                            }
                        ]}]}}})

        with self.assertRaises(TaurusConfigError) as context:
            self.obj.prepare()

        self.assertTrue('Unsupported value for action' in str(context.exception))



    def test_build_script(self):
        self.configure(
            {
                "execution": [
                    {
                        "executor": "selenium",
                        "scenario": "loc_sc",
                        "version": 2
                    }
                ],
                "scenarios": {
                    "loc_sc": {
                        "default-address": "http://blazedemo.com,",
                        "variables": {
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
                                        "target": {
                                            "locators": [
                                                {
                                                    "id": "invalid_id"
                                                },
                                                {
                                                    "xpath": "/html/body/div[3]/form/select[1]"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "type": "drag",
                                        "source": {
                                            "locators": [
                                                {
                                                    "name": "invalid_name"
                                                },
                                                {
                                                    "xpath": "/html/body/div[2]/div/p[2]/a"
                                                }
                                            ]
                                        },
                                        "target": {
                                            "locators": [
                                                {
                                                    "css": "invalid_css"
                                                },
                                                {
                                                    "xpath": "/html/body/div[3]/form/div"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "type": "assertText",
                                        "param": "Choose your departure city:",
                                        "target": {
                                            "locators": [
                                                {
                                                    "css": "myclass"
                                                },
                                                {
                                                    "xpath": "/html/body/div[3]/h2"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "type": "assertValue",
                                        "param": "Find Flights",
                                        "target": {
                                            "locators": [
                                                {
                                                    "css": "myclass"
                                                },
                                                {
                                                    "xpath": "/html/body/div[3]/form/div/input"
                                                }
                                            ]
                                        }
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
                                        "param": "Title_Basic_By",
                                        "value": "Final"
                                    },
                                    {
                                        "type": "storeText",
                                        "param": "Basic",
                                        "target": {
                                            "locators": [
                                                {
                                                    "xpath": "/html/body/div[3]/h2"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "type": "click",
                                        "target": {
                                            "locators": [
                                                {
                                                    "xpath": "/wrong/one"
                                                },
                                                {
                                                    "xpath": "/html/body/div[3]/form/div/input"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "type": "keys",
                                        "param": "KEY_ENTER",
                                        "target": {
                                            "locators": [
                                                {
                                                    "xpath": "/doc/abc"
                                                },
                                                {
                                                    "css": "body > div.container > table > tbody > tr:nth-child(1) > td:nth-child(2) > input"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "type": "type",
                                        "param": "Havel Jan",
                                        "target": {
                                            "locators": [
                                                {
                                                    "id": "fjkafjk"
                                                },
                                                {
                                                    "css": "testCss"
                                                },
                                                {
                                                    "CSS": "another"
                                                },
                                                {
                                                    "XPath": "/invalid/xpath"
                                                },
                                                {
                                                    "id": "inputName"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "type": "select",
                                        "param": "American Express",
                                        "target": {
                                            "locators": [
                                                {
                                                    "css": "myclass"
                                                },
                                                {
                                                    "xpath": "//*[@id=\"cardType\"]"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "type": "scriptEval",
                                        "param": "window.scrollTo(0, document.body.scrollHeight);"
                                    },
                                    {
                                        "type": "rawCode",
                                        "param": "for i in range(10):\\n  if i % 2 == 0:\\n    print(i)"
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
                                        "target": {
                                            "locators": [
                                                {
                                                    "css": "invalid_css"
                                                },
                                                {
                                                    "name": "inputName"
                                                }
                                            ]
                                        }
                                    },
                                    {
                                        "type": "editContent",
                                        "param": "lo-la-lu",
                                        "target": {
                                            "locators": [
                                                {
                                                    "id": "editor"
                                                }
                                            ]
                                        }
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
        self.assertFilesEqual(exp_file, self.obj.script, str_to_replace, "<somewhere>", python_files=True)
        with open(self.obj.script) as script:
            self.assertIn("bzt.resources.selenium_extras", script.read())

