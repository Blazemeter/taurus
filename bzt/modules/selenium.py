"""
Copyright 2015 BlazeMeter Inc.

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
import time

import os
from bzt import TaurusConfigError, TaurusInternalException
from urwid import Text, Pile

from bzt.engine import Scenario, FileLister, AbstractSeleniumExecutor
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, PrioritizedWidget
from bzt.modules.functional import FunctionalResultsReader, FunctionalAggregator, FunctionalSample
from bzt.modules.java import JUnitTester, TestNGTester
from bzt.modules.javascript import MochaTester
from bzt.modules.python import NoseTester
from bzt.modules.ruby import RSpecTester
from bzt.six import string_types, parse, iteritems
from bzt.utils import PythonGenerator
from bzt.utils import dehumanize_time, is_windows, BetterDict, get_full_path, get_files_recursive

try:
    from pyvirtualdisplay.smartdisplay import SmartDisplay as Display
except ImportError:
    from pyvirtualdisplay import Display


class SeleniumExecutor(AbstractSeleniumExecutor, WidgetProvider, FileLister):
    """
    Selenium executor
    :type virtual_display: Display
    :type runner: AbstractSeleniumExecutor
    """

    SUPPORTED_RUNNERS = ["nose", "junit", "testng", "rspec", "mocha"]

    def __init__(self):
        super(SeleniumExecutor, self).__init__()
        self.additional_env = {}
        self.virtual_display = None
        self.end_time = None
        self.runner = None
        self.report_file = None
        self.scenario = None
        self.script = None
        self.self_generated_script = False
        self.generated_methods = BetterDict()
        self.runner_working_dir = None
        self.register_reader = True

    def get_virtual_display(self):
        return self.virtual_display

    def add_env(self, env):
        self.additional_env.update(env)

    def set_virtual_display(self):
        display_conf = self.settings.get("virtual-display")
        if display_conf:
            if is_windows():
                self.log.warning("Cannot have virtual display on Windows, ignoring")
            else:
                if self.engine in SeleniumExecutor.SHARED_VIRTUAL_DISPLAY:
                    self.virtual_display = SeleniumExecutor.SHARED_VIRTUAL_DISPLAY[self.engine]
                else:
                    width = display_conf.get("width", 1024)
                    height = display_conf.get("height", 768)
                    self.virtual_display = Display(size=(width, height))
                    msg = "Starting virtual display[%s]: %s"
                    self.log.info(msg, self.virtual_display.size, self.virtual_display.new_display_var)
                    self.virtual_display.start()
                    SeleniumExecutor.SHARED_VIRTUAL_DISPLAY[self.engine] = self.virtual_display

    def free_virtual_display(self):
        if self.virtual_display and self.virtual_display.is_alive():
            self.virtual_display.stop()
        if self.engine in SeleniumExecutor.SHARED_VIRTUAL_DISPLAY:
            del SeleniumExecutor.SHARED_VIRTUAL_DISPLAY[self.engine]

    def get_runner_working_dir(self):
        if self.runner_working_dir is None:
            self.runner_working_dir = self.engine.create_artifact("classes", "")
        return self.runner_working_dir

    def _get_testng_xml(self):
        if 'testng-xml' in self.scenario:
            testng_xml = self.scenario.get('testng-xml')
            if testng_xml:
                return testng_xml
            else:
                return None  # empty value for switch off testng.xml path autodetect

        script_path = self.get_script_path()
        if script_path:
            script_dir = get_full_path(script_path, step_up=1)
            testng_xml = os.path.join(script_dir, 'testng.xml')
            if os.path.exists(testng_xml):
                self.log.info("Detected testng.xml file at %s", testng_xml)
                self.scenario['testng-xml'] = testng_xml
                return testng_xml

        return None

    def _create_runner(self, report_file):
        script_type = self.detect_script_type()

        runner_config = BetterDict()

        if script_type == "nose":
            runner_class = NoseTester
            runner_config.merge(self.settings.get("selenium-tools").get("nose"))
        elif script_type == "junit":
            runner_class = JUnitTester
            runner_config.merge(self.settings.get("selenium-tools").get("junit"))
            runner_config['working-dir'] = self.get_runner_working_dir()
            runner_config['props-file'] = self.engine.create_artifact("runner", ".properties")
        elif script_type == "testng":
            runner_class = TestNGTester
            runner_config.merge(self.settings.get("selenium-tools").get("testng"))
            runner_config['working-dir'] = self.get_runner_working_dir()
            runner_config['props-file'] = self.engine.create_artifact("runner", ".properties")
            testng_config = self._get_testng_xml()
            if testng_config:
                runner_config['testng-xml'] = self.engine.find_file(testng_config)
        elif script_type == "rspec":
            runner_class = RSpecTester
            runner_config.merge(self.settings.get("selenium-tools").get("rspec"))
        elif script_type == "mocha":
            runner_class = MochaTester
            runner_config.merge(self.settings.get("selenium-tools").get("mocha"))
        else:
            raise TaurusConfigError("Unsupported script type: %s" % script_type)

        runner_config["script"] = self.script
        runner_config["script-type"] = script_type
        runner_config["artifacts-dir"] = self.engine.artifacts_dir
        runner_config["report-file"] = report_file
        runner_config["stdout"] = self.engine.create_artifact("selenium", ".out")
        runner_config["stderr"] = self.engine.create_artifact("selenium", ".err")
        return runner_class(runner_config, self)

    def _register_reader(self, report_file):
        if self.engine.is_functional_mode():
            reader = FuncSamplesReader(report_file, self.log, self.generated_methods)
            if isinstance(self.engine.aggregator, FunctionalAggregator):
                self.engine.aggregator.add_underling(reader)
        else:
            reader = LoadSamplesReader(report_file, self.log, self.generated_methods)
            if isinstance(self.engine.aggregator, ConsolidatingAggregator):
                self.engine.aggregator.add_underling(reader)
        return reader

    def prepare(self):
        if self.get_load().concurrency and self.get_load().concurrency > 1:
            msg = 'Selenium supports concurrency in cloud provisioning mode only\n'
            msg += 'For details look at http://gettaurus.org/docs/Cloud.md'
            self.log.warning(msg)
        self.set_virtual_display()
        self.scenario = self.get_scenario()
        self.__setup_script()

        self.report_file = self.engine.create_artifact("selenium_tests_report", ".ldjson")
        self.runner = self._create_runner(self.report_file)

        self.runner.prepare()
        if self.register_reader:
            self.reader = self._register_reader(self.report_file)

    def __setup_script(self):
        self.script = self.get_script_path()
        if not self.script:
            if "requests" in self.scenario:
                self.script = self.__tests_from_requests()
                self.self_generated_script = True
            else:
                raise TaurusConfigError("Nothing to test, no requests were provided in scenario")

    def detect_script_type(self):
        if not os.path.exists(self.script):
            raise TaurusConfigError("Script '%s' doesn't exist" % self.script)

        if "runner" in self.execution:
            runner = self.execution["runner"]
            if runner not in SeleniumExecutor.SUPPORTED_RUNNERS:
                msg = "Runner '%s' is not supported. Supported runners: %s"
                raise TaurusConfigError(msg % (runner, SeleniumExecutor.SUPPORTED_RUNNERS))
            self.log.debug("Using script type: %s", runner)
            return runner

        file_types = set()

        if os.path.isfile(self.script):  # regular file received
            file_types.add(os.path.splitext(self.script)[1].lower())
        else:  # dir received: check contained files
            for file_name in get_files_recursive(self.script):
                file_types.add(os.path.splitext(file_name)[1].lower())

        if '.java' in file_types or '.jar' in file_types:
            if self._get_testng_xml() is not None:
                script_type = 'testng'
            else:
                script_type = 'junit'
        elif '.py' in file_types:
            script_type = 'nose'
        elif '.rb' in file_types:
            script_type = 'rspec'
        elif '.js' in file_types:
            script_type = 'mocha'
        else:
            raise TaurusConfigError("Unsupported script type: %s" % self.script)

        self.log.debug("Detected script type: %s", script_type)

        return script_type

    def startup(self):
        """
        Start runner
        :return:
        """
        self.start_time = time.time()
        self.runner.env = self.additional_env
        self.runner.startup()

    def check_virtual_display(self):
        if self.virtual_display:
            if not self.virtual_display.is_alive():
                self.log.info("Virtual display out: %s", self.virtual_display.stdout)
                self.log.warning("Virtual display err: %s", self.virtual_display.stderr)
                raise TaurusInternalException("Virtual display failed: %s" % self.virtual_display.return_code)

    def check(self):
        """
        check if test completed
        :return:
        """
        if self.widget:
            self.widget.update()

        self.check_virtual_display()

        return self.runner.check()

    def report_test_duration(self):
        if self.start_time:
            self.end_time = time.time()
            self.log.debug("Selenium tests ran for %s seconds", self.end_time - self.start_time)

    def shutdown(self):
        """
        shutdown test_runner
        :return:
        """
        self.runner.shutdown()
        self.report_test_duration()

    def post_process(self):
        if os.path.exists("geckodriver.log"):
            self.engine.existing_artifact("geckodriver.log", True)
        self.free_virtual_display()

    def has_results(self):
        if self.reader and self.reader.read_records:
            return True
        else:
            return False

    def get_widget(self):
        if not self.widget:
            self.widget = SeleniumWidget(self.script, self.runner.settings.get("stdout"))
        return self.widget

    def resource_files(self):
        resources = []

        self.scenario = self.get_scenario()
        script = self.scenario.get(Scenario.SCRIPT, None)
        if script:
            resources.append(script)

        resources.extend(self.scenario.get("additional-classpath", []))
        resources.extend(self.settings.get("additional-classpath", []))

        testng_config = self._get_testng_xml()
        if testng_config:
            resources.append(testng_config)

        return resources

    def __tests_from_requests(self):
        filename = self.engine.create_artifact("test_requests", ".py")
        wdlog = self.engine.create_artifact('webdriver', '.log')
        nose_test = SeleniumScriptBuilder(self.scenario, self.log, wdlog)
        if self.virtual_display:
            nose_test.window_size = self.virtual_display.size
        self.generated_methods.merge(nose_test.build_source_code())
        nose_test.save(filename)
        return filename

    def install_required_tools(self):
        self.scenario = BetterDict()
        runner_classes = {
            "nose": NoseTester,
            "junit": JUnitTester,
            "testng": TestNGTester,
            "rspec": RSpecTester,
            "mocha": MochaTester,
        }
        for runner, runner_class in iteritems(runner_classes):
            runner_config = BetterDict()
            runner_config.merge(self.settings.get("selenium-tools").get(runner))
            runner_config.merge({"script": ''})
            mod = runner_class(runner_config, self)
            mod.run_checklist()


class SeleniumWidget(Pile, PrioritizedWidget):
    def __init__(self, script, runner_output):
        widgets = []
        self.script_name = Text("Selenium: %s" % os.path.basename(script))
        self.summary_stats = Text("Delayed...")
        self.runner_output = runner_output
        widgets.append(self.script_name)
        widgets.append(self.summary_stats)
        super(SeleniumWidget, self).__init__(widgets)
        PrioritizedWidget.__init__(self, priority=10)

    def update(self):
        reader_summary = ''
        if os.path.exists(self.runner_output):
            with open(self.runner_output, "rt") as fds:
                lines = fds.readlines()
                if lines:
                    line = lines[-1]
                    if not line.endswith("\n") and len(lines) > 1:
                        line = lines[-2]
                    if line and "," in line:
                        reader_summary = line.split(",")[-1]

        if reader_summary:
            self.summary_stats.set_text(reader_summary)
        else:
            self.summary_stats.set_text('In progress...')

        self._invalidate()


class SeleniumScriptBuilder(PythonGenerator):
    """
    :type window_size: tuple(int,int)
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
        self.window_size = (1, 1)
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
            statement = self.gen_statement("cls.driver.set_window_size(%s, %s)" % self.window_size)
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


class LDJSONReader(object):
    def __init__(self, filename, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.filename = filename
        self.fds = None
        self.partial_buffer = ""
        self.offset = 0

    def read(self, last_pass=False):
        if not self.fds and not self.__open_fds():
            self.log.debug("No data to start reading yet")
            return

        self.fds.seek(self.offset)
        if last_pass:
            lines = self.fds.readlines()  # unlimited
        else:
            lines = self.fds.readlines(1024 * 1024)
        self.offset = self.fds.tell()

        for line in lines:
            if not line.endswith("\n"):
                self.partial_buffer += line
                continue
            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""
            yield json.loads(line)

    def __open_fds(self):
        if not os.path.isfile(self.filename):
            return False
        fsize = os.path.getsize(self.filename)
        if not fsize:
            return False
        self.fds = open(self.filename, 'rt', buffering=1)
        return True

    def __del__(self):
        if self.fds is not None:
            self.fds.close()


class SeleniumReportReader(object):
    REPORT_ITEM_KEYS = ["test_case", "test_suite", "status", "start_time", "duration",
                        "error_msg", "error_trace", "extras"]
    TEST_STATUSES = ("PASSED", "FAILED", "BROKEN", "SKIPPED")
    FAILING_TESTS_STATUSES = ("FAILED", "BROKEN")

    def __init__(self, filename, parent_logger, translation_table=None):
        super(SeleniumReportReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.json_reader = LDJSONReader(filename, self.log)
        self.translation_table = translation_table or {}

    def process_label(self, label):
        if label in self.translation_table:
            return self.translation_table[label]

        if isinstance(label, string_types):
            if label.startswith('test_') and label[5:10].isdigit():
                return label[11:]

        return label

    def read(self, last_pass=False):
        for row in self.json_reader.read(last_pass):
            for key in self.REPORT_ITEM_KEYS:
                if key not in row:
                    self.log.debug("Unexpected test record: %s", row)
                    self.log.warning("Test record doesn't conform to schema, skipping, %s", key)
                    continue

            row["test_case"] = self.process_label(row["test_case"])
            yield row


class LoadSamplesReader(ResultsReader):
    STATUS_TO_CODE = {
        "PASSED": "200",
        "SKIPPED": "300",
        "FAILED": "400",
        "BROKEN": "500",
    }

    def __init__(self, filename, parent_logger, translation_table):
        super(LoadSamplesReader, self).__init__()
        self.report_reader = SeleniumReportReader(filename, parent_logger, translation_table)
        self.read_records = 0

    def extract_sample(self, item):
        tstmp = int(item["start_time"])
        label = item["test_case"]
        concur = 1
        rtm = item["duration"]
        cnn = 0
        ltc = 0
        rcd = self.STATUS_TO_CODE.get(item["status"], "UNKNOWN")
        error = item["error_msg"] if item["status"] in SeleniumReportReader.FAILING_TESTS_STATUSES else None
        trname = ""
        byte_count = None
        return tstmp, label, concur, rtm, cnn, ltc, rcd, error, trname, byte_count

    def _read(self, last_pass=False):
        for row in self.report_reader.read(last_pass):
            self.read_records += 1
            sample = self.extract_sample(row)
            yield sample


class FuncSamplesReader(FunctionalResultsReader):
    def __init__(self, filename, parent_logger, translation_table):
        self.report_reader = SeleniumReportReader(filename, parent_logger, translation_table)
        self.read_records = 0

    def read(self, last_pass=False):
        for row in self.report_reader.read(last_pass):
            self.read_records += 1
            sample = FunctionalSample(test_case=row["test_case"], test_suite=row["test_suite"],
                                      status=row["status"], start_time=row["start_time"], duration=row["duration"],
                                      error_msg=row["error_msg"], error_trace=row["error_trace"],
                                      extras=row.get("extras", {}))
            yield sample
