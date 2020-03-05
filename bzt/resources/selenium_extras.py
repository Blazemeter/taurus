# Utility functions and classes for Taurus Selenium tests

from selenium.common.exceptions import NoSuchWindowException, NoSuchFrameException, NoSuchElementException
from apiritif import get_transaction_handlers, set_transaction_handlers, get_from_thread_store, get_iteration
from selenium.webdriver.common.by import By


def add_flow_markers():
    handlers = get_transaction_handlers()
    handlers["enter"].append(_send_start_flow_marker)
    handlers["exit"].append(_send_exit_flow_marker)
    set_transaction_handlers(handlers)


def _send_marker(stage, params):
    driver = get_from_thread_store("driver")
    driver.execute_script("/* FLOW_MARKER test-case-%s */" % stage, params)


def _send_start_flow_marker(*args, **kwargs):   # for apiritif. remove when compatibiltiy code in
    stage = "start"                             # apiritif removed (http.py) and apiritif released ( > 0.9.2)

    test_case, test_suite, scenario_name, data_sources = get_from_thread_store(
        ['test_case', 'test_suite', 'scenario_name', 'data_sources']
    )
    params = {
        "testCaseName": test_case,
        "testSuiteName": scenario_name or test_suite}

    if data_sources:
        params["testDataIterationId"] = get_iteration()

    _send_marker(stage, params)


def _send_exit_flow_marker(*args, **kwargs):   # for apiritif. remove when compatibiltiy code in
    stage = "stop"                             # apiritif removed (http.py) and apiritif released ( > 0.9.2)
    labels = "status", "message"
    values = get_from_thread_store(labels)
    params = dict(zip(labels, values))
    _send_marker(stage, params)


class FrameManager:
    def __init__(self, driver):
        self.driver = driver

    def switch(self, frame_name=None):
        try:
            if not frame_name or frame_name == "relative=top":
                self.driver.switch_to_default_content()
            elif frame_name.startswith("index="):  # Switch using index frame using relative position
                self.driver.switch_to.frame(int(frame_name.split("=")[1]))
            elif frame_name == "relative=parent":  # Switch to parent frame of the current frame
                self.driver.switch_to.parent_frame()
            else:  # Use the selenium alternative
                self.driver.switch_to.frame(frame_name)
        except NoSuchFrameException:
            raise NoSuchFrameException("Invalid Frame ID: %s" % frame_name)


class WindowManager:
    def __init__(self, driver):
        self.driver = driver
        self.windows = {}

    def switch(self, window_name=None):
        try:
            if not window_name:  # Switch to last window created
                self.driver.switch_to.window(self.driver.window_handles[-1])
            else:
                if window_name.isdigit():  # Switch to window handler index
                    self._switch_by_idx(int(window_name))
                else:
                    if window_name.startswith("win_ser_"):  # Switch using window sequential mode
                        self._switch_by_win_ser(window_name)
                    else:  # Switch using window name
                        self.driver.switch_to.window(window_name)
        except NoSuchWindowException:
            raise NoSuchWindowException("Invalid Window ID: %s" % window_name)

    def _switch_by_idx(self, win_index):
        wnd_handlers = self.driver.window_handles
        if len(wnd_handlers) <= win_index and win_index >= 0:
            self.driver.switch_to.window(wnd_handlers[win_index])
        else:
            raise NoSuchWindowException("Invalid Window ID: %s" % str(win_index))

    def _switch_by_win_ser(self, window_name):
        if window_name == "win_ser_local":
            wnd_handlers = self.driver.window_handles
            if len(wnd_handlers) > 0:
                self.driver.switch_to.window(wnd_handlers[0])
            else:
                raise NoSuchWindowException("Invalid Window ID: %s" % window_name)
        else:
            if window_name not in self.windows:
                self.windows[window_name] = self.driver.window_handles[-1]
            self.driver.switch_to.window(self.windows[window_name])

    def close(self, window_name=None):
        if window_name:
            self.switch(window_name)
        self.driver.close()


class LocatorsManager:
    BYS = {
        'xpath': By.XPATH,
        'css': By.CSS_SELECTOR,
        'name': By.NAME,
        'id': By.ID,
        'linktext': By.LINK_TEXT
    }

    def __init__(self, driver, timeout=30):
        self.driver = driver
        self.timeout = timeout

    def get_locator(self, locators):
        """
        :param locators: List of Dictionaries holding the locators, e.g. [{'id': 'elem_id'},
        {css: 'my_cls'}]
        :return: first valid locator from the passed List, if no locator is valid then returns the
        first one
        """
        first_locator = None
        for locator in locators:
            locator_type = list(locator.keys())[0]
            locator_value = locator[locator_type]
            if not first_locator:
                first_locator = (self.BYS[locator_type.lower()], locator_value)
            else:
                # set implicit wait to 0 get the result instantly for the other locators
                self.driver.implicitly_wait(0)
            elements = self.driver.find_elements(self.BYS[locator_type.lower()], locator_value)
            if len(elements) > 0:
                locator = (self.BYS[locator_type.lower()], locator_value)
                break
        else:
            self.driver.implicitly_wait(self.timeout)
            msg = "Element not found: (%s, %s)" % first_locator
            raise NoSuchElementException(msg)

        # restore the implicit wait value
        self.driver.implicitly_wait(self.timeout)
        return locator


class DialogsManager:
    """
    Provides additional methods for working with Dialogs that are not available in the Python Webdriver.
    These JavaScript functions are taken from the Java Selenium Webdriver repository
    """

    def __init__(self, driver):
        self.driver = driver

    def replace_alerts(self):
        self.driver.execute_script("""
          if (window.__webdriverAlerts) { return; }
          window.__webdriverAlerts = [];
          window.alert = function(msg) { window.__webdriverAlerts.push(msg); };
          window.__webdriverConfirms = [];
          window.__webdriverNextConfirm = true;
          window.confirm = function(msg) {
            window.__webdriverConfirms.push(msg);
            var res = window.__webdriverNextConfirm;
            window.__webdriverNextConfirm = true;
            return res;
          };
          window.__webdriverPrompts = [];
          window.__webdriverNextPrompts = true;
          window.prompt = function(msg, def) {
            window.__webdriverPrompts.push(msg || def);
            var res = window.__webdriverNextPrompt;
            window.__webdriverNextPrompt = true;
            return res;
          };
        """)

    def get_next_confirm(self):
        return self.driver.execute_script("""
                 if (!window.__webdriverConfirms) { return null; }
                 return window.__webdriverConfirms.shift();
               """)

    def get_next_alert(self):
        return self.driver.execute_script("""
                if (!window.__webdriverAlerts) { return null } 
                var t = window.__webdriverAlerts.shift(); 
                if (t) { t = t.replace(/\\n/g, ' '); }
                return t;
              """)

    def get_next_prompt(self):
        return self.driver.execute_script("""
                if (!window.__webdriverPrompts) { return null; }
                return window.__webdriverPrompts.shift();
              """)

    def answer_on_next_prompt(self, value):
        if str(value).lower() == '#cancel':
            self.driver.execute_script("window.__webdriverNextPrompt = null")
        else:
            self.driver.execute_script("window.__webdriverNextPrompt = '%s';" % value)

    def set_next_confirm_state(self, value):
        if str(value).lower() == '#ok':
            confirm = 'true'
        else:
            confirm = 'false'
        self.driver.execute_script("window.__webdriverNextConfirm = %s;" % confirm)
