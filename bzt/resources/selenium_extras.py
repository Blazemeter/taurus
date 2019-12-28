# Utility functions and classes for Taurus Selenium tests

from selenium.common.exceptions import NoSuchWindowException, NoSuchFrameException, NoSuchElementException
from apiritif import get_transaction_handlers, set_transaction_handlers, get_from_thread_store
from selenium.webdriver.common.by import By


def add_flow_markers():
    handlers = get_transaction_handlers()
    handlers["enter"].append(_send_start_flow_marker)
    handlers["exit"].append(_send_exit_flow_marker)
    set_transaction_handlers(handlers)


def _send_marker(stage, params):
    driver = get_from_thread_store("driver")
    driver.execute_script('/* FLOW_MARKER test-case-%s */' % stage, params)


def _send_start_flow_marker(test_case, test_suite):
    _send_marker('start', {'testCaseName': test_case, 'testSuiteName': test_suite})


def _send_exit_flow_marker(status, message):
    _send_marker('stop', {'status': status, 'message': message})


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
