# coding=utf-8
import unittest
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

class TestRequests(unittest.TestCase):
    def setUp(self):
        self.vars = {}
        self.template = Template(self.vars)
        
        self.driver = webdriver.Remote(command_executor='http://localhost:4723/wd/hub', desired_capabilities={"browserName": "Chrome", "deviceName": "", "platformName": "Android"})
        self.driver.implicitly_wait(3.5)
        self.wnd_mng = WindowManager(self.driver)
        self.frm_mng = FrameManager(self.driver)
        self.driver.maximize_window()

    def tearDown(self):
        self.driver.quit()

    def test_requests(self):
        self.driver.implicitly_wait(3.5)

        with apiritif.transaction_logged(self.template('/')):
            self.driver.get(self.template('http://blazedemo.com/'))

            WebDriverWait(self.driver, 3.5).until(econd.presence_of_element_located((By.XPATH, self.template("//input[@type='submit']"))), 'Element "//input[@type=\'submit\']" failed to appear within 3.5s')
            self.assertEqual(self.driver.title, self.template('BlazeDemo'))

            body = self.driver.page_source
            re_pattern = re.compile(r'contained_text')
            self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")


        with apiritif.transaction_logged(self.template('empty')):
            pass


# Utility functions and classes for Taurus Selenium tests

from string import Template as StrTemplate
from selenium.common.exceptions import NoSuchWindowException, NoSuchFrameException


class Template:
    def __init__(self, variables):
        self.variables = variables

    def apply(self, template):
        tmpl = StrTemplate(b''.decode() + template)
        return tmpl.safe_substitute(self.variables)

    __call__ = apply

    @staticmethod
    def str_repr(text):
        return repr(text)[1:] if repr(text)[0] == "u" else repr(text)


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

