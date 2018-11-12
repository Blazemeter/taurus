# coding=utf-8
import unittest
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
from bzt.resources import selenium_taurus_extras

from string import Template as StrTemplate
from selenium.common.exceptions import NoSuchWindowException, NoSuchFrameException


# Utility functions and classes for Selenium tests
class Apply(StrTemplate):
    def __init__(self, template):
        super(Apply, self).__init__(template)
        self.variables = {}

    def __repr__(self):
        return repr(self.safe_substitute(self.variables))

    def __str__(self):
        return self.safe_substitute(self.variables)


class Template:
    def __init__(self, variables):
        self.variables = variables
        self.tmpl = Apply("")

    def apply(self, template):
        self.tmpl.template = template
        self.tmpl.variables = self.variables
        return str(self.tmpl)

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

_vars = {}
_tpl = selenium_taurus_extras.Template(_vars)

class TestRequests(unittest.TestCase):
    def setUp(self):
        options = webdriver.ChromeOptions()
        self.driver = webdriver.Chrome(service_log_path='<somewhere>webdriver.log', chrome_options=options)
        self.driver.implicitly_wait(3.5)
        self.wnd_mng = selenium_taurus_extras.WindowManager(self.driver)
        self.frm_mng = selenium_taurus_extras.FrameManager(self.driver)

    def tearDown(self):
        self.driver.quit()

    def test_requests(self):
        self.driver.implicitly_wait(3.5)

        try:
            self.driver.execute_script('/* FLOW_MARKER test-case-start */', {'testCaseName': '/', 'testSuiteName': 'loc_sc'})

            with apiritif.transaction_logged('/'):
                self.driver.get('http://blazedemo.com/')

                WebDriverWait(self.driver, 3.5).until(econd.presence_of_element_located((By.XPATH, _tpl.apply("//input[@type='submit']"))), 'Element "//input[@type=\'submit\']" failed to appear within 3.5s')
                self.assertEqual(self.driver.title, _tpl.apply('BlazeDemo'))

                body = self.driver.page_source
                re_pattern = re.compile(r'contained_text')
                self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")

        except AssertionError as exc:
            self.driver.execute_script('/* FLOW_MARKER test-case-stop */', {'status': 'failed', 'message': str(exc)})
            raise
        except BaseException as exc:
            self.driver.execute_script('/* FLOW_MARKER test-case-stop */', {'status': 'broken', 'message': str(exc)})
            raise
        else:
            self.driver.execute_script('/* FLOW_MARKER test-case-stop */', {'status': 'success', 'message': ''})

        try:
            self.driver.execute_script('/* FLOW_MARKER test-case-start */', {'testCaseName': 'empty', 'testSuiteName': 'loc_sc'})

            with apiritif.transaction_logged('empty'):
                pass

        except AssertionError as exc:
            self.driver.execute_script('/* FLOW_MARKER test-case-stop */', {'status': 'failed', 'message': str(exc)})
            raise
        except BaseException as exc:
            self.driver.execute_script('/* FLOW_MARKER test-case-stop */', {'status': 'broken', 'message': str(exc)})
            raise
        else:
            self.driver.execute_script('/* FLOW_MARKER test-case-stop */', {'status': 'success', 'message': ''})
