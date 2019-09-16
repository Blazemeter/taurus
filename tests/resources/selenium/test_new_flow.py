# coding=utf-8

import logging
import random
import string
import sys
import unittest
from time import time, sleep

import apiritif

import os
import re
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys


def flow_control(test_case="", test_suite=""):
    def decorator(func):
        def wrap(self):
            def send_marker(stage, params):
                if self.flow_markers:
                    self.driver.execute_script('/* FLOW_MARKER test-case-%s */' % stage, params)

            message = ''
            status = 'success'
            send_marker('start', {'testCaseName': test_case, 'testSuiteName': test_suite})
            try:
                func(self)
            except BaseException as exc:
                message = str(exc)
                if isinstance(exc, AssertionError):
                    status = 'failed'
                else:
                    status = 'broken'

            send_marker('stop', {'status': status, 'message': message})
            if message and self.func_mode:
                raise

        return wrap

    return decorator


def setup():
    options = webdriver.ChromeOptions()
    driver = webdriver.Chrome(service_log_path='webdriver.log',
                              chrome_options=options)
    driver.implicitly_wait(60.0)
    wnd_mng = WindowManager(driver)
    frm_mng = FrameManager(driver)
    func_mode = False
    flow_markers = True
    vars = {

    }
    apiritif.put_into_thread_store(vars, driver, wnd_mng, frm_mng, func_mode, flow_markers)


def teardown():
    driver = apiritif.get_from_thread_store()[1]
    driver.quit()


class TestSdsdsdsSelenium(unittest.TestCase):

    def setUp(self):
        (self.vars, self.driver, self.wnd_mng, self.frm_mng, self.func_mode, self.flow_markers) = \
            apiritif.get_from_thread_store()

    @flow_control("t1", "sdsdsds-Selenium")
    def t1(self):
        with apiritif.transaction_logged('t1'):
            self.driver.get('http://blazedemo.com/purchase.php')
            self.driver.find_element(By.CSS_SELECTOR, 'input.btn.btn-primary').click()

    @flow_control("t2", "sdsdsds-Selenium")
    def t2(self):
        with apiritif.transaction_logged('t2'):
            self.driver.get('https://www.belarus.by/en/')
            body = self.driver.page_source
            re_pattern = re.compile('In God we trust')
            self.assertNotEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'In God we trust' not found in BODY")

    @flow_control("t3", "sdsdsds-Selenium")
    def t3(self):
        with apiritif.transaction_logged('t3'):
            self.driver.get('some.strange.url')

    def test_all(self):
        self.t1()
        self.t2()
        self.t3()


from selenium.common.exceptions import NoSuchWindowException, NoSuchFrameException


class FrameManager():

    def __init__(self, driver):
        self.driver = driver

    def switch(self, frame_name=None):
        try:
            if ((not frame_name) or (frame_name == 'relative=top')):
                self.driver.switch_to_default_content()
            elif frame_name.startswith('index='):
                self.driver.switch_to.frame(int(frame_name.split('=')[1]))
            elif (frame_name == 'relative=parent'):
                self.driver.switch_to.parent_frame()
            else:
                self.driver.switch_to.frame(frame_name)
        except NoSuchFrameException:
            raise NoSuchFrameException(('Invalid Frame ID: %s' % frame_name))


class WindowManager():

    def __init__(self, driver):
        self.driver = driver
        self.windows = {

        }

    def switch(self, window_name=None):
        try:
            if (not window_name):
                self.driver.switch_to.window(self.driver.window_handles[(- 1)])
            elif window_name.isdigit():
                self._switch_by_idx(int(window_name))
            elif window_name.startswith('win_ser_'):
                self._switch_by_win_ser(window_name)
            else:
                self.driver.switch_to.window(window_name)
        except NoSuchWindowException:
            raise NoSuchWindowException(('Invalid Window ID: %s' % window_name))

    def _switch_by_idx(self, win_index):
        wnd_handlers = self.driver.window_handles
        if ((len(wnd_handlers) <= win_index) and (win_index >= 0)):
            self.driver.switch_to.window(wnd_handlers[win_index])
        else:
            raise NoSuchWindowException(('Invalid Window ID: %s' % str(win_index)))

    def _switch_by_win_ser(self, window_name):
        if (window_name == 'win_ser_local'):
            wnd_handlers = self.driver.window_handles
            if (len(wnd_handlers) > 0):
                self.driver.switch_to.window(wnd_handlers[0])
            else:
                raise NoSuchWindowException(('Invalid Window ID: %s' % window_name))
        else:
            if (window_name not in self.windows):
                self.windows[window_name] = self.driver.window_handles[(- 1)]
            self.driver.switch_to.window(self.windows[window_name])

    def close(self, window_name=None):
        if window_name:
            self.switch(window_name)
        self.driver.close()
