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
from appium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys

def setup():
    driver = webdriver.Remote(command_executor='http://localhost:4723/wd/hub', desired_capabilities={
        'browserName': 'chrome',
        'deviceName': '',
        'platformName': 'android',
    })
    driver.implicitly_wait(3.5)
    wnd_mng = WindowManager(driver)
    frm_mng = FrameManager(driver)
    vars = {
        
    }
    apiritif.put_into_thread_store(vars, driver, wnd_mng, frm_mng)


def teardown():
    (_, driver, _, _) = apiritif.get_from_thread_store()
    driver.quit()


class TestLocScAppium(unittest.TestCase, ):

    def setUp(self):
        (self.vars, self.driver, self.wnd_mng, self.frm_mng) = apiritif.get_from_thread_store()

    def test_1_(self):
        with apiritif.transaction_logged('/'):
            self.driver.get('http://blazedemo.com/')
            WebDriverWait(self.driver, 3.5).until(econd.presence_of_element_located((By.XPATH, "//input[@type='submit']")), 'Element "//input[@type=\'submit\']" failed to appear within 3.5s')
            self.assertEqual(self.driver.title, 'BlazeDemo')
            body = self.driver.page_source
            re_pattern = re.compile('contained_text')
            self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")

    def test_2_empty(self):
        with apiritif.transaction_logged('empty'):
            pass

from selenium.common.exceptions import NoSuchWindowException, NoSuchFrameException

class FrameManager:

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

class WindowManager:

    def __init__(self, driver):
        self.driver = driver
        self.windows = {
            
        }

    def switch(self, window_name=None):
        try:
            if (not window_name):
                self.driver.switch_to.window(self.driver.window_handles[(-1)])
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
                self.windows[window_name] = self.driver.window_handles[(-1)]
            self.driver.switch_to.window(self.windows[window_name])

    def close(self, window_name=None):
        if window_name:
            self.switch(window_name)
        self.driver.close()
