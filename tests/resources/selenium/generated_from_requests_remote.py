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
from selenium.common.exceptions import NoSuchElementException, TimeoutException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.options import ArgOptions
from bzt.resources.selenium_extras import wait_for, waiter, get_locator, add_flow_markers

class TestLocScRemote(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 3.5
        options = webdriver.FirefoxOptions()
        from selenium.webdriver.remote.remote_connection import RemoteConnection
        import copy
        _original_execute = RemoteConnection.execute

        def execute_with_retries(self, command, params=None):
            params_copy = copy.deepcopy(params)
            retries = 3
            delay = 2
            last_exc = None
            for attempt in range(retries):
                try:
                    if (params != params_copy):
                        return _original_execute(self, command, params_copy)
                    else:
                        return _original_execute(self, command, params)
                except Exception as e:
                    last_exc = e
                    print(f'[Retry] RemoteConnection.execute failed on attempt {(attempt + 1)}: {e}')
                    sleep(delay)
            raise last_exc
        RemoteConnection.execute = execute_with_retries
        options.set_capability('app', '')
        options.set_capability('browserName', 'firefox')
        options.set_capability('deviceName', '')
        options.set_capability('javascriptEnabled', 'True')
        options.set_capability('platformName', 'linux')
        options.set_capability('platformVersion', '')
        options.set_capability('seleniumVersion', '')
        options.set_capability('version', '54.0')
        self.driver = webdriver.Remote(command_executor='http://user:key@remote_web_driver_host:port/wd/hub',
                                       options=options)
        self.driver.implicitly_wait(timeout)
        add_flow_markers()
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={},
                                       scenario_name='loc_sc_remote')

    def _1_(self):
        with apiritif.smart_transaction('/'):
            self.driver.get('http://blazedemo.com/')
            wait_for('present', [{'xpath': "//input[@type='submit']"}], 3.5)
            self.assertEqual(self.driver.title, 'BlazeDemo')
            body = self.driver.page_source
            re_pattern = re.compile('contained_text')
            self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")

    def _2_empty(self):
        with apiritif.smart_transaction('empty'):
            pass

    def test_locscremote(self):
        self._1_()
        self._2_empty()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
