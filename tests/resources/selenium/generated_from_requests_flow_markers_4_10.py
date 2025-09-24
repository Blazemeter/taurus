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
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.options import ArgOptions
from bzt.resources.selenium_extras import add_flow_markers, get_locator, wait_for, waiter

class TestLocSc(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 3.5
        options = webdriver.ChromeOptions()
        options.add_argument('--no-sandbox')
        options.add_argument('--disable-dev-shm-usage')
        options.add_argument('--disable-gpu')
        options.set_capability('unhandledPromptBehavior', 'ignore')
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
        service = Service(service_args=['--log-path=/somewhere/webdriver.log'])
        self.driver = webdriver.Chrome(service=service, options=options)
        self.driver.implicitly_wait(timeout)
        add_flow_markers()
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={},
                                       scenario_name='loc_sc')

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

    def test_locsc(self):
        self._1_()
        self._2_empty()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
