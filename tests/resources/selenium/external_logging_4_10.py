# coding=utf-8

import logging
import random
import string
import sys
import unittest
from time import time, sleep

import apiritif
import traceback
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
from bzt.resources.selenium_extras import get_locator, action_start, waiter, action_end

class TestSample(unittest.TestCase):

    def setUp(self):
        self.driver = None
        action_start({'actionId': None, 'param': {}, 'type': 'new_session', 'value': None})
        try:
            self.vars = {}
            timeout = 30.0
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
            apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={},
                                           scenario_name='sample')
        except Exception:
            (ex_type, ex, tb) = sys.exc_info()
            action_end({'message': str(traceback.format_exception(ex_type, ex, tb)), 'param': {}, 'type': 'new_session'})
            apiritif.log.error(str(traceback.format_exception(ex_type, ex, tb)))
            raise
        action_end({'param': {}, 'type': 'new_session'})


    def _1_Test(self):
        with apiritif.smart_transaction('Test'):
            action_start({'actionId': None, 'param': 'http://blazedemo.com/', 'selectors': [], 'tag': '', 'type': 'go', 'value': None})
            self.driver.get('http://blazedemo.com/')

            waiter()
            action_end({'actionId': None, 'param': 'http://blazedemo.com/', 'selectors': [], 'tag': '', 'type': 'go', 'value': None})
            action_start({'actionId': None, 'param': 'leaving blazedemo', 'selectors': [], 'tag': '', 'type': 'log', 'value': None})
            action_end({'actionId': None, 'param': 'leaving blazedemo', 'selectors': [], 'tag': '', 'type': 'log', 'value': None})

    def test_sample(self):
        self._1_Test()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
