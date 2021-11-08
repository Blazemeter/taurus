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
from selenium.webdriver.common.options import ArgOptions
from bzt.resources.selenium_extras import waiter, action_end, get_locator, action_start

class TestSample(unittest.TestCase):

    def setUp(self):
        self.driver = None
        action_start({'param': {}, 'type': 'new_session', 'value': None})
        try:
            self.vars = {}
            timeout = 30.0
            options = webdriver.ChromeOptions()
            options.add_argument('--no-sandbox')
            options.add_argument('--disable-dev-shm-usage')
            options.add_argument('--disable-gpu')
            options.set_capability('unhandledPromptBehavior', 'ignore')
            self.driver = webdriver.Chrome(service_log_path='/somewhere/webdriver.log', options=options)
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
            action_start({'param': 'http://blazedemo.com/', 'selectors': [], 'tag': '', 'type': 'go', 'value': None})
            self.driver.get('http://blazedemo.com/')

            waiter()
            action_end({'param': 'http://blazedemo.com/', 'selectors': [], 'tag': '', 'type': 'go', 'value': None})
            action_start({'param': 'leaving blazedemo', 'selectors': [], 'tag': '', 'type': 'log', 'value': None})
            action_end({'param': 'leaving blazedemo', 'selectors': [], 'tag': '', 'type': 'log', 'value': None})

    def test_sample(self):
        self._1_Test()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
