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
from bzt.resources.selenium_extras import waiter, action_start, action_end, get_locator

class TestBlazedemoTestSelenium(unittest.TestCase):

    def setUp(self):
        self.driver = None
        action_start({'param': {}, 'type': 'new_session', 'value': None})
        try:
            self.vars = {}
            
            timeout = 30.0
            options = webdriver.FirefoxOptions()
            profile = webdriver.FirefoxProfile()
            profile.set_preference('webdriver.log.file', '/somewhere/webdriver.log')
            options.set_capability('unhandledPromptBehavior', 'ignore')
            self.driver = webdriver.Firefox(profile, options=options)
            self.driver.implicitly_wait(timeout)
            apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={}, scenario_name='blazedemo_test-Selenium')
        except Exception:
            (ex_type, ex, tb) = sys.exc_info()
            action_end({'message': str(traceback.format_exception(ex_type, ex, tb)), 'param': {}, 'type': 'new_session'})
            apiritif.log.error(str(traceback.format_exception(ex_type, ex, tb)))
            raise
        action_end({'param': {}, 'type': 'new_session'})
    

    def _1_open_blazedemo(self):
        with apiritif.smart_transaction('open blazedemo'):
            action_start({'param': 'https://blazedemo.com/', 'selectors': [], 'tag': '', 'type': 'go', 'value': None})
            self.driver.get('https://blazedemo.com/')

            action_end({'param': 'https://blazedemo.com/', 'selectors': [], 'tag': '', 'type': 'go', 'value': None})

    def _2_justgo(self):
        with apiritif.smart_transaction('just_go'):
            action_start({'param': 'https//blazemeter.com', 'selectors': [], 'tag': '', 'type': 'go', 'value': None})
            self.driver.get('https//blazemeter.com')
            
            waiter()
            action_end({'param': 'https//blazemeter.com', 'selectors': [], 'tag': '', 'type': 'go', 'value': None})

    def test_blazedemotest_Selenium(self):
        self._1_open_blazedemo()
        self._2_justgo()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
