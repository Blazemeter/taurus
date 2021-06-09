# coding=utf-8

import logging
import random
import string
import sys
import traceback
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
from bzt.resources.selenium_extras import waiter, dialogs_replace, get_locator

class TestSample(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 30.0
        self.driver = None
        apiritif.external_handler(self.driver.session_id if self.driver else None, 'yaml_action_start', {'param': {}, 'type': 'new_session'})
        try:
            options = webdriver.ChromeOptions()
            options.add_argument('--no-sandbox')
            options.add_argument('--disable-dev-shm-usage')
            self.driver = webdriver.Chrome(service_log_path='/somewhere/webdriver.log', options=options)
            self.driver.implicitly_wait(timeout)
        except Exception as e:
            (ex_type, ex, tb) = sys.exc_info()
            apiritif.external_handler(self.driver.session_id if self.driver else None, 'yaml_action_end', {'message': str(traceback.format_exception(ex_type, ex, tb)), 'param': {}, 'type': 'new_session'})
            apiritif.log.error(str(traceback.format_exception(ex_type, ex, tb)))
            raise e
        apiritif.external_handler(self.driver.session_id if self.driver else None, 'yaml_action_end', {'param': {}, 'type': 'new_session'})
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={},
                                       scenario_name='sample')


    def _1_Test(self):
        with apiritif.smart_transaction('Test'):
            apiritif.external_handler(self.driver.session_id if self.driver else None, 'yaml_action_start', {'param': 'http://blazedemo.com/', 'selectors': [], 'tag': '', 'type': 'go', 'value': None})
            self.driver.get('http://blazedemo.com/')

            dialogs_replace()
            waiter()
            apiritif.external_handler(self.driver.session_id if self.driver else None, 'yaml_action_end', {'param': 'http://blazedemo.com/', 'selectors': [], 'tag': '', 'type': 'go', 'value': None})

    def test_sample(self):
        self._1_Test()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
