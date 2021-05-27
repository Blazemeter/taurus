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
from bzt.resources.selenium_extras import dialogs_replace, add_logging_handlers, waiter, get_locator

class TestSample(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 30.0
        self.driver = None
        options = webdriver.ChromeOptions()
        options.add_argument('--no-sandbox')
        options.add_argument('--disable-dev-shm-usage')
        options.set_capability('unhandledPromptBehavior', 'ignore')
        try:
            self.driver = webdriver.Chrome(service_log_path='/somewhere/webdriver.log', options=options)
        except Exception:
            (ex_type, ex, tb) = sys.exc_info()
            apiritif.log.info(('<StoppingReason>' + str(traceback.format_exception(ex_type, ex, tb))))
            raise
        self.driver.implicitly_wait(timeout)
        add_logging_handlers()
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={},
                                       scenario_name='sample')


    def _1_Test(self):
        with apiritif.smart_transaction('Test'):
            apiritif.external_log('start: go(http://blazedemo.com/)')
            self.driver.get('http://blazedemo.com/')

            dialogs_replace()
            waiter()
            apiritif.external_log('end: go(http://blazedemo.com/)')
            apiritif.external_log('start: log(leaving blazedemo)')
            apiritif.external_log('leaving blazedemo')
            apiritif.external_log('end: log(leaving blazedemo)')

    def test_sample(self):
        self._1_Test()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
