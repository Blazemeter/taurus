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
from bzt.resources.selenium_extras import add_logging_handlers, get_locator, dialogs_replace


class TestSample(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 30.0
        self.driver = None
        options = webdriver.ChromeOptions()
        options.add_argument('--no-sandbox')
        options.add_argument('--disable-dev-shm-usage')
        self.driver = webdriver.Chrome(service_log_path='/somewhere/webdriver.log', options=options)
        self.driver.implicitly_wait(timeout)
        add_logging_handlers()
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={},
                                       scenario_name='sample')

    def _1_Test(self):
        with apiritif.smart_transaction('Test'):
            apiritif.extended_log('start: go(http://blazedemo.com/)')
            self.driver.get('http://blazedemo.com/')

            dialogs_replace()
            apiritif.extended_log('end: go(http://blazedemo.com/)')
            apiritif.extended_log('start: log(leaving blazedemo)')
            apiritif.extended_log('leaving blazedemo')
            apiritif.extended_log('end: log(leaving blazedemo)')

    def test_sample(self):
        self._1_Test()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
