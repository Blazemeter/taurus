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
from selenium.common.exceptions import NoSuchElementException, TimeoutException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys
from bzt.resources.selenium_extras import wait_for, get_locator, dialogs_replace


class TestLocScAppium(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 3.5
        self.driver = None
        options = None
        self.driver = webdriver.Remote(command_executor='http://localhost:4723/wd/hub',
                                       desired_capabilities={'browserName': 'chrome', 'deviceName': '',
                                                             'platformName': 'android'},
                                       options=options)
        self.driver.implicitly_wait(timeout)
        apiritif.put_into_thread_store(scenario_name='loc_sc_appium', timeout=timeout, driver=self.driver, windows={},
                                       func_mode=False)

    def _1_(self):
        with apiritif.smart_transaction('/'):
            self.driver.get('http://blazedemo.com/')
            dialogs_replace()
            wait_for('present', [{'xpath': "//input[@type='submit']"}], 3.5)
            self.assertEqual(self.driver.title, 'BlazeDemo')
            body = self.driver.page_source
            re_pattern = re.compile('contained_text')
            self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")

    def _2_empty(self):
        with apiritif.smart_transaction('empty'):
            pass

    def test_locscappium(self):
        self._1_()
        self._2_empty()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
