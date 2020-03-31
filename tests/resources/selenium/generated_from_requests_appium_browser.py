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
from bzt.resources.selenium_extras import get_locator, dialogs_replace


class TestLocScAppium(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 3.5
        self.driver = None
        self.driver = webdriver.Remote(command_executor='http://localhost:4723/wd/hub',
                                       desired_capabilities={'browserName': 'chrome', 'deviceName': '',
                                                             'platformName': 'android'})
        self.driver.implicitly_wait(timeout)
        apiritif.put_into_thread_store(scenario_name='loc_sc_appium', func_mode=False, driver=self.driver,
                                       timeout=timeout)

    def _1_(self):
        with apiritif.smart_transaction('/'):
            self.driver.get('http://blazedemo.com/')
            dialogs_replace()

            var_loc_wait = get_locator([{'xpath': "//input[@type='submit']"}])
            WebDriverWait(self.driver, 3.5).until(econd.presence_of_element_located((
                var_loc_wait[0],
                var_loc_wait[1])), 'Element \'xpath\':"//input[@type=\'submit\']" failed to appear within 3.5s')
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
