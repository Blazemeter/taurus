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
from bzt.resources.selenium_extras import waiter, get_locator

class TestSc1(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 30.0
        options = webdriver.FirefoxOptions()
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '/tmp/bzt/2021-09-23_11-49-13.475963/webdriver.log')
        options.set_capability('unhandledPromptBehavior', 'ignore')
        self.driver = webdriver.Firefox(profile, options=options)
        self.driver.implicitly_wait(timeout)
        apiritif.put_into_thread_store(timeout=timeout, func_mode=True, driver=self.driver, windows={},
                                       scenario_name='Test-tear-down')


def _1_httpsblazedemocomsetup1(self):
        with apiritif.smart_transaction('https://blazedemo.com/setup1'):
            self.driver.get('https://blazedemo.com/setup1')

    def _2_setup2(self):
        with apiritif.smart_transaction('setup2'):
            self.driver.get('https://blazedemo.com/setup2')

            waiter()

    def _3_httpsblazedemocommain1(self):
        with apiritif.smart_transaction('https://blazedemo.com/main1'):
            self.driver.get('https://blazedemo.com/main1')

    def _4_main2(self):
        with apiritif.smart_transaction('main2'):
            self.driver.get('https://blazedemo.com/main2')

            waiter()

    def _5_httpsblazedemocomteardown1(self):
        with apiritif.smart_transaction('https://blazedemo.com/teardown1'):
            self.driver.get('https://blazedemo.com/teardown1')

    def _6_teardown2(self):
        with apiritif.smart_transaction('teardown2'):
            self.driver.get('https://blazedemo.com/teardown2')

            waiter()

    def test_sc1(self):
        try:
            self._1_httpsblazedemocomsetup1()
            self._2_setup2()
            self._3_httpsblazedemocommain1()
            self._4_main2()
        finally:
            apiritif.set_stage("teardown")      # can't be interrupted
            self._5_httpsblazedemocomteardown1()
            self._6_teardown2()
