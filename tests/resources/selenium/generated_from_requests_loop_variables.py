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
from selenium.webdriver.common.options import ArgOptions
from bzt.resources.selenium_extras import get_loop_range, get_locator, waiter

class TestLocSc(unittest.TestCase):

    def setUp(self):
        self.vars = {'end': 20, 'start': 10, 'step': 1}

        timeout = 30.0
        options = webdriver.FirefoxOptions()
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '/somewhere/webdriver.log')
        options.set_capability('unhandledPromptBehavior', 'ignore')
        self.driver = webdriver.Firefox(profile, options=options)
        self.driver.implicitly_wait(timeout)
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={},
                                       scenario_name='loc_sc')


    def _1_None(self):
        with apiritif.smart_transaction('None'):

            for i in get_loop_range(self.vars['start'], self.vars['end'], self.vars['step']):
                self.vars['i'] = str(i)

                var_loc_keys = get_locator([{'id': 'id_{}'.format(self.vars['i'])}])
                self.driver.find_element(
                    var_loc_keys[0],
                    var_loc_keys[1]).click()
                waiter()

    def test_locsc(self):
        self._1_None()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
