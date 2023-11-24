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
from bzt.resources.selenium_extras import get_current_iteration, get_locator, waiter
from bzt.resources.bzm_extras import BzmExtras

class TestPublishSc(unittest.TestCase):

    def setUp(self):
        self.vars = {}
        
        timeout = 30.0
        options = webdriver.FirefoxOptions()
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '/somewhere/webdriver.log')
        options.set_capability('unhandledPromptBehavior', 'ignore')
        self.driver = webdriver.Firefox(profile, options=options)
        self.driver.implicitly_wait(timeout)
        current_iter = get_current_iteration()
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={}, scenario_name='publish_sc', current_iteration=current_iter)
        self.bzm_extras = BzmExtras({'master_publish_url': 'https://tdm.blazemeter.com/api/v1/publish?signature=8UJR9hHfsdjg9032nkvx'})

    def _1_None(self):
        with apiritif.smart_transaction('None'):

            do_testdata_orchestration = self.bzm_extras.do_testdata_orchestration

            do_testdata_orchestration('publish', 'entity1', 'target1', True)

            do_testdata_orchestration = self.bzm_extras.do_testdata_orchestration

            do_testdata_orchestration('un-publish', 'entity1', 'target1')

    def test_publishsc(self):
        self._1_None()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
