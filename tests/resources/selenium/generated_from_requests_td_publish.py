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
        from selenium.webdriver.remote.remote_connection import RemoteConnection
        import copy
        _original_execute = RemoteConnection.execute

        def execute_with_retries(self, command, params=None):
            params_copy = copy.deepcopy(params)
            retries = 3
            delay = 2
            last_exc = None
            for attempt in range(retries):
                try:
                    if (params != params_copy):
                        return _original_execute(self, command, params_copy)
                    else:
                        return _original_execute(self, command, params)
                except Exception as e:
                    last_exc = e
                    print(f'[Retry] RemoteConnection.execute failed on attempt {(attempt + 1)}: {e}')
                    sleep(delay)
            raise last_exc
        RemoteConnection.execute = execute_with_retries
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
