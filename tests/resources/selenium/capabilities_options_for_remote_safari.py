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
from bzt.resources.selenium_extras import get_locator, add_flow_markers, waiter


class TestRemoteSc(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 30.0
        options = webdriver.WebKitGTKOptions()
        options.ignore_local_proxy_environment_variables()
        options.add_argument('one')
        options.add_argument('two')
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
        options.set_capability('browserName', 'safari')
        self.driver = webdriver.Remote(command_executor='http://addr-of-remote-server.com/api/v4/grid/wd/hub',
                                       options=options)
        self.driver.implicitly_wait(timeout)
        add_flow_markers()
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={},
                                       scenario_name='remote_sc')

    def _1_blacom(self):
        with apiritif.smart_transaction('bla.com'):
            self.driver.get('bla.com')

    def test_remotesc(self):
        self._1_blacom()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
