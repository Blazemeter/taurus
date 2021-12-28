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
from bzt.resources.selenium_extras import get_locator, add_flow_markers, waiter


class TestLocScRemote(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 30.0
        options = webdriver.WebKitGTKOptions()
        options.add_argument('one')
        options.add_argument('two')
        self.driver = webdriver.Remote(command_executor='http://user:key@remote_web_driver_host:port/wd/hub',
                                       desired_capabilities={'browserName': 'safari', 'cap1': 'val1', 'cap2': 'val2'},
                                       options=options)
        self.driver.implicitly_wait(timeout)
        add_flow_markers()
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={},
                                       scenario_name='loc_sc_remote')

    def _1_blacom(self):
        with apiritif.smart_transaction('bla.com'):
            self.driver.get('bla.com')

    def test_locscremote(self):
        self._1_blacom()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
