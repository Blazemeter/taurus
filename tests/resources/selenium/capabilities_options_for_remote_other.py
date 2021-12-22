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
from bzt.resources.selenium_extras import add_flow_markers, get_locator, waiter


class TestLocScRemote(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 30.0
        options = ArgOptions()
        options.ignore_local_proxy_environment_variables()
        options.add_argument('one')
        options.add_argument('two')
        options.set_capability('cap1', 'val1')
        options.set_capability('cap2', 'val2')
        self.driver = webdriver.Remote(command_executor='http://user:key@remote_web_driver_host:port/wd/hub',
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
