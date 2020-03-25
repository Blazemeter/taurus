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
from bzt.resources.selenium_extras import add_flow_markers, Manager, DialogsManager


class TestLocScRemote(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 3.5
        self.driver = None
        self.driver = webdriver.Remote(command_executor='http://user:key@remote_web_driver_host:port/wd/hub',
                                       desired_capabilities={'app': '', 'browserName': 'firefox', 'deviceName': '',
                                                             'javascriptEnabled': 'True', 'platformName': 'linux',
                                                             'platformVersion': '', 'seleniumVersion': '',
                                                             'version': '54.0'})
        self.driver.implicitly_wait(timeout)
        self.mng = Manager()
        self.dlg_mng = DialogsManager(False)
        add_flow_markers()
        apiritif.put_into_thread_store(timeout=timeout, driver=self.driver, scenario_name='loc_sc_remote',
                                       func_mode=False)

    def _1_(self):
        with apiritif.smart_transaction('/'):
            self.driver.get('http://blazedemo.com/')
            self.dlg_mng.replace_dialogs()

            var_loc_wait = self.mng.get_locator([{'xpath': "//input[@type='submit']"}])
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

    def test_locscremote(self):
        self._1_()
        self._2_empty()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
