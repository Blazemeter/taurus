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
from bzt.resources.selenium_extras import LocatorsManager, add_flow_markers, DialogsManager, WaitForManager


class TestLocScRemote(unittest.TestCase):

    def setUp(self):
        self.vars = {

        }
        self.driver = None
        self.driver = webdriver.Remote(command_executor='http://user:key@remote_web_driver_host:port/wd/hub',
                                       desired_capabilities={
                                           'app': '',
                                           'browserName': 'firefox',
                                           'deviceName': '',
                                           'javascriptEnabled': 'True',
                                           'platformName': 'linux',
                                           'platformVersion': '',
                                           'seleniumVersion': '',
                                           'version': '54.0',
                                       })
        self.driver.implicitly_wait(3.5)
        self.loc_mng = LocatorsManager(self.driver, 3.5)
        self.wait_for_mng = WaitForManager(self.driver, 3.5)
        self.dlg_mng = DialogsManager(self.driver, False)
        add_flow_markers()
        apiritif.put_into_thread_store(func_mode=False, driver=self.driver, scenario_name='loc_sc_remote')

    def _1_(self):
        with apiritif.smart_transaction('/'):
            self.driver.get('http://blazedemo.com/')
            self.dlg_mng.replace_dialogs()
            self.wait_for_mng.wait_for('present', [{'xpath': "//input[@type='submit']"}], 3.5)
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
