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
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys
from bzt.resources.selenium_extras import add_flow_markers


class TestLocSc(unittest.TestCase):

    def setUp(self):
        self.driver = None
        options = webdriver.ChromeOptions()
        self.driver = webdriver.Chrome(service_log_path='<somewhere>webdriver.log', chrome_options=options)
        self.driver.implicitly_wait(3.5)
        self.vars = {

        }
        add_flow_markers()
        apiritif.put_into_thread_store(driver=self.driver, func_mode=False)

    def _1_(self):
        with apiritif.smart_transaction('/'):
            self.driver.get('http://blazedemo.com/')
            WebDriverWait(self.driver, 3.5).until(
                econd.presence_of_element_located((By.XPATH, "//input[@type='submit']")),
                'Element "//input[@type=\'submit\']" failed to appear within 3.5s')
            self.assertEqual(self.driver.title, 'BlazeDemo')
            body = self.driver.page_source
            re_pattern = re.compile('contained_text')
            self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")

    def _2_empty(self):
        with apiritif.smart_transaction('empty'):
            pass

    def test_locsc(self):
        self._1_()
        self._2_empty()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
