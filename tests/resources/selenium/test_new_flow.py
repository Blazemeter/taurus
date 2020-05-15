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


class TestSdsdsdsSelenium(unittest.TestCase):

    def setUp(self):
        self.driver = None
        timeout = 60.0
        options = webdriver.ChromeOptions()
        self.driver = webdriver.Chrome(service_log_path='webdriver.log', options=options)
        self.driver.implicitly_wait(timeout)

        self.vars = {

        }

        add_flow_markers()

        apiritif.put_into_thread_store(
            driver=self.driver,
            timeout=timeout,
            func_mode=False)  # don't stop after failed test case

    def _1_t1(self):
        with apiritif.smart_transaction('t1'):
            self.driver.get('http://blazedemo.com/purchase.php')
            self.driver.find_element(By.CSS_SELECTOR, 'input.btn.btn-primary').click()

    def _2_t2(self):
        with apiritif.smart_transaction('t2'):
            self.driver.get('https://www.belarus.by/en/')
            body = self.driver.page_source
            re_pattern = re.compile('In God we trust')
            self.assertNotEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'In God we trust' not found in BODY")

    def _3_t3(self):
        with apiritif.smart_transaction('t3'):
            self.driver.get('some.strange.url')

    def test_sdsdsds_Selenium(self):
        self._1_t1()
        self._2_t2()
        self._3_t3()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
