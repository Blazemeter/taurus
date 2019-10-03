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
from selenium.common.exceptions import NoSuchElementException, NoSuchWindowException, NoSuchFrameException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys

from bzt.resources.selenium_extras import FrameManager, WindowManager


def setup():
    # todo: avoid duplicate setup calls
    options = webdriver.ChromeOptions()
    driver = webdriver.Chrome(service_log_path='webdriver.log',
                              chrome_options=options)
    driver.implicitly_wait(60.0)
    wnd_mng = WindowManager(driver)
    frm_mng = FrameManager(driver)
    func_mode = False
    flow_markers = True
    vars = {

    }
    apiritif.put_into_thread_store(vars, driver, wnd_mng, frm_mng, func_mode, flow_markers)


def teardown():
    driver = apiritif.get_from_thread_store()[1]
    driver.quit()


class TestSdsdsdsSelenium(unittest.TestCase):

    def setUp(self):
        (self.vars, self.driver, self.wnd_mng, self.frm_mng, self.func_mode, self.flow_markers) = \
            apiritif.get_from_thread_store()

    def t1(self):
        with apiritif.smart_transaction(
                name='t1',
                flow_markers=True,
                driver=self.driver):
            self.driver.get('http://blazedemo.com/purchase.php')
            self.driver.find_element(By.CSS_SELECTOR, 'input.btn.btn-primary').click()

    def t2(self):
        with apiritif.smart_transaction(
                name='t2',
                flow_markers=True,
                driver=self.driver):
            self.driver.get('https://www.belarus.by/en/')
            body = self.driver.page_source
            re_pattern = re.compile('In God we trust')
            self.assertNotEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'In God we trust' not found in BODY")

    def t3(self):
        with apiritif.smart_transaction(
                name='t3',
                flow_markers=True,
                driver=self.driver):
            self.driver.get('some.strange.url')

    def test_all(self):
        self.t1()
        self.t2()
        self.t3()

