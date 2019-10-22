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


class TestSdsdsdsSelenium(unittest.TestCase):

    def setUp(self):
        self.driver = None
        #options = webdriver.ChromeOptions()
        try:
            self.driver = webdriver.Remote(command_executor='https://1:2@bza-v.env.blazemeter.net/api/v4/grid/wd/hub/')
        except BaseException as exc:
            raise BaseException("Prefix: %s" % str(exc))

        #self.driver = webdriver.Chrome(service_log_path='webdriver.log', chrome_options=options)
        self.driver.implicitly_wait(60.0)
        func_mode = False  # don't stop after failed test case
        flow_markers = True  # send flow markers to webdriver
        self.vars = {

        }

        apiritif.put_into_thread_store(
            vars=self.vars,
            driver=self.driver,
            func_mode=func_mode,
            flow_markers=flow_markers)

        self.wnd_mng = WindowManager(self.driver)
        self.frm_mng = FrameManager(self.driver)

    def tearDown(self):
        if self.driver:
            self.driver.quit()

    def test_them_all(self):
        with apiritif.smart_transaction(name='https://mail.ru'):
            self.driver.get('https://mail.ru')


