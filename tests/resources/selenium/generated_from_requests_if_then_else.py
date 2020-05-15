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
from bzt.resources.selenium_extras import get_locator, dialogs_replace


class TestLocSc(unittest.TestCase):

    def setUp(self):
        self.vars = {'city_select_name': 'fromPort', 'input_name_id': 'inputName'}

        timeout = 3.5
        self.driver = None
        options = webdriver.ChromeOptions()
        options.add_argument('--no-sandbox')
        options.add_argument('--disable-dev-shm-usage')
        self.driver = webdriver.Chrome(
            service_log_path='/somewhere/webdriver.log',
            options=options)
        self.driver.implicitly_wait(timeout)
        apiritif.put_into_thread_store(scenario_name='loc_sc', timeout=timeout, func_mode=False, windows={},
                                       driver=self.driver)

    def _1_Conditions_test(self):
        with apiritif.smart_transaction('Conditions test'):
            self.driver.get('http://blazedemo.com')

            dialogs_replace()

            test = self.driver.execute_script('return document.getElementsByName("fromPort")[0].length > 0;')
            if test:

                var_loc_keys = get_locator([{'id': 'wrong_id'}, {'xpath': '/html/body/div[3]/form/div/input'}])
                self.driver.find_element(
                    var_loc_keys[0],
                    var_loc_keys[1]).click()
                sleep(1.0)

                test = self.driver.execute_script('return document.getElementsByClassName("table")[0].rows.length > 5;')
                if test:

                    var_loc_keys = get_locator([{'xpath': '/html/body/div[2]/table/tbody/tr[5]/td[1]/input'}])
                    self.driver.find_element(
                        var_loc_keys[0],
                        var_loc_keys[1]).click()

                    test = self.driver.execute_script(
                        'return document.getElementById("{}").value === \'\';'.format(self.vars['input_name_id']))
                    if test:

                        var_loc_keys = get_locator([{'id': self.vars['input_name_id']}])
                        self.driver.find_element(
                            var_loc_keys[0],
                            var_loc_keys[1]).clear()
                        self.driver.find_element(
                            var_loc_keys[0],
                            var_loc_keys[1]).send_keys('John Doe')
                    else:

                        var_loc_keys = get_locator([{'id': self.vars['input_name_id']}])
                        self.driver.find_element(
                            var_loc_keys[0],
                            var_loc_keys[1]).clear()
                        self.driver.find_element(
                            var_loc_keys[0],
                            var_loc_keys[1]).send_keys('Jack Green')

                    var_loc_keys = get_locator([{'xpath': '/html/body/div[2]/form/div[11]/div/input'}])
                    self.driver.find_element(
                        var_loc_keys[0],
                        var_loc_keys[1]).click()
                    sleep(5.0)
            else:

                test = self.driver.execute_script('return document.getElementsByClassName("table")[0].rows.length > 5;')
                if test:

                    var_loc_keys = get_locator([{'id': self.vars['elem2_id']}])
                    self.driver.find_element(
                        var_loc_keys[0],
                        var_loc_keys[1]).clear()
                    self.driver.find_element(
                        var_loc_keys[0],
                        var_loc_keys[1]).send_keys('my text')

                    test = self.driver.execute_script('return window.screen.width > 1000;')
                    if test:
                        self.driver.save_screenshot('file_1000')
                    else:
                        self.driver.save_screenshot('file')
                else:

                    var_loc_keys = get_locator([{'xpath': '/html/body/div[3]/input'}])
                    self.driver.find_element(
                        var_loc_keys[0],
                        var_loc_keys[1]).click()

    def test_locsc(self):
        self._1_Conditions_test()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
