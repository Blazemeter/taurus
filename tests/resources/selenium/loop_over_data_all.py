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
from bzt.resources.selenium_extras import get_csv_reader_for_entity_loop, waiter, get_locator
reader_1 = apiritif.CSVReaderPerThread('/somewhere/cities.csv', fieldnames=['name', ' country'], loop=True, quoted=False, delimiter=',', encoding='utf-8')


class TestScenario1(unittest.TestCase):

    def setUp(self):
        self.vars = {}
        reader_1.read_vars()
        self.vars.update(reader_1.get_vars())

        timeout = 30.0
        options = webdriver.FirefoxOptions()
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
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '/somewhere/webdriver.log')
        options.set_capability('unhandledPromptBehavior', 'ignore')
        self.driver = webdriver.Firefox(profile, options=options)
        self.driver.implicitly_wait(timeout)
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={}, scenario_name='scenario1', data_sources=True)


    def _1_None(self):
        with apiritif.smart_transaction('None'):

            cities_csv_csv_reader = get_csv_reader_for_entity_loop([{'path': '/somewhere/cities.csv', 'variable-names': ['name', 'country'], 'quoted': False, 'delimiter': ',', 'encoding': 'utf-8'}], 'cities.csv')
            cities_csv_loop_counter = 0
            while True:
                try:
                    cities_csv_csv_reader.read_vars()

                    self.vars["city"] = cities_csv_csv_reader.get_vars()
                    cities_csv_loop_counter += 1
                    if (1 < cities_csv_loop_counter < 6):
                        self.driver.get('https://www.google.com?q={}'.format(self.vars['city']['name']))

                        waiter()
                    elif (cities_csv_loop_counter > 5):
                        break
                except apiritif.utils.NormalShutdown:
                    break

    def test_scenario1(self):
        self._1_None()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
