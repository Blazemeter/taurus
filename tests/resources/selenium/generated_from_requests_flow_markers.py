# coding=utf-8
import unittest
import os
import re
from time import sleep, time
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys

import apiritif
from bzt.resources import selenium_taurus_extras

_vars = {}
_tpl = selenium_taurus_extras.Template(_vars)

class TestRequests(unittest.TestCase):
    def setUp(self):
        options = webdriver.ChromeOptions()
        self.driver = webdriver.Chrome(service_log_path='<somewhere>webdriver.log', chrome_options=options)
        self.driver.implicitly_wait(3.5)
        self.wnd_mng = selenium_taurus_extras.WindowManager(self.driver)
        self.frm_mng = selenium_taurus_extras.FrameManager(self.driver)

    def tearDown(self):
        self.driver.quit()

    def test_requests(self):
        self.driver.implicitly_wait(3.5)

        try:
            self.driver.execute_script('/* FLOW_MARKER test-case-start */', {'testCaseName': '/', 'testSuiteName': 'loc_sc'})

            with apiritif.transaction_logged('/'):
                self.driver.get('http://blazedemo.com/')

                WebDriverWait(self.driver, 3.5).until(econd.presence_of_element_located((By.XPATH, _tpl.apply("//input[@type='submit']"))), 'Element "//input[@type=\'submit\']" failed to appear within 3.5s')
                self.assertEqual(self.driver.title, _tpl.apply('BlazeDemo'))

                body = self.driver.page_source
                re_pattern = re.compile(r'contained_text')
                self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")

        except AssertionError as exc:
            self.driver.execute_script('/* FLOW_MARKER test-case-stop */', {'status': 'assert', 'message': str(exc)})
            raise
        except BaseException as exc:
            self.driver.execute_script('/* FLOW_MARKER test-case-stop */', {'status': 'fail', 'message': str(exc)})
            raise
        else:
            self.driver.execute_script('/* FLOW_MARKER test-case-stop */', {'status': 'success', 'message': ''})

        try:
            self.driver.execute_script('/* FLOW_MARKER test-case-start */', {'testCaseName': 'empty', 'testSuiteName': 'loc_sc'})

            with apiritif.transaction_logged('empty'):
                pass

        except AssertionError as exc:
            self.driver.execute_script('/* FLOW_MARKER test-case-stop */', {'status': 'assert', 'message': str(exc)})
            raise
        except BaseException as exc:
            self.driver.execute_script('/* FLOW_MARKER test-case-stop */', {'status': 'fail', 'message': str(exc)})
            raise
        else:
            self.driver.execute_script('/* FLOW_MARKER test-case-stop */', {'status': 'success', 'message': ''})
