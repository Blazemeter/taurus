import unittest
import re
from time import sleep
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
import selenium_taurus_extras

_vars = {}
_tpl = selenium_taurus_extras.Template(_vars)

class TestRequests(unittest.TestCase):
    def setUp(self):
        self.driver = webdriver.Remote(command_executor='http://user:key@remote_web_driver_host:port/wd/hub', desired_capabilities={"app": "", "browserName": "firefox", "deviceName": "", "javascriptEnabled": "True", "platformName": "linux", "platformVersion": "", "seleniumVersion": "", "version": "54.0"})
        self.driver.implicitly_wait(3.5)
        self.wnd_mng = selenium_taurus_extras.WindowManager(self.driver)

    def tearDown(self):
        self.driver.quit()

    def test_requests(self):
        self.driver.implicitly_wait(3.5)

        with apiritif.transaction_logged('/'):
            self.driver.get('http://blazedemo.com/')

            WebDriverWait(self.driver, 3.5).until(econd.presence_of_element_located((By.XPATH, _tpl.apply("//input[@type='submit']"))), 'Element "//input[@type=\'submit\']" failed to appear within 3.5s')
            self.assertEqual(self.driver.title, _tpl.apply('BlazeDemo'))


        body = self.driver.page_source
        re_pattern = re.compile(r'contained_text')
        self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")

        with apiritif.transaction_logged('empty'):
            pass

