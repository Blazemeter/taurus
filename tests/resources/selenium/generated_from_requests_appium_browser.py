import unittest
import re
from time import sleep
from appium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys

import apiritif

class TestRequests(unittest.TestCase):
    def setUp(self):
        self.driver = webdriver.Remote(command_executor='http://localhost:4723/wd/hub' , desired_capabilities={"browserName": "Chrome", "deviceName": "", "platformName": "Android"})
        self.driver.implicitly_wait(3.5)

    def tearDown(self):
        self.driver.quit()

    def test_requests(self):
        self.driver.implicitly_wait(3.5)

        with apiritif.transaction('/'):
            self.driver.get('http://blazedemo.com/')

            WebDriverWait(self.driver, 3.5).until(econd.presence_of_element_located((By.XPATH, "//input[@type='submit']")), 'Element "//input[@type=\'submit\']" failed to appear within 3.5s')
            self.assertEqual(self.driver.title,'BlazeDemo')


        body = self.driver.page_source
        re_pattern = re.compile(r'contained_text')
        self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")

        with apiritif.transaction('empty'):
            pass

