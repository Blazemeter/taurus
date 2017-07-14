import unittest
import re
from time import sleep
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait

import apiritif

class TestRequests(unittest.TestCase):
    def setUp(self):
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '<somewhere>/webdriver.log')
        self.driver = webdriver.Firefox(profile)
        self.driver.implicitly_wait(3.5)
        self.driver.maximize_window()

    def tearDown(self):
        self.driver.quit()

    def test_requests(self):
        self.driver.implicitly_wait(3.5)

        with apiritif.transaction('/'):
            self.driver.get('http://blazedemo.com/')

            WebDriverWait(self.driver, 3.5).until(econd.visibility_of_element_located((By.NAME, 'toPort')), "Element 'toPort' failed to appear within 3.5s")
            self.driver.find_element(By.NAME, 'toPort').send_keys('B')
            self.driver.find_element(By.XPATH, '//div[3]/form/select[1]//option[3]').click()
            self.driver.find_element(By.XPATH, '//div[3]/form/select[2]//option[6]').click()
            self.driver.find_element(By.XPATH, "//input[@type='submit']").click()
            sleep(3)
            self.driver.delete_all_cookies()
            self.driver.find_element(By.LINK_TEXT, 'destination of the week! The Beach!').click()


        body = self.driver.page_source
        re_pattern = re.compile(r'contained_text')
        self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")

        with apiritif.transaction('empty'):
            pass
