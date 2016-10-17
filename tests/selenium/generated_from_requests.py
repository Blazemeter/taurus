import unittest
import re
from time import sleep
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException

class TestRequests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', 'somewhere')
        cls.driver = webdriver.Firefox(profile)
        cls.driver.implicitly_wait(30.0)
        cls.driver.maximize_window()
        
    @classmethod
    def tearDownClass(cls):
        cls.driver.quit()
        
    def test_00000__(self):
        # start request: http://blazedemo.com/
        self.driver.get('http://blazedemo.com/')
        body = self.driver.page_source
        re_pattern = re.compile(r'contained_text')
        self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")
        # end request: http://blazedemo.com/
        
