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
        profile.set_preference('webdriver.log.file', '/home/taras/Projects/taurus/build/test/2016-09-12_16-59-49.661488/webdriver.log')
        cls.driver = webdriver.Firefox(profile)
        cls.driver.implicitly_wait(30.0)
        cls.driver.maximize_window()
        
    @classmethod
    def tearDownClass(cls):
        cls.driver.quit()
        
    def test_00000__(self):
        # start request: http://blazedemo.com/
        self.driver.get('http://blazedemo.com/')
        # end request: http://blazedemo.com/
        
