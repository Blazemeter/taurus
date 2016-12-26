import unittest
import re
from time import sleep
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
from selenium.webdriver.common.by import By


class TestRequests(unittest.TestCase):
    driver=None
        
    @classmethod
    def setUpClass(cls):
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '/home/undera/Sources/taurus/build/test/2016-12-26_20-58-02.964276/webdriver.log')
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
        self.driver.find_element(By.CSS_SELECTOR, "div.container p a").click()
        self.driver.find_element(By.CSS_SELECTOR, "img.rounded").send_keys("test")
        body = self.driver.page_source
        re_pattern = re.compile(r'contained_text')
        self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")
        
