import unittest
import re
from time import sleep
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
from selenium.webdriver.common.by import By
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait


class TestRequests(unittest.TestCase):
    driver=None
        
    @classmethod
    def setUpClass(cls):
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '/home/undera/Sources/taurus/build/test/2016-12-26_21-34-41.730333/webdriver.log')
        cls.driver = webdriver.Firefox(profile)
        cls.driver.implicitly_wait(3.0)
        cls.driver.maximize_window()
        
    @classmethod
    def tearDownClass(cls):
        cls.driver.quit()
        
    def test_00000__(self):
        # start request: http://blazedemo.com/
        self.driver.get('http://blazedemo.com/')
        # end request: http://blazedemo.com/
        self.driver.find_element(By.CSS_SELECTOR, 'div.container p a').click()
        WebDriverWait(self.driver, 3).until(econd.presence_of_element_located((By.CSS_SELECTOR, 'img.rounded')), "Element 'img.rounded' failed to appear in 3s")
        self.driver.find_element(By.NAME, 'toPort').send_keys('"')
        body = self.driver.page_source
        re_pattern = re.compile(r'contained_text')
        self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")
        
