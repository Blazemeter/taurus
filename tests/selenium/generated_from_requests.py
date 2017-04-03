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
    driver = None
        
    @classmethod
    def setUpClass(cls):
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '<somewhere>/webdriver.log')
        cls.driver = webdriver.Firefox(profile)
        cls.driver.implicitly_wait(3.5)
        cls.driver.maximize_window()
        
    @classmethod
    def tearDownClass(cls):
        cls.driver.quit()
        
    def setUp(self):
        self.driver.implicitly_wait(3.5)
        
    def test_00000__(self):
        self.driver.get('http://blazedemo.com/')
        WebDriverWait(self.driver, 3.5).until(econd.visibility_of_element_located((By.NAME, 'toPort')), "Element 'toPort' failed to appear within 3.5s")
        self.driver.find_element(By.NAME, 'toPort').send_keys('B')
        self.driver.find_element(By.XPATH, '//div[3]/form/select[1]//option[3]').click()
        self.driver.find_element(By.XPATH, '//div[3]/form/select[2]//option[6]').click()
        self.driver.find_element(By.XPATH, "//input[@type='submit']").click()
        self.driver.find_element(By.LINK_TEXT, 'destination of the week! The Beach!').click()
        body = self.driver.page_source
        re_pattern = re.compile(r'contained_text')
        self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")
        pass
        
    def test_00001_empty(self):
        pass
        
