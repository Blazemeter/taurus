# -*- coding: utf-8 -*-
import unittest

from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException


class TestBlazemeterFail(unittest.TestCase):
    def setUp(self):
        self.base_url = "http://demo.blazemeter.com/"
        self.verificationErrors = []
        self.accept_next_alert = True
    
    def test_blazemeter_fail(self):
        pass
    
    def is_element_present(self, how, what):
        return True
    
    def is_alert_present(self):
        return True
    
    def close_alert_and_get_its_text(self):
        pass
    
    def tearDown(self):
        pass

if __name__ == "__main__":
    unittest.main()
