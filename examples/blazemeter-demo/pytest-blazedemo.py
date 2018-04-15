# -*- coding: utf-8 -*-
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
import unittest, time, re

class Py2(unittest.TestCase):
    def setUp(self):
        self.driver = webdriver.Firefox()
        self.driver.implicitly_wait(30)
        self.base_url = "http://blazedemo.com/"
        self.verificationErrors = []
        self.accept_next_alert = True
    
    def test_py2(self):
        driver = self.driver
        driver.get(self.base_url + "/")
        Select(driver.find_element_by_name("fromPort")).select_by_visible_text("Boston")
        Select(driver.find_element_by_name("toPort")).select_by_visible_text("London")
        driver.find_element_by_css_selector("input.btn.btn-primary").click()
        driver.find_element_by_xpath("(//input[@value='Choose This Flight'])[2]").click()
        driver.find_element_by_id("inputName").clear()
        driver.find_element_by_id("inputName").send_keys("Blazy Blaze")
        driver.find_element_by_id("address").clear()
        driver.find_element_by_id("address").send_keys("h")
        driver.find_element_by_id("city").clear()
        driver.find_element_by_id("city").send_keys("Santa Clara")
        driver.find_element_by_id("state").clear()
        driver.find_element_by_id("state").send_keys("CA")
        driver.find_element_by_id("zipCode").clear()
        driver.find_element_by_id("zipCode").send_keys("12345")
        Select(driver.find_element_by_id("cardType")).select_by_visible_text("American Express")
        driver.find_element_by_id("creditCardNumber").clear()
        driver.find_element_by_id("creditCardNumber").send_keys("8")
        driver.find_element_by_id("creditCardMonth").clear()
        driver.find_element_by_id("creditCardMonth").send_keys("12")
        driver.find_element_by_id("creditCardYear").clear()
        driver.find_element_by_id("creditCardYear").send_keys("2019")
        driver.find_element_by_id("nameOnCard").clear()
        driver.find_element_by_id("nameOnCard").send_keys("BlazeMeter")
        driver.find_element_by_css_selector("input.btn.btn-primary").click()
    
    def is_element_present(self, how, what):
        try: self.driver.find_element(by=how, value=what)
        except NoSuchElementException as e: return False
        return True
    
    def is_alert_present(self):
        try: self.driver.switch_to_alert()
        except NoAlertPresentException as e: return False
        return True
    
    def close_alert_and_get_its_text(self):
        try:
            alert = self.driver.switch_to_alert()
            alert_text = alert.text
            if self.accept_next_alert:
                alert.accept()
            else:
                alert.dismiss()
            return alert_text
        finally: self.accept_next_alert = True
    
    def tearDown(self):
        self.driver.quit()
        self.assertEqual([], self.verificationErrors)

if __name__ == "__main__":
    unittest.main()
