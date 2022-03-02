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
        Select(driver.find_element(By.NAME, "fromPort")).select_by_visible_text("Boston")
        Select(driver.find_element(By.NAME,"toPort")).select_by_visible_text("London")
        driver.find_element(By.CSS_SELECTOR, "input.btn.btn-primary").click()
        driver.find_element(By.XPATH, "(//input[@value='Choose This Flight'])[2]").click()
        driver.find_element(By.ID, "inputName").clear()
        driver.find_element(By.ID, "inputName").send_keys("Blazy Blaze")
        driver.find_element(By.ID, "address").clear()
        driver.find_element(By.ID, "address").send_keys("h")
        driver.find_element(By.ID, "city").clear()
        driver.find_element(By.ID, "city").send_keys("Santa Clara")
        driver.find_element(By.ID, "state").clear()
        driver.find_element(By.ID, "state").send_keys("CA")
        driver.find_element(By.ID, "zipCode").clear()
        driver.find_element(By.ID, "zipCode").send_keys("12345")
        Select(driver.find_element(By.ID, "cardType")).select_by_visible_text("American Express")
        driver.find_element(By.ID, "creditCardNumber").clear()
        driver.find_element(By.ID, "creditCardNumber").send_keys("8")
        driver.find_element(By.ID, "creditCardMonth").clear()
        driver.find_element(By.ID, "creditCardMonth").send_keys("12")
        driver.find_element(By.ID, "creditCardYear").clear()
        driver.find_element(By.ID, "creditCardYear").send_keys("2019")
        driver.find_element(By.ID, "nameOnCard").clear()
        driver.find_element(By.ID, "nameOnCard").send_keys("BlazeMeter")
        driver.find_element(By.CSS_SELECTOR, "input.btn.btn-primary").click()
    
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
