# coding=utf-8
import unittest
import re
from time import sleep
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys

import apiritif
import selenium_taurus_extras

_vars = {}
_tpl = selenium_taurus_extras.Template(_vars)

class TestRequests(unittest.TestCase):
    def setUp(self):
        options = webdriver.FirefoxOptions()
        profile = webdriver.FirefoxProfile()
        self.driver = webdriver.Firefox(profile, firefox_options=options)
        self.driver.implicitly_wait(60.0)
        self.wnd_mng = selenium_taurus_extras.WindowManager(self.driver)
        self.frm_mng = selenium_taurus_extras.FrameManager(self.driver)

    def tearDown(self):
        self.driver.quit()

    def test_requests(self):
        self.driver.implicitly_wait(60.0)

        with apiritif.transaction_logged('Test'):
            self.driver.get(_tpl.apply('https://www.demoblaze.com/'))
            self.driver.find_element(By.ID, _tpl.apply('itemc')).click()


