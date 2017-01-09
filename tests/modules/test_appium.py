import re
import time
import logging

from appium import webdriver
from unittest import TestCase


class AppiumTest(TestCase):
    def setUp(self):
        self.log = logging.getLogger('')
        desired_caps = {}
        desired_caps['platformName'] = 'Android'
        desired_caps['deviceName'] = 'Android Emulator'
        desired_caps['browserName'] = 'Chrome'
        self.log.info("connect to webdriver...")
        self.driver = webdriver.Remote('http://localhost:4723/wd/hub', desired_caps)
        time.sleep(3)

    def tearDown(self):
        self.driver.quit()

    def test_install(self):
        pass

    def test_web(self):
        self.log.info('connected.')
        self.log.info('open url into browser...')
        time.sleep(3)
        element = self.driver.find_elements_by_id('com.android.chrome:id/terms_accept')
        self.driver.get('http://blazedemo.com')
        # btn = driver.find_element_by_name("Find Flights")
        # btn.click()
        self.log.info('done.')
        body = self.driver.page_source
        re_pattern = re.compile(r'Buenos Aires')
        self.log.info("found: %s", len(re.findall(re_pattern, body)) > 0)

    def test_app(self):
        pass