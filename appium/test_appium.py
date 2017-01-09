import re
import time
import logging

from appium.webdriver import Remote

from unittest import TestCase


class TestWebAppium(TestCase):
    def setUp(self):
        self.log = logging.getLogger('')
        desired_caps = {}
        desired_caps['platformName'] = 'Android'
        desired_caps['deviceName'] = 'Android Emulator'
        desired_caps['browserName'] = 'Chrome'
        self.log.info("connect to webdriver...")
        self.driver = Remote('http://localhost:4723/wd/hub', desired_caps)
        time.sleep(3)

    def tearDown(self):
        self.driver.quit()

    def test_1(self):
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


class TestAppAppium(TestCase):
    def setUp(self):
        self.log = logging.getLogger('')
        desired_caps = {}
        desired_caps['platformName'] = 'Android'
        desired_caps['deviceName'] = 'Android Emulator'
        desired_caps['app'] = '/home/taras/Android/Sdk/extras/google/webdriver/SimpleApp/bin/SimpleApp.apk'
        self.log.info("connect to webdriver...")
        self.driver = Remote('http://localhost:4723/wd/hub', desired_caps)
        time.sleep(3)

    def tearDown(self):
        self.driver.quit()

    def test_1(self):
        a = 1+1
        pass



