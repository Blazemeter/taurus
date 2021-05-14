import unittest
import apiritif

import traceback
import sys

from selenium import webdriver


class TestSample(unittest.TestCase):

    def setUp(self):
        timeout = 10.0
        self.driver = None
        options = webdriver.ChromeOptions()
        options.add_argument('--no-sandbox')
        options.add_argument('--disable-dev-shm-usage')
        try:
            raise Exception('You will not find any driver here')
        except Exception:
            ex_type, ex, tb = sys.exc_info()
            apiritif.log.info('<StoppingReason>' + str(traceback.format_exception(ex_type, ex, tb)))
            raise

        self.driver.implicitly_wait(timeout)
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={}, scenario_name='test_stopping_reason')

    def tearDown(self):
        if self.driver:
            self.driver.quit()

    def test_is_available(self):
        self.assertTrue(True)


