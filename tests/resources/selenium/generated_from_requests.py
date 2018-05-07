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

class TestRequests(unittest.TestCase):
    def setUp(self):
        options = webdriver.FirefoxOptions()
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '<somewhere>/webdriver.log')
        self.driver = webdriver.Firefox(profile, firefox_options=options)
        self.driver.implicitly_wait(3.5)

    def tearDown(self):
        self.driver.quit()

    def test_requests(self):
        self.driver.implicitly_wait(3.5)

        with apiritif.transaction('/'):
            self.driver.get('http://blazedemo.com/')

            WebDriverWait(self.driver, 3.5).until(econd.presence_of_element_located((By.XPATH, "//input[@type='submit']")), 'Element "//input[@type=\'submit\']" failed to appear within 3.5s')
            self.assertEqual(self.driver.title,'BlazeDemo')
            ActionChains(self.driver).move_to_element(self.driver.find_element(By.XPATH, '/html/body/div[2]/div/p[2]/a')).perform()
            ActionChains(self.driver).double_click(self.driver.find_element(By.XPATH, '/html/body/div[3]/h2')).perform()
            ActionChains(self.driver).click_and_hold(self.driver.find_element(By.XPATH, '/html/body/div[3]/form/select[1]')).perform()
            ActionChains(self.driver).release(self.driver.find_element(By.XPATH, '/html/body/div[3]/form/select[1]/option[6]')).perform()
            Select(self.driver.find_element(By.NAME, 'toPort')).select_by_visible_text('London')
            self.driver.find_element(By.CSS_SELECTOR, 'body input.btn.btn-primary').send_keys(Keys.ENTER)
            self.assertEqual(self.driver.find_element(By.ID, 'address').get_attribute('value'), '123 Beautiful st.')
            self.assertEqual(self.driver.find_element(By.XPATH, '/html/body/div[2]/form/div[1]/label').get_attribute('innerText'), 'Name')
            WebDriverWait(self.driver, 3.5).until(econd.visibility_of_element_located((By.NAME, 'toPort')), "Element 'toPort' failed to appear within 3.5s")
            self.driver.find_element(By.NAME, 'toPort').send_keys('B')
            self.driver.find_element(By.XPATH, '//div[3]/form/select[1]//option[3]').click()
            self.driver.find_element(By.XPATH, '//div[3]/form/select[2]//option[6]').click()
            self.driver.switch_to.window(self.driver.window_handles[0])
            current_window = self.driver.current_window_handle; self.driver.switch_to.window(self.driver.window_handles[1]); self.driver.close(); self.driver.switch_to.window(current_window)
            self.driver.find_element(By.NAME, 'toPort').submit()
            self.driver.execute_script("alert('This is Sparta');")
            self.driver.switch_to.frame(self.driver.find_element(By.NAME, 'my_frame'))
            if self.driver.find_element(By.ID, 'editor').get_attribute('contenteditable'): self.driver.find_element(By.ID, 'editor').clear(); self.driver.find_element(By.ID, 'editor').send_keys('lo-la-lu')
            sleep(3)
            self.driver.delete_all_cookies()
            self.driver.find_element(By.LINK_TEXT, 'destination of the week! The Beach!').click()


        body = self.driver.page_source
        re_pattern = re.compile(r'contained_text')
        self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")

        with apiritif.transaction('empty'):
            pass

