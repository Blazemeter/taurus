# coding=utf-8

import logging
import random
import string
import sys
import unittest
from time import time, sleep

import apiritif

import os
import re
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys
from bzt.resources.selenium_extras import FrameManager, WindowManager


class TestLocSc(unittest.TestCase, ):
    def setUp(self):
        self.driver = None
        options = webdriver.FirefoxOptions()
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '<somewhere>webdriver.log')
        self.driver = webdriver.Firefox(profile, firefox_options=options)
        self.driver.implicitly_wait(3.5)
        self.wnd_mng = WindowManager(self.driver)
        self.frm_mng = FrameManager(self.driver)

        self.vars = {
            'name': 'Name',
            'red_pill': 'take_it',
        }

        apiritif.put_into_thread_store(driver=self.driver, func_mode=False)


    def _1_(self):
        with apiritif.smart_transaction('/'):
            self.driver.get('http://blazedemo.com/')
            WebDriverWait(self.driver, 3.5).until(econd.presence_of_element_located((By.XPATH, "//input[@type='submit']")), 'Element "//input[@type=\'submit\']" failed to appear within 3.5s')
            self.assertEqual(self.driver.title, 'BlazeDemo')
            ActionChains(self.driver).move_to_element(self.driver.find_element(By.XPATH, '/html/body/div[2]/div/p[2]/a')).perform()
            ActionChains(self.driver).double_click(self.driver.find_element(By.XPATH, '/html/body/div[3]/h2')).perform()
            ActionChains(self.driver).click_and_hold(self.driver.find_element(By.XPATH, '/html/body/div[3]/form/select[1]')).perform()
            ActionChains(self.driver).release(self.driver.find_element(By.XPATH, '/html/body/div[3]/form/select[1]/option[6]')).perform()
            Select(self.driver.find_element(By.NAME, 'toPort')).select_by_visible_text('London')
            self.driver.find_element(By.CSS_SELECTOR, 'body input.btn.btn-primary').send_keys(Keys.ENTER)
            self.assertEqual(self.driver.find_element(By.ID, 'address').get_attribute('value').strip(), '123 Beautiful st.'.strip())
            self.assertEqual(self.driver.find_element(By.XPATH, '/html/body/div[2]/form/div[1]/label').get_attribute('innerText').strip(), self.vars['name'].strip())
            WebDriverWait(self.driver, 3.5).until(econd.visibility_of_element_located((By.NAME, 'toPort')), "Element 'toPort' failed to appear within 3.5s")
            self.driver.find_element(By.NAME, 'toPort').send_keys('B')
            self.driver.find_element(By.NAME, 'toPort').clear()
            self.driver.find_element(By.NAME, 'toPort').send_keys('B')
            self.driver.find_element(By.NAME, 'toPort').send_keys(Keys.ENTER)
            self.driver.find_element(By.NAME, 'toPort').clear()
            self.driver.find_element(By.NAME, 'toPort').send_keys(Keys.ENTER)
            self.driver.find_element(By.XPATH, '//div[3]/form/select[1]//option[3]').click()
            self.driver.find_element(By.XPATH, '//div[3]/form/select[2]//option[6]').click()
            self.wnd_mng.switch('0')
            self.driver.execute_script("window.open('some.url');")
            self.wnd_mng.switch('win_ser_local')
            self.wnd_mng.switch('win_ser_1')
            self.wnd_mng.switch('that_window')
            self.wnd_mng.close('1')
            self.wnd_mng.close('win_ser_local')
            self.wnd_mng.close('win_ser_1')
            self.wnd_mng.close('that_window')
            self.driver.find_element(By.NAME, 'toPort').submit()
            self.driver.execute_script("alert('This is Sparta');")
            
            for i in range(10):
                if ((i % 2) == 0):
                    print(i)
            ActionChains(self.driver).drag_and_drop(self.driver.find_element(By.ID, 'address'), self.driver.find_element(By.NAME, 'toPort')).perform()
            self.frm_mng.switch(self.driver.find_element(By.NAME, 'my_frame'))
            self.frm_mng.switch('index=1')
            self.frm_mng.switch('relative=parent')
            
            if self.driver.find_element(By.ID, 'editor').get_attribute('contenteditable'):
                self.driver.execute_script(('arguments[0].innerHTML = %s;' % 'lo-la-lu'), self.driver.find_element(By.ID, 'editor'))
            else:
                raise NoSuchElementException("The element (By.ID, 'editor') is not contenteditable element")
            sleep(3.5)
            self.driver.delete_all_cookies()
            self.driver.find_element(By.LINK_TEXT, 'destination of the week! The Beach!').click()
            
            self.vars['Title'] = self.driver.title
            
            self.vars['Basic'] = self.driver.find_element(By.XPATH, "//*[@id='basics']/h2").get_attribute('innerText')
            
            self.vars['World'] = self.driver.find_element(By.XPATH, "//*[@id='basics']/h1").get_attribute('value')
            
            self.vars['Final'] = '{} {} by {}'.format(self.vars['Title'], self.vars['Basic'], self.vars['By'])
            self.driver.get('http:\\blazemeter.com')
            print(self.vars['red_pill'])
            self.driver.save_screenshot('screen.png')
            
            filename = os.path.join(os.getenv('TAURUS_ARTIFACTS_DIR'), ('screenshot-%d.png' % (time() * 1000)))
            self.driver.save_screenshot(filename)
            body = self.driver.page_source
            re_pattern = re.compile('contained_text')
            self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")

    def _2_empty(self):
        with apiritif.smart_transaction('empty'):
            pass

    def test_locsc(self):
        self._1_()
        self._2_empty()

    def tearDown(self):
        if self.driver:
            self.driver.quit()

