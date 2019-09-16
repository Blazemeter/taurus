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


def setup():
    options = webdriver.FirefoxOptions()
    profile = webdriver.FirefoxProfile()
    profile.set_preference('webdriver.log.file', '<somewhere>webdriver.log')
    driver = webdriver.Firefox(profile, firefox_options=options)
    driver.implicitly_wait(3.5)
    wnd_mng = WindowManager(driver)
    frm_mng = FrameManager(driver)
    vars = {
        'name': 'Name',
        'red_pill': 'take_it',
    }
    apiritif.put_into_thread_store(vars, driver, wnd_mng, frm_mng)


def teardown():
    (_, driver, _, _) = apiritif.get_from_thread_store()
    driver.quit()


class TestLocSc(unittest.TestCase, ):
    def setUp(self):
        (self.vars, self.driver, self.wnd_mng, self.frm_mng) = apiritif.get_from_thread_store()

    def test_1_(self):
        with apiritif.transaction_logged('/'):
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

    def test_2_empty(self):
        with apiritif.transaction_logged('empty'):
            pass

from selenium.common.exceptions import NoSuchWindowException, NoSuchFrameException

class FrameManager:

    def __init__(self, driver):
        self.driver = driver

    def switch(self, frame_name=None):
        try:
            if ((not frame_name) or (frame_name == 'relative=top')):
                self.driver.switch_to_default_content()
            elif frame_name.startswith('index='):
                self.driver.switch_to.frame(int(frame_name.split('=')[1]))
            elif (frame_name == 'relative=parent'):
                self.driver.switch_to.parent_frame()
            else:
                self.driver.switch_to.frame(frame_name)
        except NoSuchFrameException:
            raise NoSuchFrameException(('Invalid Frame ID: %s' % frame_name))

class WindowManager:

    def __init__(self, driver):
        self.driver = driver
        self.windows = {
            
        }

    def switch(self, window_name=None):
        try:
            if (not window_name):
                self.driver.switch_to.window(self.driver.window_handles[(-1)])
            elif window_name.isdigit():
                self._switch_by_idx(int(window_name))
            elif window_name.startswith('win_ser_'):
                self._switch_by_win_ser(window_name)
            else:
                self.driver.switch_to.window(window_name)
        except NoSuchWindowException:
            raise NoSuchWindowException(('Invalid Window ID: %s' % window_name))

    def _switch_by_idx(self, win_index):
        wnd_handlers = self.driver.window_handles
        if ((len(wnd_handlers) <= win_index) and (win_index >= 0)):
            self.driver.switch_to.window(wnd_handlers[win_index])
        else:
            raise NoSuchWindowException(('Invalid Window ID: %s' % str(win_index)))

    def _switch_by_win_ser(self, window_name):
        if (window_name == 'win_ser_local'):
            wnd_handlers = self.driver.window_handles
            if (len(wnd_handlers) > 0):
                self.driver.switch_to.window(wnd_handlers[0])
            else:
                raise NoSuchWindowException(('Invalid Window ID: %s' % window_name))
        else:
            if (window_name not in self.windows):
                self.windows[window_name] = self.driver.window_handles[(-1)]
            self.driver.switch_to.window(self.windows[window_name])

    def close(self, window_name=None):
        if window_name:
            self.switch(window_name)
        self.driver.close()
