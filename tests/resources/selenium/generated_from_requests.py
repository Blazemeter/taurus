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
from selenium.common.exceptions import NoSuchElementException, TimeoutException
from selenium.webdriver.common.by import By
from selenium.webdriver.common.action_chains import ActionChains
from selenium.webdriver.support.ui import Select
from selenium.webdriver.support import expected_conditions as econd
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.common.keys import Keys
from bzt.resources.selenium_extras import FrameManager, WindowManager, LocatorsManager


class TestLocSc(unittest.TestCase):

    def setUp(self):
        self.driver = None
        options = webdriver.FirefoxOptions()
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '<somewhere>webdriver.log')
        self.driver = webdriver.Firefox(profile, firefox_options=options)
        self.driver.implicitly_wait(3.5)
        self.wnd_mng = WindowManager(self.driver)
        self.frm_mng = FrameManager(self.driver)
        self.loc_mng = LocatorsManager(self.driver)
        self.vars = {
            'name': 'Name',
            'red_pill': 'take_it',
        }
        apiritif.put_into_thread_store(func_mode=False, driver=self.driver)

    def _1_(self):
        with apiritif.smart_transaction('/'):
            self.driver.get('http://blazedemo.com/')

            var_loc_wait = self.loc_mng.get_locator([{
                'xpath': "//input[@type='submit']",
            }])
            WebDriverWait(self.driver, 3.5).until(econd.presence_of_element_located((
                var_loc_wait[0],
                var_loc_wait[1])), 'Element \'xpath\':"//input[@type=\'submit\']" failed to appear within 3.5s')
            self.assertEqual(self.driver.title, 'BlazeDemo')

            var_loc_chain = self.loc_mng.get_locator([{
                'xpath': '/html/body/div[2]/div/p[2]/a',
            }])
            ActionChains(self.driver).move_to_element(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()

            var_loc_chain = self.loc_mng.get_locator([{
                'xpath': '/html/body/div[3]/h2',
            }])
            ActionChains(self.driver).double_click(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()

            var_loc_chain = self.loc_mng.get_locator([{
                'xpath': '/html/body/div[3]/form/select[1]',
            }])
            ActionChains(self.driver).click_and_hold(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()

            var_loc_chain = self.loc_mng.get_locator([{
                'xpath': '/html/body/div[3]/form/select[1]/option[6]',
            }])
            ActionChains(self.driver).release(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()

            var_loc_select = self.loc_mng.get_locator([{
                'name': 'toPort',
            }])
            Select(self.driver.find_element(
                var_loc_select[0],
                var_loc_select[1])).select_by_visible_text('London')

            var_loc_keys = self.loc_mng.get_locator([{
                'css': 'body input.btn.btn-primary',
            }])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys(Keys.ENTER)

            var_loc_as = self.loc_mng.get_locator([{
                'id': 'address',
            }])
            self.assertEqual(self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('value').strip(), '123 Beautiful st.'.strip())

            var_loc_as = self.loc_mng.get_locator([{
                'xpath': '/html/body/div[2]/form/div[1]/label',
            }])
            self.assertEqual(self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('innerText').strip(), self.vars['name'].strip())

            var_loc_wait = self.loc_mng.get_locator([{
                'name': 'toPort',
            }])
            WebDriverWait(self.driver, 3.5).until(econd.visibility_of_element_located((
                var_loc_wait[0],
                var_loc_wait[1])), "Element 'name':'toPort' failed to appear within 3.5s")

            var_loc_keys = self.loc_mng.get_locator([{
                'name': 'toPort',
            }])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys('B')

            var_loc_keys = self.loc_mng.get_locator([{
                'name': 'toPort',
            }])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).clear()
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys('B')

            var_loc_keys = self.loc_mng.get_locator([{
                'name': 'toPort',
            }])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys(Keys.ENTER)

            var_loc_keys = self.loc_mng.get_locator([{
                'name': 'toPort',
            }])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).clear()
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys(Keys.ENTER)

            var_loc_keys = self.loc_mng.get_locator([{
                'xpath': '//div[3]/form/select[1]//option[3]',
            }])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).click()

            var_loc_keys = self.loc_mng.get_locator([{
                'xpath': '//div[3]/form/select[2]//option[6]',
            }])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).click()
            self.wnd_mng.switch('0')
            self.driver.execute_script("window.open('some.url');")
            self.wnd_mng.switch('win_ser_local')
            self.wnd_mng.switch('win_ser_1')
            self.wnd_mng.switch('that_window')
            self.wnd_mng.close('1')
            self.wnd_mng.close('win_ser_local')
            self.wnd_mng.close('win_ser_1')
            self.wnd_mng.close('that_window')

            var_loc_keys = self.loc_mng.get_locator([{
                'name': 'toPort',
            }])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).submit()
            self.driver.execute_script("alert('This is Sparta');")

            for i in range(10):
                if ((i % 2) == 0):
                    print(i)

            source = self.loc_mng.get_locator([{
                'id': 'address',
            }])

            target = self.loc_mng.get_locator([{
                'name': 'toPort',
            }])
            ActionChains(self.driver).drag_and_drop(self.driver.find_element(
                source[0],
                source[1]), self.driver.find_element(
                target[0],
                target[1])).perform()
            self.frm_mng.switch(self.driver.find_element(By.NAME, 'my_frame'))
            self.frm_mng.switch('index=1')
            self.frm_mng.switch('relative=parent')

            var_edit_content = self.loc_mng.get_locator([{
                'id': 'editor',
            }])

            if self.driver.find_element(
                    var_edit_content[0],
                    var_edit_content[1]).get_attribute('contenteditable'):
                self.driver.execute_script(("arguments[0].innerHTML = '%s';" % 'lo-la-lu'), self.driver.find_element(
                    var_edit_content[0],
                    var_edit_content[1]))
            else:
                raise NoSuchElementException(('The element (%s: %r) is not a contenteditable element' % (
                    var_edit_content[0],
                    var_edit_content[1])))
            sleep(3.5)
            self.driver.delete_all_cookies()

            var_loc_keys = self.loc_mng.get_locator([{
                'linktext': 'destination of the week! The Beach!',
            }])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).click()

            self.vars['Title'] = self.driver.title

            var_loc_as = self.loc_mng.get_locator([{
                'xpath': "//*[@id='basics']/h2",
            }])

            self.vars['Basic'] = self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('innerText')

            var_loc_as = self.loc_mng.get_locator([{
                'xpath': "//*[@id='basics']/h1",
            }])

            self.vars['World'] = self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('value')

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
