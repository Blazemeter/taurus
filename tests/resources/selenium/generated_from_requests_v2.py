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
from bzt.resources.selenium_extras import close_window, dialogs_answer_on_next_alert, dialogs_get_next_alert, \
    dialogs_get_next_confirm, switch_window, wait_for, dialogs_get_next_prompt, get_locator, switch_frame, \
    dialogs_answer_on_next_confirm, dialogs_answer_on_next_prompt, dialogs_replace


class TestLocSc(unittest.TestCase):

    def setUp(self):
        self.vars = {'my_xpath_locator': '/html/body/div[3]', 'name': 'Name', 'red_pill': 'take_it,'}

        timeout = 3.5
        self.driver = None
        options = webdriver.FirefoxOptions()
        options.set_preference('network.proxy.type', 4)
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '/somewhere/webdriver.log')
        self.driver = webdriver.Firefox(profile, options=options)
        self.driver.implicitly_wait(timeout)
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={},
                                       scenario_name='loc_sc')

    def _1_Test_V2(self):
        with apiritif.smart_transaction('Test V2'):
            self.driver.get('http://blazedemo.com')

            dialogs_replace()
            self.driver.set_window_size('750', '750')
            switch_window(0)

            var_loc_chain = get_locator([{'id': 'invalid_id'}, {'xpath': self.vars['my_xpath_locator']}])
            ActionChains(self.driver).click_and_hold(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()

            var_loc_chain = get_locator([{'id': 'id_123'}])
            ActionChains(self.driver).move_to_element_with_offset(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1]), -10, -10).perform()

            var_loc_chain = get_locator([{'name': 'name_123'}])
            ActionChains(self.driver).move_to_element(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()

            source = get_locator([{'name': 'invalid_name'}, {'xpath': '/html/body/div[2]/div/p[2]/a'}])

            target = get_locator([{'css': 'invalid_css'}, {'xpath': '/html/body/div[3]/form/div'}])
            ActionChains(self.driver).drag_and_drop(self.driver.find_element(
                source[0],
                source[1]), self.driver.find_element(
                target[0],
                target[1])).perform()

            var_loc_as = get_locator([{'css': 'myclass'}, {'xpath': '/html/body/div[3]/h2'}])
            self.assertEqual(self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('innerText').strip(), 'Choose your departure city:'.strip())

            var_loc_as = get_locator([{'css': 'myclass'}, {'xpath': '/html/body/div[3]/form/div/input'}])
            self.assertEqual(self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('value').strip(), 'Find Flights'.strip())
            self.assertEqual(self.driver.title, 'BlazeDemo')

            self.vars['hEaDeR'] = self.driver.title

            self.vars['final_var'] = 'test_text'

            var_loc_as = get_locator([{'xpath': '/html/body/div[3]/h2'}])

            self.vars['Basic'] = self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('innerText')
            self.assertTrue(self.driver.execute_script('return 10 === 2*5;'), '10 === 2*5')

            self.vars['var_eval'] = self.driver.execute_script('return 0 == false;')

            var_loc_keys = get_locator([{'xpath': '/wrong/one'}, {'xpath': '/html/body/div[3]/form/div/input'}])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).click()

            var_loc_keys = get_locator([{'xpath': '/doc/abc'}, {
                'css': 'body > div.container > table > tbody > tr:nth-child(1) > td:nth-child(2) > input'}])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys(Keys.ENTER)

            var_loc_keys = get_locator([{'id': 'fjkafjk'}, {'css': 'testCss'}])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).clear()
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys('myusername')

            var_loc_select = get_locator([{'css': 'myclass'}, {'xpath': '//*[@id="cardType"]'}])
            Select(self.driver.find_element(
                var_loc_select[0],
                var_loc_select[1])).select_by_visible_text('American Express')
            self.driver.execute_script('window.scrollTo(0, document.body.scrollHeight);')

            for i in range(10):
                if ((i % 2) == 0):
                    print(i)
            print(self.vars['red_pill'])
            sleep(4.6)
            self.driver.delete_all_cookies()
            self.driver.save_screenshot('screen.png')

            filename = os.path.join(os.getenv('TAURUS_ARTIFACTS_DIR'), ('screenshot-%d.png' % (time() * 1000)))
            self.driver.save_screenshot(filename)
            wait_for('visible', [{'css': 'invalid_css'}, {'name': 'inputName'}], 3.5)
            wait_for('visible', [{'css': 'invalid_css'}, {'name': 'inputName'}], 9020.0)

            var_edit_content = get_locator([{'id': 'editor'}])

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
            sleep(4.6)
            self.driver.delete_all_cookies()
            self.driver.save_screenshot('screen.png')

            filename = os.path.join(os.getenv('TAURUS_ARTIFACTS_DIR'), ('screenshot-%d.png' % (time() * 1000)))
            self.driver.save_screenshot(filename)
            self.driver.execute_script("window.open('vacation.html');")
            self.driver.maximize_window()
            switch_frame('index=1')
            switch_frame('relative=parent')
            switch_frame(self.driver.find_element(By.NAME, 'my_frame'))
            close_window()
            dialogs_answer_on_next_prompt('my input')
            dialogs_answer_on_next_confirm('#Ok')
            dialogs_answer_on_next_alert('#Ok')

            dialog = dialogs_get_next_alert()
            self.assertIsNotNone(dialog, 'No dialog of type alert appeared')
            self.assertEqual(dialog, 'Exception occurred!', "Dialog message didn't match")

            dialog = dialogs_get_next_confirm()
            self.assertIsNotNone(dialog, 'No dialog of type confirm appeared')
            self.assertEqual(dialog, 'Are you sure?', "Dialog message didn't match")

            dialog = dialogs_get_next_prompt()
            self.assertIsNotNone(dialog, 'No dialog of type prompt appeared')
            self.assertEqual(dialog, 'What is your age?', "Dialog message didn't match")

    def test_locsc(self):
        self._1_Test_V2()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
