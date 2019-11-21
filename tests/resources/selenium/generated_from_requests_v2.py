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
from bzt.resources.selenium_extras import WindowManager, LocatorsManager, FrameManager


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
            'red_pill': 'take_it,',
        }
        apiritif.put_into_thread_store(func_mode=False, driver=self.driver)

    def _1_Test_V2(self):
        with apiritif.smart_transaction('Test V2'):
            self.driver.get('http://blazedemo.com')
            self.driver.set_window_size('750', '750')
            self.wnd_mng.switch(0)

            try:
                ActionChains(self.driver).click_and_hold(self.driver.find_element(By.ID, 'invalid_id')).perform()

            except NoSuchElementException as nse:
                try:
                    ActionChains(self.driver).click_and_hold(WebDriverWait(self.driver, 0).until(
                        econd.presence_of_element_located((By.XPATH, '/html/body/div[3]/form/select[1]')),
                        '')).perform()

                except TimeoutException:
                    raise nse

            source = self.loc_mng.get_locator([{
                'name': 'invalid_name',
            }, {
                'xpath': '/html/body/div[2]/div/p[2]/a',
            }])

            target = self.loc_mng.get_locator([{
                'css': 'invalid_css',
            }, {
                'xpath': '/html/body/div[3]/form/div',
            }])
            ActionChains(self.driver).drag_and_drop(self.driver.find_element(
                source[0],
                source[1]), self.driver.find_element(
                target[0],
                target[1])).perform()

            try:
                self.assertEqual(self.driver.find_element(By.CSS_SELECTOR, 'myclass').get_attribute('innerText'),
                                 'Choose your departure city:'.strip())

            except NoSuchElementException as nse:
                try:
                    self.assertEqual(WebDriverWait(self.driver, 0).until(
                        econd.presence_of_element_located((By.XPATH, '/html/body/div[3]/h2')), '').get_attribute(
                        'innerText'), 'Choose your departure city:'.strip())

                except TimeoutException:
                    raise nse

            try:
                self.assertEqual(self.driver.find_element(By.CSS_SELECTOR, 'myclass').get_attribute('value'),
                                 'Find Flights'.strip())

            except NoSuchElementException as nse:
                try:
                    self.assertEqual(WebDriverWait(self.driver, 0).until(
                        econd.presence_of_element_located((By.XPATH, '/html/body/div[3]/form/div/input')),
                        '').get_attribute('value'), 'Find Flights'.strip())

                except TimeoutException:
                    raise nse
            self.assertEqual(self.driver.title, 'BlazeDemo')

            self.vars['hEaDeR'] = self.driver.title

            self.vars['Title_Basic_By'] = 'Title_Basic_By'

            self.vars['Basic'] = self.driver.find_element(By.XPATH, '/html/body/div[3]/h2').get_attribute('innerText')

            try:
                self.driver.find_element(By.XPATH, '/wrong/one').click()

            except NoSuchElementException as nse:
                try:
                    WebDriverWait(self.driver, 0).until(
                        econd.presence_of_element_located((By.XPATH, '/html/body/div[3]/form/div/input')), '').click()

                except TimeoutException:
                    raise nse

            try:
                self.driver.find_element(By.XPATH, '/doc/abc').send_keys(Keys.ENTER)

            except NoSuchElementException as nse:
                try:
                    WebDriverWait(self.driver, 0).until(econd.presence_of_element_located((By.CSS_SELECTOR,
                                                                                           'body > div.container > table > tbody > tr:nth-child(1) > td:nth-child(2) > input')),
                                                        '').send_keys(Keys.ENTER)

                except TimeoutException:
                    raise nse

            try:
                self.driver.find_element(By.ID, 'fjkafjk').clear()
                self.driver.find_element(By.ID, 'fjkafjk').send_keys('Havel Jan')

            except NoSuchElementException as nse:
                try:
                    WebDriverWait(self.driver, 0).until(econd.presence_of_element_located((By.CSS_SELECTOR, 'testCss')),
                                                        '').clear()
                    WebDriverWait(self.driver, 0).until(econd.presence_of_element_located((By.CSS_SELECTOR, 'testCss')),
                                                        '').send_keys('Havel Jan')

                except TimeoutException:
                    try:
                        WebDriverWait(self.driver, 0).until(
                            econd.presence_of_element_located((By.CSS_SELECTOR, 'another')), '').clear()
                        WebDriverWait(self.driver, 0).until(
                            econd.presence_of_element_located((By.CSS_SELECTOR, 'another')), '').send_keys('Havel Jan')

                    except TimeoutException:
                        try:
                            WebDriverWait(self.driver, 0).until(
                                econd.presence_of_element_located((By.XPATH, '/invalid/xpath')), '').clear()
                            WebDriverWait(self.driver, 0).until(
                                econd.presence_of_element_located((By.XPATH, '/invalid/xpath')), '').send_keys(
                                'Havel Jan')

                        except TimeoutException:
                            try:
                                WebDriverWait(self.driver, 0).until(
                                    econd.presence_of_element_located((By.ID, 'inputName')), '').clear()
                                WebDriverWait(self.driver, 0).until(
                                    econd.presence_of_element_located((By.ID, 'inputName')), '').send_keys('Havel Jan')

                            except TimeoutException:
                                raise nse

            try:
                Select(self.driver.find_element(By.CSS_SELECTOR, 'myclass')).select_by_visible_text('American Express')

            except NoSuchElementException as nse:
                try:
                    Select(WebDriverWait(self.driver, 0).until(
                        econd.presence_of_element_located((By.XPATH, '//*[@id="cardType"]')),
                        '')).select_by_visible_text('American Express')

                except TimeoutException:
                    raise nse
            self.driver.execute_script('window.scrollTo(0, document.body.scrollHeight);')

            print(self.vars['red_pill'])
            sleep(4.6)
            self.driver.delete_all_cookies()
            self.driver.save_screenshot('screen.png')

            filename = os.path.join(os.getenv('TAURUS_ARTIFACTS_DIR'), ('screenshot-%d.png' % (time() * 1000)))
            self.driver.save_screenshot(filename)

            try:
                WebDriverWait(self.driver, 3.5).until(
                    econd.visibility_of_element_located((By.CSS_SELECTOR, 'invalid_css')),
                    "Element 'css': 'invalid_css' failed to appear within 3.5s")

            except TimeoutException:
                WebDriverWait(self.driver, 3.5).until(econd.visibility_of_element_located((By.NAME, 'inputName')),
                                                      "Element 'name': 'inputName' failed to appear within 3.5s")

            edit_content_loc = self.loc_mng.get_locator([{
                'id': 'editor',
            }])

            if self.driver.find_element(
                    edit_content_loc[0],
                    edit_content_loc[1]).get_attribute('contenteditable'):
                self.driver.execute_script(("arguments[0].innerHTML = '%s';" % 'lo-la-lu'), self.driver.find_element(
                    edit_content_loc[0],
                    edit_content_loc[1]))
            else:
                raise NoSuchElementException(('The element (%s: %r) is not a contenteditable element' % (
                    edit_content_loc[0],
                    edit_content_loc[1])))
            sleep(4.6)
            self.driver.delete_all_cookies()
            self.driver.save_screenshot('screen.png')

            filename = os.path.join(os.getenv('TAURUS_ARTIFACTS_DIR'), ('screenshot-%d.png' % (time() * 1000)))
            self.driver.save_screenshot(filename)
            self.driver.execute_script("window.open('vacation.html');")
            self.driver.maximize_window()
            self.frm_mng.switch('index=1')
            self.frm_mng.switch('relative=parent')
            self.frm_mng.switch(self.driver.find_element(By.NAME, 'my_frame'))
            self.wnd_mng.close()

    def test_locsc(self):
        self._1_Test_V2()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
