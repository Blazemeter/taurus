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
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.common.options import ArgOptions
from bzt.resources.selenium_extras import find_element_by_shadow, waiter, wait_for, get_locator

class TestLocSc(unittest.TestCase):

    def setUp(self):
        self.vars = {'button_name': 'test_btn', 'city_select_name': 'fromPort', 'input_name_id': 'inputName'}

        timeout = 3.5
        options = webdriver.ChromeOptions()
        options.add_argument('--no-sandbox')
        options.add_argument('--disable-dev-shm-usage')
        options.add_argument('--disable-gpu')
        options.set_capability('unhandledPromptBehavior', 'ignore')
        service = Service(service_args=['--log-path=/somewhere/webdriver.log'])
        self.driver = webdriver.Chrome(service=service, options=options)
        self.driver.implicitly_wait(timeout)
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={},
                                       scenario_name='loc_sc')


    def _1_Shadow_locators_test(self):
        with apiritif.smart_transaction('Shadow locators test'):

            self.assertEqual(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').get_attribute('innerText').strip(), 'text'.strip())

            self.assertEqual(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button[name={}]'.format(self.vars['button_name'])).get_attribute('innerText').strip(), 'text'.strip())

            self.assertEqual(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').get_attribute('value').strip(), 'value'.strip())

            self.assertEqual(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').get_attribute('value').strip(), 'value'.strip())


            if find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').get_attribute('contenteditable'):
                self.driver.execute_script(("arguments[0].innerHTML = '%s';" % 'new text'), find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button'))
            else:
                raise NoSuchElementException(("The element (shadow: '%s') is not a contenteditable element" % ('c-basic, lightning-accordion-section, .slds-button',)))


            if find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').get_attribute('contenteditable'):
                self.driver.execute_script(("arguments[0].innerHTML = '%s';" % 'new text'), find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button'))
            else:
                raise NoSuchElementException(("The element (shadow: '%s') is not a contenteditable element" % ('c-basic, lightning-accordion-section, .slds-button',)))

            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').click()
            waiter()

            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').click()
            waiter()

            ActionChains(self.driver).double_click(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).perform()
            waiter()

            ActionChains(self.driver).double_click(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).perform()
            waiter()

            ActionChains(self.driver).context_click(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).perform()
            waiter()

            ActionChains(self.driver).context_click(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).perform()
            waiter()

            ActionChains(self.driver).click_and_hold(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).perform()

            ActionChains(self.driver).click_and_hold(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).perform()

            ActionChains(self.driver).release(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).perform()

            ActionChains(self.driver).release(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).perform()

            ActionChains(self.driver).move_to_element_with_offset(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button'), -10, -10).perform()

            ActionChains(self.driver).move_to_element_with_offset(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button'), -10, -10).perform()

            ActionChains(self.driver).move_to_element(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).perform()

            ActionChains(self.driver).move_to_element(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).perform()


            target = get_locator([{'id': 'id12'}])
            ActionChains(self.driver).drag_and_drop(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button'), self.driver.find_element(
                target[0],
                target[1])).perform()
            waiter()

            source = get_locator([{'id': 'id34'}])

            ActionChains(self.driver).drag_and_drop(self.driver.find_element(
                source[0],
                source[1]), find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).perform()
            waiter()


            target = get_locator([{'id': 'id12'}])
            ActionChains(self.driver).drag_and_drop(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button'), self.driver.find_element(
                target[0],
                target[1])).perform()
            waiter()

            source = get_locator([{'id': 'id34'}])

            ActionChains(self.driver).drag_and_drop(self.driver.find_element(
                source[0],
                source[1]), find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).perform()
            waiter()

            Select(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).select_by_visible_text('value')
            waiter()

            Select(find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')).select_by_visible_text('value')
            waiter()


            self.vars['my_var'] = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').get_attribute('innerText')


            self.vars['my_var'] = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').get_attribute('innerText')


            self.vars['my_var'] = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').get_attribute('value')


            self.vars['my_var'] = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').get_attribute('value')

            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').clear()
            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').send_keys('text')
            waiter()

            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').clear()
            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').send_keys('text')
            waiter()

            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').clear()
            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').send_keys('password')

            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').clear()
            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').send_keys('password')

            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').submit()

            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').submit()

            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').send_keys(Keys.ENTER)

            find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button').send_keys(Keys.ENTER)
            wait_for('visible', [{'shadow': 'toPort'}], 5.0)
            wait_for('notvisible', [{'shadow': 'c-basic, lightning-accordion-section, .slds-button'}], 2.0)

    def test_locsc(self):
        self._1_Shadow_locators_test()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
