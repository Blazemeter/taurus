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
from collections import OrderedDict
from bzt.resources.selenium_extras import check_opened_new_window, find_element_by_shadow, send_keys, get_locator

class TestLocSc(unittest.TestCase):

    def setUp(self):
        self.vars = {'city_select_name': 'fromPort', 'input_name_id': 'inputName'}

        timeout = 3.5
        self.driver = None
        options = webdriver.ChromeOptions()
        options.add_argument('--no-sandbox')
        options.add_argument('--disable-dev-shm-usage')
        self.driver = webdriver.Chrome(service_log_path='/somewhere/webdriver.log', options=options)
        self.driver.implicitly_wait(timeout)
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows=OrderedDict(), scenario_name='loc_sc')


    def _1_Shadow_locators_test(self):
        with apiritif.smart_transaction('Shadow locators test'):

            var_loc_as = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            self.assertEqual(var_loc_as.get_attribute('innerText').strip(), 'text'.strip())

            var_loc_as = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            self.assertEqual(var_loc_as.get_attribute('innerText').strip(), 'text'.strip())

            var_loc_as = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            self.assertEqual(var_loc_as.get_attribute('value').strip(), 'value'.strip())

            var_loc_as = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            self.assertEqual(var_loc_as.get_attribute('value').strip(), 'value'.strip())

            var_edit_content = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')

            if var_edit_content.get_attribute('contenteditable'):
                self.driver.execute_script(("arguments[0].innerHTML = '%s';" % 'new text'), var_edit_content)
            else:
                raise NoSuchElementException(("The element (shadow: '%s') is not a contenteditable element" % ('c-basic, lightning-accordion-section, .slds-button',)))

            var_edit_content = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')

            if var_edit_content.get_attribute('contenteditable'):
                self.driver.execute_script(("arguments[0].innerHTML = '%s';" % 'new text'), var_edit_content)
            else:
                raise NoSuchElementException(("The element (shadow: '%s') is not a contenteditable element" % ('c-basic, lightning-accordion-section, .slds-button',)))

            var_loc_keys = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            var_loc_keys.click()
            check_opened_new_window()

            var_loc_keys = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            var_loc_keys.click()
            check_opened_new_window()

            var_loc_chain = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).double_click(var_loc_chain).perform()

            var_loc_chain = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).double_click(var_loc_chain).perform()

            var_loc_chain = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).context_click(var_loc_chain).perform()

            var_loc_chain = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).context_click(var_loc_chain).perform()

            var_loc_chain = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).click_and_hold(var_loc_chain).perform()

            var_loc_chain = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).click_and_hold(var_loc_chain).perform()

            var_loc_chain = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).release(var_loc_chain).perform()

            var_loc_chain = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).release(var_loc_chain).perform()

            var_loc_chain = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).move_to_element_with_offset(var_loc_chain, -10, -10).perform()

            var_loc_chain = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).move_to_element_with_offset(var_loc_chain, -10, -10).perform()

            var_loc_chain = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).move_to_element(var_loc_chain).perform()

            var_loc_chain = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).move_to_element(var_loc_chain).perform()

            source = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')

            target = get_locator([{'id': 'id12'}])
            ActionChains(self.driver).drag_and_drop(source, self.driver.find_element(
                target[0],
                target[1])).perform()

            source = get_locator([{'id': 'id34'}])

            target = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).drag_and_drop(self.driver.find_element(
                source[0],
                source[1]), target).perform()

            source = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')

            target = get_locator([{'id': 'id12'}])
            ActionChains(self.driver).drag_and_drop(source, self.driver.find_element(
                target[0],
                target[1])).perform()

            source = get_locator([{'id': 'id34'}])

            target = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            ActionChains(self.driver).drag_and_drop(self.driver.find_element(
                source[0],
                source[1]), target).perform()

            var_loc_select = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            Select(var_loc_select).select_by_visible_text('value')

            var_loc_select = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            Select(var_loc_select).select_by_visible_text('value')

            var_loc_as = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')

            self.vars['my_var'] = var_loc_as.get_attribute('innerText')

            var_loc_as = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')

            self.vars['my_var'] = var_loc_as.get_attribute('innerText')

            var_loc_as = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')

            self.vars['my_var'] = var_loc_as.get_attribute('value')

            var_loc_as = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')

            self.vars['my_var'] = var_loc_as.get_attribute('value')

            var_loc_keys = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            var_loc_keys.clear()
            send_keys(var_loc_keys, 'text')

            var_loc_keys = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            var_loc_keys.clear()
            send_keys(var_loc_keys, 'text')

            var_loc_keys = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            var_loc_keys.submit()

            var_loc_keys = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            var_loc_keys.submit()

            var_loc_keys = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            send_keys(var_loc_keys, Keys.ENTER)

            var_loc_keys = find_element_by_shadow('c-basic, lightning-accordion-section, .slds-button')
            send_keys(var_loc_keys, Keys.ENTER)

    def test_locsc(self):
        self._1_Shadow_locators_test()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
