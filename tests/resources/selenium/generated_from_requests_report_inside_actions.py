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
from selenium.webdriver.common.options import ArgOptions
from bzt.resources.selenium_extras import dialogs_answer_on_next_confirm, open_window, close_window, switch_frame, get_loop_range, get_locator, wait_for, dialogs_answer_on_next_alert, waiter, dialogs_get_next_prompt, dialogs_get_next_alert, dialogs_answer_on_next_prompt, dialogs_replace, switch_window, dialogs_get_next_confirm
reader_1 = apiritif.CSVReaderPerThread('first.csv', loop=True)
reader_2 = apiritif.CSVReaderPerThread('second.csv', loop=False)


class TestLocSc(unittest.TestCase):

    def setUp(self):
        self.vars = {'name': 'Name', 'red_pill': 'take_it'}
        reader_1.read_vars()
        reader_2.read_vars()
        self.vars.update(reader_1.get_vars())
        self.vars.update(reader_2.get_vars())
 
        timeout = 3.5
        options = webdriver.FirefoxOptions()
        from selenium.webdriver.remote.remote_connection import RemoteConnection
        import copy
        _original_execute = RemoteConnection.execute

        def execute_with_retries(self, command, params=None):
            params_copy = copy.deepcopy(params)
            retries = 3
            delay = 2
            last_exc = None
            for attempt in range(retries):
                try:
                    if (params != params_copy):
                        return _original_execute(self, command, params_copy)
                    else:
                        return _original_execute(self, command, params)
                except Exception as e:
                    last_exc = e
                    print(f'[Retry] RemoteConnection.execute failed on attempt {(attempt + 1)}: {e}')
                    sleep(delay)
            raise last_exc
        RemoteConnection.execute = execute_with_retries
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '/somewhere/webdriver.log')
        options.set_capability('unhandledPromptBehavior', 'ignore')
        self.driver = webdriver.Firefox(profile, options=options)
        self.driver.implicitly_wait(timeout)
        apiritif.put_into_thread_store(timeout=timeout, func_mode=True, driver=self.driver, windows={},
                                       scenario_name='loc_sc', data_sources=True)
    

    def _1_(self):
        with apiritif.smart_transaction('/'):
            self.driver.get('http://blazedemo.com/')
            dialogs_replace()
            with apiritif.transaction('/_01_waitfor_present'):
                wait_for('present', [{'xpath': "//input[@type='submit']"}], 3.5)
            with apiritif.transaction('/_02_waitfor_present'):
                wait_for('present', [{'xpath': "//input[@name='test,name']"}], 80.0)
            with apiritif.transaction('/_03_assert_BlazeDemo'):
                self.assertEqual(self.driver.title, 'BlazeDemo')
            with apiritif.transaction('/_04_mousemove_None'):

                var_loc_chain = get_locator([{'xpath': '/html/body/div[2]/div/p[2]/a'}])
                ActionChains(self.driver).move_to_element(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()
            with apiritif.transaction('/_05_doubleclick_None'):

                var_loc_chain = get_locator([{'xpath': '/html/body/div[3]/h2'}])
                ActionChains(self.driver).double_click(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()
                waiter()
            with apiritif.transaction('/_06_contextclick_None'):

                var_loc_chain = get_locator([{'xpath': '/html/body/div[3]/form/select[1]'}])
                ActionChains(self.driver).context_click(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()
                waiter()
            with apiritif.transaction('/_07_mousedown_None'):

                var_loc_chain = get_locator([{'xpath': '/html/body/div[3]/form/select[1]'}])
                ActionChains(self.driver).click_and_hold(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()
            with apiritif.transaction('/_08_mouseup_None'):

                var_loc_chain = get_locator([{'xpath': '/html/body/div[3]/form/select[1]/option[6]'}])
                ActionChains(self.driver).release(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()
            with apiritif.transaction('/_09_select_London'):

                var_loc_select = get_locator([{'name': 'toPort'}])
                Select(self.driver.find_element(
                var_loc_select[0],
                var_loc_select[1])).select_by_visible_text('London')
                waiter()
            with apiritif.transaction('/_10_keys_KEY_ENTER'):

                var_loc_keys = get_locator([{'css': 'body input.btn.btn-primary'}])
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys(Keys.ENTER)
            with apiritif.transaction('/_11_assertvalue_123_Beautiful_st.'):

                var_loc_as = get_locator([{'id': 'address'}])
                self.assertEqual(self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('value').strip(), '123 Beautiful st.'.strip())
            with apiritif.transaction('/_12_asserttext__name'):

                var_loc_as = get_locator([{'xpath': '/html/body/div[2]/form/div[1]/label'}])
                self.assertEqual(self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('innerText').strip(), self.vars['name'].strip())
            with apiritif.transaction('/_13_waitfor_visible'):
                wait_for('visible', [{'name': 'toPort'}], 3.5)
            with apiritif.transaction('/_14_keys_B'):

                var_loc_keys = get_locator([{'name': 'toPort'}])
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys('B')
            with apiritif.transaction('/_15_type_B'):

                var_loc_keys = get_locator([{'name': 'toPort'}])
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).clear()
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys('B')
                waiter()
            with apiritif.transaction('/_16_keys_KEY_ENTER'):

                var_loc_keys = get_locator([{'name': 'toPort'}])
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys(Keys.ENTER)
            with apiritif.transaction('/_17_type_KEY_ENTER'):

                var_loc_keys = get_locator([{'name': 'toPort'}])
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).clear()
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys(Keys.ENTER)
                waiter()
            with apiritif.transaction('/_18_type_mypassword'):

                var_loc_keys = get_locator([{'name': 'toPort'}])
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).clear()
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys('mypassword')
                waiter()
            with apiritif.transaction('/_19_click_None'):

                var_loc_keys = get_locator([{'xpath': '//div[3]/form/select[1]//option[3]'}])
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).click()
                waiter()
            with apiritif.transaction('/_20_click_None'):

                var_loc_keys = get_locator([{'xpath': '//div[3]/form/select[2]//option[6]'}])
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).click()
                waiter()
            with apiritif.transaction('/_21_switch_0'):
                switch_window('0')
            with apiritif.transaction('/_22_open_some.url'):
                open_window('some.url')
            with apiritif.transaction('/_23_switch_win_ser_local'):
                switch_window('win_ser_local')
            with apiritif.transaction('/_24_switch_win_ser_1'):
                switch_window('win_ser_1')
            with apiritif.transaction('/_25_switch_that_window'):
                switch_window('that_window')
            with apiritif.transaction('/_26_close_1'):
                close_window('1')
            with apiritif.transaction('/_27_close_win_ser_local'):
                close_window('win_ser_local')
            with apiritif.transaction('/_28_close_win_ser_1'):
                close_window('win_ser_1')
            with apiritif.transaction('/_29_close_that_window'):
                close_window('that_window')
            with apiritif.transaction('/_30_submit_None'):

                var_loc_keys = get_locator([{'name': 'toPort'}])
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).submit()
            with apiritif.transaction('/_31_script_alert(This_is_Sparta)'):
                self.driver.execute_script("alert('This is Sparta');")
                waiter()
            with apiritif.transaction('/_32_script_alert(This_is__red_pill)'):
                self.driver.execute_script("{{alert('This is {}');}}".format(self.vars['red_pill']))
                waiter()
            with apiritif.transaction('/_33_rawcode_for_i_in_range(10):__if_i__2_==_'):

                for i in range(10):
                    if ((i % 2) == 0):
                        print(i)
            with apiritif.transaction('/_34_drag_elementByName(toPort)'):

                source = get_locator([{'id': 'address'}])

                target = get_locator([{'name': 'toPort'}])
                ActionChains(self.driver).drag_and_drop(self.driver.find_element(
                source[0],
                source[1]), self.driver.find_element(
                target[0],
                target[1])).perform()
                waiter()
            with apiritif.transaction('/_35_switchframe_my_frame'):
                switch_frame(self.driver.find_element(By.NAME, 'my_frame'))
            with apiritif.transaction('/_36_switchframe_top_frame'):
                switch_frame(self.driver.find_element(By.NAME, 'top_frame'))
            with apiritif.transaction('/_37_switchframe_//id=result'):
                switch_frame(self.driver.find_element(By.XPATH, "//*[@id='result']"))
            with apiritif.transaction('/_38_switchframe_.my_class'):
                switch_frame(self.driver.find_element(By.CSS_SELECTOR, '.my_class'))
            with apiritif.transaction('/_39_switchframe_frame_id'):
                switch_frame(self.driver.find_element(By.ID, 'frame_id'))
            with apiritif.transaction('/_40_switchframe_1'):
                switch_frame('index=1')
            with apiritif.transaction('/_41_switchframe_relative=parent'):
                switch_frame('relative=parent')
            with apiritif.transaction('/_42_editcontent_lo-la-lu'):

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
            with apiritif.transaction('/_43_pausefor_3.5s'):
                sleep(3.5)
            with apiritif.transaction('/_44_clear_None'):
                self.driver.delete_all_cookies()
            with apiritif.transaction('/_45_click_None'):

                var_loc_keys = get_locator([{'linktext': 'destination of the week! The Beach!'}])
                self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).click()
                waiter()
            with apiritif.transaction('/_46_store_Title'):

                self.vars['Title'] = self.driver.title
            with apiritif.transaction('/_47_storetext_Basic'):

                var_loc_as = get_locator([{'xpath': "//*[@id='basics']/h2"}])

                self.vars['Basic'] = self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('innerText')
            with apiritif.transaction('/_48_storevalue_World'):

                var_loc_as = get_locator([{'xpath': "//*[@id='basics']/h1"}])

                self.vars['World'] = self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('value')
            with apiritif.transaction('/_49_store_Final'):

                self.vars['Final'] = '{} {} by {}'.format(self.vars['Title'], self.vars['Basic'], self.vars['By'])
            with apiritif.transaction('/_50_store_var_eval'):

                self.vars['var_eval'] = self.driver.execute_script('return 0 == false;')
            with apiritif.transaction('/_51_assert_10_===_25'):
                self.assertTrue(self.driver.execute_script('return 10 === 2*5;'), '10 === 2*5')
            with apiritif.transaction('/_52_go_http:blazemeter.com'):
                self.driver.get('http:\\blazemeter.com')

                dialogs_replace()
                waiter()
            with apiritif.transaction('/_53_assertdialog_alert'):

                dialog = dialogs_get_next_alert()
                self.assertIsNotNone(dialog, 'No dialog of type alert appeared')
                self.assertEqual(dialog, 'Alert Message', "Dialog message didn't match")
            with apiritif.transaction('/_54_assertdialog_prompt'):

                dialog = dialogs_get_next_prompt()
                self.assertIsNotNone(dialog, 'No dialog of type prompt appeared')
                self.assertEqual(dialog, 'Enter value', "Dialog message didn't match")
            with apiritif.transaction('/_55_assertdialog_confirm'):

                dialog = dialogs_get_next_confirm()
                self.assertIsNotNone(dialog, 'No dialog of type confirm appeared')
                self.assertEqual(dialog, 'Are you sure?', "Dialog message didn't match")
            with apiritif.transaction('/_56_answerdialog_prompt'):
                dialogs_answer_on_next_prompt('myvalue')
            with apiritif.transaction('/_57_answerdialog_confirm'):
                dialogs_answer_on_next_confirm('#Ok')
            with apiritif.transaction('/_58_answerdialog_alert'):
                dialogs_answer_on_next_alert('#Ok')
            for i in get_loop_range(1, 1, 1):
                self.vars['i'] = str(i)
                with apiritif.transaction('/_59_1_echo__red_pill'):
                    print(self.vars['red_pill'])
            with apiritif.transaction('/_60_screenshot_screen.png'):
                self.driver.save_screenshot('screen.png')
            with apiritif.transaction('/_61_screenshot_None'):

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
