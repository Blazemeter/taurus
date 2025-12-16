# coding=utf-8

import logging
import random
import string
import sys
import unittest
from time import time, sleep

import apiritif

import traceback
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
from bzt.resources.selenium_extras import dialogs_answer_on_next_confirm, dialogs_get_next_prompt, dialogs_get_next_alert, dialogs_answer_on_next_alert, wait_for, action_start, get_locator, action_end, waiter, dialogs_answer_on_next_prompt, dialogs_replace, switch_frame, open_window, close_window, dialogs_get_next_confirm, switch_window

class TestLocSc(unittest.TestCase):

    def setUp(self):
        self.driver = None
        action_start({'param': {}, 'type': 'new_session', 'value': None})
        try:
            self.vars = {'my_xpath_locator': '/html/body/div[3]', 'name': 'Name', 'pos': 100, 'red_pill': 'take_it,'}

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
            apiritif.put_into_thread_store(timeout=timeout, func_mode=False, driver=self.driver, windows={},
                                           scenario_name='loc_sc')
        except Exception:
            (ex_type, ex, tb) = sys.exc_info()
            action_end({'message': str(traceback.format_exception(ex_type, ex, tb)), 'param': {}, 'type': 'new_session'})
            apiritif.log.error(str(traceback.format_exception(ex_type, ex, tb)))
            raise
        action_end({'param': {}, 'type': 'new_session'})


    def _1_Test_V2(self):
        with apiritif.smart_transaction('Test V2'):
            action_start({'param': 'http://blazedemo.com', 'selectors': [], 'tag': '', 'type': 'go', 'value': None})
            self.driver.get('http://blazedemo.com')

            dialogs_replace()
            waiter()
            action_end({'param': 'http://blazedemo.com', 'selectors': [], 'tag': '', 'type': 'go', 'value': None})
            action_start({'param': '750, 750', 'selectors': [], 'tag': 'window', 'type': 'resize', 'value': None})
            self.driver.set_window_size('750', '750')
            action_end({'param': '750, 750', 'selectors': [], 'tag': 'window', 'type': 'resize', 'value': None})
            action_start({'param': 0, 'selectors': [], 'tag': 'window', 'type': 'switch', 'value': None})
            switch_window(0)
            action_end({'param': 0, 'selectors': [], 'tag': 'window', 'type': 'switch', 'value': None})
            action_start({'param': None, 'selectors': [{'id': 'invalid_id'}, {'xpath': self.vars['my_xpath_locator']}], 'tag': '', 'type': 'mousedown', 'value': None})

            var_loc_chain = get_locator([{'id': 'invalid_id'}, {'xpath': self.vars['my_xpath_locator']}])
            ActionChains(self.driver).click_and_hold(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()
            action_end({'param': None, 'selectors': [{'id': 'invalid_id'}, {'xpath': self.vars['my_xpath_locator']}], 'tag': '', 'type': 'mousedown', 'value': None})
            action_start({'param': None, 'selectors': [{'id': 'id_123'}], 'tag': '', 'type': 'mouseout', 'value': None})

            var_loc_chain = get_locator([{'id': 'id_123'}])
            ActionChains(self.driver).move_to_element_with_offset(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1]), -10, -10).perform()
            action_end({'param': None, 'selectors': [{'id': 'id_123'}], 'tag': '', 'type': 'mouseout', 'value': None})
            action_start({'param': None, 'selectors': [{'name': 'name_123'}], 'tag': '', 'type': 'mouseover', 'value': None})

            var_loc_chain = get_locator([{'name': 'name_123'}])
            ActionChains(self.driver).move_to_element(self.driver.find_element(
                var_loc_chain[0],
                var_loc_chain[1])).perform()
            action_end({'param': None, 'selectors': [{'name': 'name_123'}], 'tag': '', 'type': 'mouseover', 'value': None})
            action_start({'param': None, 'selectors': ([{'name': 'invalid_name'}, {'xpath': '/html/body/div[2]/div/p[2]/a'}], [{'css': 'invalid_css'}, {'xpath': '/html/body/div[3]/form/div'}]), 'tag': '', 'type': 'drag', 'value': None})

            source = get_locator([{'name': 'invalid_name'}, {'xpath': '/html/body/div[2]/div/p[2]/a'}])

            target = get_locator([{'css': 'invalid_css'}, {'xpath': '/html/body/div[3]/form/div'}])
            ActionChains(self.driver).drag_and_drop(self.driver.find_element(
                source[0],
                source[1]), self.driver.find_element(
                target[0],
                target[1])).perform()
            waiter()
            action_end({'param': None, 'selectors': ([{'name': 'invalid_name'}, {'xpath': '/html/body/div[2]/div/p[2]/a'}], [{'css': 'invalid_css'}, {'xpath': '/html/body/div[3]/form/div'}]), 'tag': '', 'type': 'drag', 'value': None})
            action_start({'param': 'Choose your departure city:', 'selectors': [{'css': 'myclass'}, {'xpath': '/html/body/div[3]/h2'}], 'tag': '', 'type': 'asserttext', 'value': None})

            var_loc_as = get_locator([{'css': 'myclass'}, {'xpath': '/html/body/div[3]/h2'}])
            self.assertEqual(self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('innerText').strip(), 'Choose your departure city:'.strip())
            action_end({'param': 'Choose your departure city:', 'selectors': [{'css': 'myclass'}, {'xpath': '/html/body/div[3]/h2'}], 'tag': '', 'type': 'asserttext', 'value': None})
            action_start({'param': 'Find Flights', 'selectors': [{'css': 'myclass'}, {'xpath': '/html/body/div[3]/form/div/input'}], 'tag': '', 'type': 'assertvalue', 'value': None})

            var_loc_as = get_locator([{'css': 'myclass'}, {'xpath': '/html/body/div[3]/form/div/input'}])
            self.assertEqual(self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('value').strip(), 'Find Flights'.strip())
            action_end({'param': 'Find Flights', 'selectors': [{'css': 'myclass'}, {'xpath': '/html/body/div[3]/form/div/input'}], 'tag': '', 'type': 'assertvalue', 'value': None})
            action_start({'param': 'BlazeDemo', 'selectors': [], 'tag': 'title', 'type': 'assert', 'value': None})
            self.assertEqual(self.driver.title, 'BlazeDemo')
            action_end({'param': 'BlazeDemo', 'selectors': [], 'tag': 'title', 'type': 'assert', 'value': None})
            action_start({'param': 'hEaDeR', 'selectors': [], 'tag': 'title', 'type': 'store', 'value': None})

            self.vars['hEaDeR'] = self.driver.title
            action_end({'param': 'hEaDeR', 'selectors': [], 'tag': 'title', 'type': 'store', 'value': None})
            action_start({'param': 'final_var', 'selectors': [], 'tag': 'string', 'type': 'store', 'value': 'test_text'})

            self.vars['final_var'] = 'test_text'
            action_end({'param': 'final_var', 'selectors': [], 'tag': 'string', 'type': 'store', 'value': 'test_text'})
            action_start({'param': 'Basic', 'selectors': [{'xpath': '/html/body/div[3]/h2'}], 'tag': '', 'type': 'storetext', 'value': None})

            var_loc_as = get_locator([{'xpath': '/html/body/div[3]/h2'}])

            self.vars['Basic'] = self.driver.find_element(
                var_loc_as[0],
                var_loc_as[1]).get_attribute('innerText')
            action_end({'param': 'Basic', 'selectors': [{'xpath': '/html/body/div[3]/h2'}], 'tag': '', 'type': 'storetext', 'value': None})
            action_start({'param': '10 === 2*5', 'selectors': [], 'tag': 'eval', 'type': 'assert', 'value': None})
            self.assertTrue(self.driver.execute_script('return 10 === 2*5;'), '10 === 2*5')
            action_end({'param': '10 === 2*5', 'selectors': [], 'tag': 'eval', 'type': 'assert', 'value': None})
            action_start({'param': 'var_assert', 'selectors': [], 'tag': 'eval', 'type': 'assert', 'value': 'myFunction();\nfunction myFunction(){{\n btnNameVar="{}";\n return "support";\n}}'.format(self.vars['btnName1'])})
            self.assertTrue(self.driver.execute_script('return var_assert;'), 'var_assert')
            action_end({'param': 'var_assert', 'selectors': [], 'tag': 'eval', 'type': 'assert', 'value': 'myFunction();\nfunction myFunction(){{\n btnNameVar="{}";\n return "support";\n}}'.format(self.vars['btnName1'])})
            action_start({'param': 'var_eval', 'selectors': [], 'tag': 'eval', 'type': 'store', 'value': 'myFunction();\nfunction myFunction(){{\n btnNameVar="{}";\n return "support";\n}}'.format(self.vars['btnName1'])})

            self.vars['var_eval'] = self.driver.execute_script('return myFunction();\nfunction myFunction(){{\n btnNameVar="{}";\n return "support";\n}};'.format(self.vars['btnName1']))
            action_end({'param': 'var_eval', 'selectors': [], 'tag': 'eval', 'type': 'store', 'value': 'myFunction();\nfunction myFunction(){{\n btnNameVar="{}";\n return "support";\n}}'.format(self.vars['btnName1'])})
            action_start({'param': 'var_eval', 'selectors': [], 'tag': 'eval', 'type': 'store', 'value': '["{}", "{}", "{}", "{}"]'.format(self.vars['id1'], self.vars['id2'], self.vars['id3'], self.vars['id4'])})

            self.vars['var_eval'] = self.driver.execute_script('return ["{}", "{}", "{}", "{}"];'.format(self.vars['id1'], self.vars['id2'], self.vars['id3'], self.vars['id4']))
            action_end({'param': 'var_eval', 'selectors': [], 'tag': 'eval', 'type': 'store', 'value': '["{}", "{}", "{}", "{}"]'.format(self.vars['id1'], self.vars['id2'], self.vars['id3'], self.vars['id4'])})
            action_start({'param': None, 'selectors': [{'xpath': '/wrong/one'}, {'xpath': '/html/body/div[3]/form/div/input'}], 'tag': '', 'type': 'click', 'value': None})

            var_loc_keys = get_locator([{'xpath': '/wrong/one'}, {'xpath': '/html/body/div[3]/form/div/input'}])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).click()
            waiter()
            action_end({'param': None, 'selectors': [{'xpath': '/wrong/one'}, {'xpath': '/html/body/div[3]/form/div/input'}], 'tag': '', 'type': 'click', 'value': None})
            action_start({'param': 'KEY_ENTER', 'selectors': [{'xpath': '/doc/abc'}, {'css': 'body > div.container > table > tbody > tr:nth-child(1) > td:nth-child(2) > input'}], 'tag': '', 'type': 'keys', 'value': None})

            var_loc_keys = get_locator([{'xpath': '/doc/abc'}, {'css': 'body > div.container > table > tbody > tr:nth-child(1) > td:nth-child(2) > input'}])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys(Keys.ENTER)
            action_end({'param': 'KEY_ENTER', 'selectors': [{'xpath': '/doc/abc'}, {'css': 'body > div.container > table > tbody > tr:nth-child(1) > td:nth-child(2) > input'}], 'tag': '', 'type': 'keys', 'value': None})
            action_start({'param': 'myusername', 'selectors': [{'id': 'fjkafjk'}, {'css': 'testCss'}], 'tag': '', 'type': 'type', 'value': None})

            var_loc_keys = get_locator([{'id': 'fjkafjk'}, {'css': 'testCss'}])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).clear()
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys('myusername')
            waiter()
            action_end({'param': 'myusername', 'selectors': [{'id': 'fjkafjk'}, {'css': 'testCss'}], 'tag': '', 'type': 'type', 'value': None})
            action_start({'param': '************', 'selectors': [{'id': 'fjkafjk'}, {'css': 'testCss'}], 'tag': '', 'type': 'typesecret', 'value': None})

            var_loc_keys = get_locator([{'id': 'fjkafjk'}, {'css': 'testCss'}])
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).clear()
            self.driver.find_element(
                var_loc_keys[0],
                var_loc_keys[1]).send_keys('mysecret')
            action_end({'param': '************', 'selectors': [{'id': 'fjkafjk'}, {'css': 'testCss'}], 'tag': '', 'type': 'typesecret', 'value': None})
            action_start({'param': 'American Express', 'selectors': [{'css': 'myclass'}, {'xpath': '//*[@id="cardType"]'}], 'tag': '', 'type': 'select', 'value': None})

            var_loc_select = get_locator([{'css': 'myclass'}, {'xpath': '//*[@id="cardType"]'}])
            Select(self.driver.find_element(
                var_loc_select[0],
                var_loc_select[1])).select_by_visible_text('American Express')
            waiter()
            action_end({'param': 'American Express', 'selectors': [{'css': 'myclass'}, {'xpath': '//*[@id="cardType"]'}], 'tag': '', 'type': 'select', 'value': None})
            action_start({'param': '{window.scrollTo(0, document.body.scrollHeight);}', 'selectors': [], 'tag': 'eval', 'type': 'script', 'value': None})
            self.driver.execute_script('{window.scrollTo(0, document.body.scrollHeight);}')
            waiter()
            action_end({'param': '{window.scrollTo(0, document.body.scrollHeight);}', 'selectors': [], 'tag': 'eval', 'type': 'script', 'value': None})
            action_start({'param': '{{window.scrollTo(0, {});}}'.format(self.vars['pos']), 'selectors': [], 'tag': 'eval', 'type': 'script', 'value': None})
            self.driver.execute_script('{{window.scrollTo(0, {});}}'.format(self.vars['pos']))
            waiter()
            action_end({'param': '{{window.scrollTo(0, {});}}'.format(self.vars['pos']), 'selectors': [], 'tag': 'eval', 'type': 'script', 'value': None})
            action_start({'param': "{var event = new InputEvent('input', { bubbles: true,cancelable: false,data: true });}", 'selectors': [], 'tag': 'eval', 'type': 'script', 'value': None})
            self.driver.execute_script("{var event = new InputEvent('input', { bubbles: true,cancelable: false,data: true });}")
            waiter()
            action_end({'param': "{var event = new InputEvent('input', { bubbles: true,cancelable: false,data: true });}", 'selectors': [], 'tag': 'eval', 'type': 'script', 'value': None})
            action_start({'param': 'for i in range(10):\n  if i % 2 == 0:\n    print(i)', 'selectors': [], 'tag': '', 'type': 'rawcode', 'value': None})

            for i in range(10):
                if ((i % 2) == 0):
                    print(i)
            action_end({'param': 'for i in range(10):\n  if i % 2 == 0:\n    print(i)', 'selectors': [], 'tag': '', 'type': 'rawcode', 'value': None})
            action_start({'param': self.vars['red_pill'], 'selectors': [], 'tag': 'string', 'type': 'echo', 'value': None})
            print(self.vars['red_pill'])
            action_end({'param': self.vars['red_pill'], 'selectors': [], 'tag': 'string', 'type': 'echo', 'value': None})
            action_start({'param': '4.6s', 'selectors': [], 'tag': '', 'type': 'pausefor', 'value': None})
            sleep(4.6)
            action_end({'param': '4.6s', 'selectors': [], 'tag': '', 'type': 'pausefor', 'value': None})
            action_start({'param': None, 'selectors': [], 'tag': 'cookies', 'type': 'clear', 'value': None})
            self.driver.delete_all_cookies()
            action_end({'param': None, 'selectors': [], 'tag': 'cookies', 'type': 'clear', 'value': None})
            action_start({'param': 'screen.png', 'selectors': [], 'tag': '', 'type': 'screenshot', 'value': None})
            self.driver.save_screenshot('screen.png')
            action_end({'param': 'screen.png', 'selectors': [], 'tag': '', 'type': 'screenshot', 'value': None})
            action_start({'param': None, 'selectors': [], 'tag': '', 'type': 'screenshot', 'value': None})

            filename = os.path.join(os.getenv('TAURUS_ARTIFACTS_DIR'), ('screenshot-%d.png' % (time() * 1000)))
            self.driver.save_screenshot(filename)
            action_end({'param': None, 'selectors': [], 'tag': '', 'type': 'screenshot', 'value': None})
            action_start({'param': 'visible', 'selectors': [{'css': 'invalid_css'}, {'name': 'inputName'}], 'tag': '', 'type': 'waitfor', 'value': '2h30m20s'})
            wait_for('visible', [{'css': 'invalid_css'}, {'name': 'inputName'}], 9020.0)
            action_end({'param': 'visible', 'selectors': [{'css': 'invalid_css'}, {'name': 'inputName'}], 'tag': '', 'type': 'waitfor', 'value': '2h30m20s'})
            action_start({'param': 'lo-la-lu', 'selectors': [{'id': 'editor'}], 'tag': '', 'type': 'editcontent', 'value': None})

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
            action_end({'param': 'lo-la-lu', 'selectors': [{'id': 'editor'}], 'tag': '', 'type': 'editcontent', 'value': None})
            action_start({'param': '4.6s', 'selectors': [], 'tag': '', 'type': 'pausefor', 'value': None})
            sleep(4.6)
            action_end({'param': '4.6s', 'selectors': [], 'tag': '', 'type': 'pausefor', 'value': None})
            action_start({'param': None, 'selectors': [], 'tag': 'cookies', 'type': 'clear', 'value': None})
            self.driver.delete_all_cookies()
            action_end({'param': None, 'selectors': [], 'tag': 'cookies', 'type': 'clear', 'value': None})
            action_start({'param': 'screen.png', 'selectors': [], 'tag': '', 'type': 'screenshot', 'value': None})
            self.driver.save_screenshot('screen.png')
            action_end({'param': 'screen.png', 'selectors': [], 'tag': '', 'type': 'screenshot', 'value': None})
            action_start({'param': None, 'selectors': [], 'tag': '', 'type': 'screenshot', 'value': None})

            filename = os.path.join(os.getenv('TAURUS_ARTIFACTS_DIR'), ('screenshot-%d.png' % (time() * 1000)))
            self.driver.save_screenshot(filename)
            action_end({'param': None, 'selectors': [], 'tag': '', 'type': 'screenshot', 'value': None})
            action_start({'param': 'vacation.html', 'selectors': [], 'tag': 'window', 'type': 'open', 'value': None})
            open_window('vacation.html')
            action_end({'param': 'vacation.html', 'selectors': [], 'tag': 'window', 'type': 'open', 'value': None})
            action_start({'param': None, 'selectors': [], 'tag': 'window', 'type': 'maximize', 'value': None})
            self.driver.maximize_window()
            action_end({'param': None, 'selectors': [], 'tag': 'window', 'type': 'maximize', 'value': None})
            action_start({'param': 1, 'selectors': [], 'tag': 'byidx', 'type': 'switchframe', 'value': None})
            switch_frame('index=1')
            action_end({'param': 1, 'selectors': [], 'tag': 'byidx', 'type': 'switchframe', 'value': None})
            action_start({'param': 'relative=parent', 'selectors': [], 'tag': '', 'type': 'switchframe', 'value': None})
            switch_frame('relative=parent')
            action_end({'param': 'relative=parent', 'selectors': [], 'tag': '', 'type': 'switchframe', 'value': None})
            action_start({'param': 'my_frame', 'selectors': [], 'tag': 'byname', 'type': 'switchframe', 'value': None})
            switch_frame(self.driver.find_element(By.NAME, 'my_frame'))
            action_end({'param': 'my_frame', 'selectors': [], 'tag': 'byname', 'type': 'switchframe', 'value': None})
            action_start({'param': 'name=my_frame', 'selectors': [], 'tag': '', 'type': 'switchframe', 'value': None})
            switch_frame(self.driver.find_element(By.NAME, 'my_frame'))
            action_end({'param': 'name=my_frame', 'selectors': [], 'tag': '', 'type': 'switchframe', 'value': None})
            action_start({'param': 'css=my_frame_cls', 'selectors': [], 'tag': '', 'type': 'switchframe', 'value': None})
            switch_frame(self.driver.find_element(By.CSS_SELECTOR, 'my_frame_cls'))
            action_end({'param': 'css=my_frame_cls', 'selectors': [], 'tag': '', 'type': 'switchframe', 'value': None})
            action_start({'param': 'id=my_frame_id', 'selectors': [], 'tag': '', 'type': 'switchframe', 'value': None})
            switch_frame(self.driver.find_element(By.ID, 'my_frame_id'))
            action_end({'param': 'id=my_frame_id', 'selectors': [], 'tag': '', 'type': 'switchframe', 'value': None})
            action_start({'param': "xpath='//xpath'", 'selectors': [], 'tag': '', 'type': 'switchframe', 'value': None})
            switch_frame(self.driver.find_element(By.XPATH, '//xpath'))
            action_end({'param': "xpath='//xpath'", 'selectors': [], 'tag': '', 'type': 'switchframe', 'value': None})
            action_start({'param': None, 'selectors': [], 'tag': 'window', 'type': 'close', 'value': None})
            close_window()
            action_end({'param': None, 'selectors': [], 'tag': 'window', 'type': 'close', 'value': None})
            action_start({'param': 'prompt', 'selectors': [], 'tag': '', 'type': 'answerdialog', 'value': 'my input'})
            dialogs_answer_on_next_prompt('my input')
            action_end({'param': 'prompt', 'selectors': [], 'tag': '', 'type': 'answerdialog', 'value': 'my input'})
            action_start({'param': 'confirm', 'selectors': [], 'tag': '', 'type': 'answerdialog', 'value': '#Ok'})
            dialogs_answer_on_next_confirm('#Ok')
            action_end({'param': 'confirm', 'selectors': [], 'tag': '', 'type': 'answerdialog', 'value': '#Ok'})
            action_start({'param': 'alert', 'selectors': [], 'tag': '', 'type': 'answerdialog', 'value': '#Ok'})
            dialogs_answer_on_next_alert('#Ok')
            action_end({'param': 'alert', 'selectors': [], 'tag': '', 'type': 'answerdialog', 'value': '#Ok'})
            action_start({'param': 'alert', 'selectors': [], 'tag': '', 'type': 'assertdialog', 'value': 'Exception occurred!'})

            dialog = dialogs_get_next_alert()
            self.assertIsNotNone(dialog, 'No dialog of type alert appeared')
            self.assertEqual(dialog, 'Exception occurred!', "Dialog message didn't match")
            action_end({'param': 'alert', 'selectors': [], 'tag': '', 'type': 'assertdialog', 'value': 'Exception occurred!'})
            action_start({'param': 'confirm', 'selectors': [], 'tag': '', 'type': 'assertdialog', 'value': 'Are you sure?'})

            dialog = dialogs_get_next_confirm()
            self.assertIsNotNone(dialog, 'No dialog of type confirm appeared')
            self.assertEqual(dialog, 'Are you sure?', "Dialog message didn't match")
            action_end({'param': 'confirm', 'selectors': [], 'tag': '', 'type': 'assertdialog', 'value': 'Are you sure?'})
            action_start({'param': 'prompt', 'selectors': [], 'tag': '', 'type': 'assertdialog', 'value': 'What is your age?'})

            dialog = dialogs_get_next_prompt()
            self.assertIsNotNone(dialog, 'No dialog of type prompt appeared')
            self.assertEqual(dialog, 'What is your age?', "Dialog message didn't match")
            action_end({'param': 'prompt', 'selectors': [], 'tag': '', 'type': 'assertdialog', 'value': 'What is your age?'})

    def test_locsc(self):
        self._1_Test_V2()

    def tearDown(self):
        if self.driver:
            self.driver.quit()
