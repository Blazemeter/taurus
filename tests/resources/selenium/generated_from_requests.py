# coding=utf-8
import unittest
import os
import re
from time import sleep, time
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
from bzt.resources import selenium_taurus_extras

_vars = {}
_tpl = selenium_taurus_extras.Template(_vars)
_vars['name'] = 'Name'
_vars['red_pill'] = 'take_it'

class TestRequests(unittest.TestCase):
    def setUp(self):
        options = webdriver.FirefoxOptions()
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '<somewhere>webdriver.log')
        self.driver = webdriver.Firefox(profile, firefox_options=options)
        self.driver.implicitly_wait(3.5)
        self.wnd_mng = selenium_taurus_extras.WindowManager(self.driver)
        self.frm_mng = selenium_taurus_extras.FrameManager(self.driver)

    def tearDown(self):
        self.driver.quit()

    def test_requests(self):
        self.driver.implicitly_wait(3.5)

        with apiritif.transaction_logged('/'):
            self.driver.get('http://blazedemo.com/')

            WebDriverWait(self.driver, 3.5).until(econd.presence_of_element_located((By.XPATH, _tpl.apply("//input[@type='submit']"))), 'Element "//input[@type=\'submit\']" failed to appear within 3.5s')
            self.assertEqual(self.driver.title, _tpl.apply('BlazeDemo'))
            ActionChains(self.driver).move_to_element(self.driver.find_element(By.XPATH, _tpl.apply('/html/body/div[2]/div/p[2]/a'))).perform()
            ActionChains(self.driver).double_click(self.driver.find_element(By.XPATH, _tpl.apply('/html/body/div[3]/h2'))).perform()
            ActionChains(self.driver).click_and_hold(self.driver.find_element(By.XPATH, _tpl.apply('/html/body/div[3]/form/select[1]'))).perform()
            ActionChains(self.driver).release(self.driver.find_element(By.XPATH, _tpl.apply('/html/body/div[3]/form/select[1]/option[6]'))).perform()
            Select(self.driver.find_element(By.NAME, _tpl.apply('toPort'))).select_by_visible_text(_tpl.apply('London'))
            self.driver.find_element(By.CSS_SELECTOR, _tpl.apply('body input.btn.btn-primary')).send_keys(Keys.ENTER)
            self.assertEqual(_tpl.apply(self.driver.find_element(By.ID, _tpl.apply('address')).get_attribute('value')).strip(), _tpl.apply('123 Beautiful st.').strip())
            self.assertEqual(_tpl.apply(self.driver.find_element(By.XPATH, _tpl.apply('/html/body/div[2]/form/div[1]/label')).get_attribute('innerText')).strip(), _tpl.apply('${name}').strip())
            WebDriverWait(self.driver, 3.5).until(econd.visibility_of_element_located((By.NAME, _tpl.apply('toPort'))), "Element 'toPort' failed to appear within 3.5s")
            self.driver.find_element(By.NAME, _tpl.apply('toPort')).send_keys(_tpl.apply('B'))
            self.driver.find_element(By.NAME, _tpl.apply('toPort')).clear()
            self.driver.find_element(By.NAME, _tpl.apply('toPort')).send_keys(_tpl.apply('B'))
            self.driver.find_element(By.NAME, _tpl.apply('toPort')).send_keys(Keys.ENTER)
            self.driver.find_element(By.NAME, _tpl.apply('toPort')).clear()
            self.driver.find_element(By.NAME, _tpl.apply('toPort')).send_keys(Keys.ENTER)
            self.driver.find_element(By.XPATH, _tpl.apply('//div[3]/form/select[1]//option[3]')).click()
            self.driver.find_element(By.XPATH, _tpl.apply('//div[3]/form/select[2]//option[6]')).click()
            self.wnd_mng.switch(_tpl.apply('0'))
            self.driver.execute_script(_tpl.apply("window.open('some.url');"))
            self.wnd_mng.switch(_tpl.apply('win_ser_local'))
            self.wnd_mng.switch(_tpl.apply('win_ser_1'))
            self.wnd_mng.switch(_tpl.apply('that_window'))
            self.wnd_mng.close(_tpl.apply('1'))
            self.wnd_mng.close(_tpl.apply('win_ser_local'))
            self.wnd_mng.close(_tpl.apply('win_ser_1'))
            self.wnd_mng.close(_tpl.apply('that_window'))
            self.driver.find_element(By.NAME, _tpl.apply('toPort')).submit()
            self.driver.execute_script(_tpl.apply("alert('This is Sparta');"))
            ActionChains(self.driver).drag_and_drop(self.driver.find_element(By.ID, _tpl.apply('address')), self.driver.find_element(By.NAME, _tpl.apply('toPort'))).perform()
            self.frm_mng.switch(self.driver.find_element(By.NAME, _tpl.apply('my_frame')))
            self.frm_mng.switch(1)
            self.frm_mng.switch('relative=parent')
            if self.driver.find_element(By.ID, 'editor').get_attribute('contenteditable'):
                self.driver.execute_script(
                    'arguments[0].innerHTML = %s;' % _tpl.str_repr(_tpl.apply('lo-la-lu')),
                    self.driver.find_element(By.ID, 'editor')
                )
            else:
                raise NoSuchElementException("The element (By.ID, 'editor') is not contenteditable element")
            sleep(3.5)
            self.driver.delete_all_cookies()
            self.driver.find_element(By.LINK_TEXT, _tpl.apply('destination of the week! The Beach!')).click()
            _vars['Title'] = _tpl.apply(self.driver.title)
            _vars['Basic'] = _tpl.apply(self.driver.find_element(By.XPATH, _tpl.apply("//*[@id='basics']/h2")).get_attribute('innerText'))
            _vars['World'] = _tpl.apply(self.driver.find_element(By.XPATH, _tpl.apply("//*[@id='basics']/h1")).get_attribute('value'))
            _vars['Final'] = _tpl.apply('${Title} ${Basic} by ${By}')
            self.driver.get(_tpl.apply('http:\\blazemeter.com'))
            print(_tpl.apply('${red_pill}'))
            self.driver.save_screenshot(_tpl.apply('screen.png'))
            filename = os.path.join(os.getenv('TAURUS_ARTIFACTS_DIR'), 'screenshot-%d.png' % (time() * 1000))
            self.driver.save_screenshot(filename)

            body = self.driver.page_source
            re_pattern = re.compile(r'contained_text')
            self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")


        with apiritif.transaction_logged('empty'):
            pass


# Utility functions and classes for Taurus Selenium tests

from string import Template as StrTemplate
from selenium.common.exceptions import NoSuchWindowException, NoSuchFrameException


class Apply(StrTemplate):
    def __init__(self, template):
        super(Apply, self).__init__(template)
        self.variables = {}

    def __repr__(self):
        return repr(self.safe_substitute(self.variables))

    def __str__(self):
        return self.safe_substitute(self.variables)


class Template:
    def __init__(self, variables):
        self.variables = variables
        self.tmpl = Apply("")

    def apply(self, template):
        self.tmpl.template = template
        self.tmpl.variables = self.variables
        return str(self.tmpl)

    @staticmethod
    def str_repr(text):
        return repr(text)[1:] if repr(text)[0] == "u" else repr(text)


class FrameManager:
    def __init__(self, driver):
        self.driver = driver

    def switch(self, frame_name=None):
        try:
            if not frame_name or frame_name == "relative=top":
                self.driver.switch_to_default_content()
            elif frame_name.startswith("index="):  # Switch using index frame using relative position
                self.driver.switch_to.frame(int(frame_name.split("=")[1]))
            elif frame_name == "relative=parent":  # Switch to parent frame of the current frame
                self.driver.switch_to.parent_frame()
            else:  # Use the selenium alternative
                self.driver.switch_to.frame(frame_name)
        except NoSuchFrameException:
            raise NoSuchFrameException("Invalid Frame ID: %s" % frame_name)


class WindowManager:
    def __init__(self, driver):
        self.driver = driver
        self.windows = {}

    def switch(self, window_name=None):
        try:
            if not window_name:  # Switch to last window created
                self.driver.switch_to.window(self.driver.window_handles[-1])
            else:
                if window_name.isdigit():  # Switch to window handler index
                    self._switch_by_idx(int(window_name))
                else:
                    if window_name.startswith("win_ser_"):  # Switch using window sequential mode
                        self._switch_by_win_ser(window_name)
                    else:  # Switch using window name
                        self.driver.switch_to.window(window_name)
        except NoSuchWindowException:
            raise NoSuchWindowException("Invalid Window ID: %s" % window_name)

    def _switch_by_idx(self, win_index):
        wnd_handlers = self.driver.window_handles
        if len(wnd_handlers) <= win_index and win_index >= 0:
            self.driver.switch_to.window(wnd_handlers[win_index])
        else:
            raise NoSuchWindowException("Invalid Window ID: %s" % str(win_index))

    def _switch_by_win_ser(self, window_name):
        if window_name == "win_ser_local":
            wnd_handlers = self.driver.window_handles
            if len(wnd_handlers) > 0:
                self.driver.switch_to.window(wnd_handlers[0])
            else:
                raise NoSuchWindowException("Invalid Window ID: %s" % window_name)
        else:
            if window_name not in self.windows:
                self.windows[window_name] = self.driver.window_handles[-1]
            self.driver.switch_to.window(self.windows[window_name])

    def close(self, window_name=None):
        if window_name:
            self.switch(window_name)
        self.driver.close()

