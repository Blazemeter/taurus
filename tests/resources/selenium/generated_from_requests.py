# coding=utf-8
import unittest
import re
from time import sleep
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
import selenium_taurus_extras

_vars = {}
_tpl = selenium_taurus_extras.Template(_vars)
_vars['name'] = 'Name'
_vars['red_pill'] = 'take_it'

class TestRequests(unittest.TestCase):
    def setUp(self):
        options = webdriver.FirefoxOptions()
        profile = webdriver.FirefoxProfile()
        profile.set_preference('webdriver.log.file', '<somewhere>/webdriver.log')
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
            if self.driver.find_element(By.ID, _tpl.apply('editor')).get_attribute('contenteditable'): self.driver.find_element(By.ID, _tpl.apply('editor')).clear(); self.driver.find_element(By.ID, _tpl.apply('editor')).send_keys(_tpl.apply('lo-la-lu'))
            sleep(3)
            self.driver.delete_all_cookies()
            self.driver.find_element(By.LINK_TEXT, _tpl.apply('destination of the week! The Beach!')).click()
            _vars['Title'] = _tpl.apply(self.driver.title)
            _vars['Basic'] = _tpl.apply(self.driver.find_element(By.XPATH, _tpl.apply("//*[@id='basics']/h2")).get_attribute('innerText'))
            _vars['World'] = _tpl.apply(self.driver.find_element(By.XPATH, _tpl.apply("//*[@id='basics']/h1")).get_attribute('value'))
            _vars['Final'] = _tpl.apply('${Title} ${Basic} by ${By}')
            self.driver.get(_tpl.apply('http:\\blazemeter.com'))
            print(_tpl.apply('${red_pill}'))
            
            
        body = self.driver.page_source
        re_pattern = re.compile(r'contained_text')
        self.assertEqual(0, len(re.findall(re_pattern, body)), "Assertion: 'contained_text' found in BODY")

        with apiritif.transaction_logged('empty'):
            pass

