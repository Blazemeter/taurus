import unittest

from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.common.exceptions import NoAlertPresentException


class TestBlazemeterPass(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        raise Exception("Catch that")
    
    def test_pass(self):
        pass
