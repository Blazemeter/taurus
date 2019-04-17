# coding=utf-8

import logging
import random
import string
import sys
import unittest
from time import time, sleep

import apiritif


def setup():
    target = apiritif.http.target('http://localhost:8000/')
    target.keep_alive(True)
    target.auto_assert_ok(True)
    target.use_cookies(True)
    target.allow_redirects(True)
    
    vars = {
        'an': 'av',
    }
    
    apiritif.put_into_thread_store(vars, target)


class TestAPI(unittest.TestCase, ):

    def setUp(self):
        (self.vars, self.target) = apiritif.get_from_thread_store()

    def test_1_an(self):
        with apiritif.transaction(self.vars['an']):
            response = self.target.get(self.vars['an'])

    def test_2_set_variables(self):
        self.vars['an'] = 'another_path1'
        self.vars['bn'] = 'another_path2'

    def test_3_an(self):
        with apiritif.transaction(self.vars['an']):
            response = self.target.get(self.vars['an'])
