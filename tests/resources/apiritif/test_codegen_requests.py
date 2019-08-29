# coding=utf-8

import logging
import random
import string
import sys
import unittest
from time import time, sleep

import apiritif


def setup():
    vars = {
        'var1': 'val1',
    }

    apiritif.put_into_thread_store(vars)


class TestAPI(unittest.TestCase, ):

    def setUp(self):
        (self.vars,) = apiritif.get_from_thread_store()

    def test_1_apiritif(self):
        with apiritif.transaction('apiritif'):
            response = apiritif.http.get('http://localhost:8000/')

    def test_2_apiritifvar1(self):
        with apiritif.transaction('apiritif/{}'.format(self.vars['var1'])):
            response = apiritif.http.get('http://localhost:8000/{}'.format(self.vars['var1']))

    def test_3_set_variables(self):
        self.vars['var1'] = 'val2'

    def test_2_apiritifvar2(self):
        with apiritif.transaction('apiritif/{}'.format(self.vars['var1'])):
            response = apiritif.http.get('http://localhost:8000/{}'.format(self.vars['var1']))
