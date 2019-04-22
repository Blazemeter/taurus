# coding=utf-8

import logging
import random
import string
import sys
import unittest
from time import time, sleep

import apiritif

reader_1 = apiritif.CSVReaderPerThread('first-file.csv')
reader_2 = apiritif.CSVReaderPerThread('/second/file.csv', loop=True, quoted=False, delimiter='-')


def setup():
    target = apiritif.http.target('http://localhost:8000/')
    target.keep_alive(True)
    target.auto_assert_ok(True)
    target.use_cookies(True)
    target.allow_redirects(True)

    vars = {
        'cn': 'cv',
    }
    reader_1.read_vars()
    reader_2.read_vars()
    vars.update(reader_1.get_vars())
    vars.update(reader_2.get_vars())

    apiritif.put_into_thread_store(vars, target)


class TestAPI(unittest.TestCase, ):

    def setUp(self):
        (self.vars, self.target) = apiritif.get_from_thread_store()

    def test_1_an(self):
        with apiritif.transaction(self.vars['an']):
            response = self.target.get(self.vars['an'])

    def test_2_bn(self):
        with apiritif.transaction(self.vars['bn']):
            response = self.target.get(self.vars['bn'])

    def test_3_cn(self):
        with apiritif.transaction(self.vars['cn']):
            response = self.target.get(self.vars['cn'])
