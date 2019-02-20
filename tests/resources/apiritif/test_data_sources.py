
import logging
import random
import string
import sys
import time
import unittest

import apiritif
import apiritif.csv

target = apiritif.http.target('http://localhost:8000/')
target.keep_alive(True)
target.auto_assert_ok(True)
target.use_cookies(True)
target.allow_redirects(True)

reader_1 = apiritif.csv.CSVReaderPerThread('first-file.csv')
reader_2 = apiritif.csv.CSVReaderPerThread('/second/file.csv', loop=True, quoted=False, delimiter='-')


def setup():
    reader_1.read_vars()
    reader_2.read_vars()


class TestAPI(unittest.TestCase):

    def setUp(self):
        self.vars = {
            'cn': 'cv',
        }
        self.vars.update(reader_1.get_vars())
        self.vars.update(reader_2.get_vars())

    def test_1_an(self):
        with apiritif.transaction(self.vars['an']):
            response = target.get(self.vars['an'])

    def test_2_bn(self):
        with apiritif.transaction(self.vars['bn']):
            response = target.get(self.vars['bn'])

    def test_3_cn(self):
        with apiritif.transaction(self.vars['cn']):
            response = target.get(self.vars['cn'])
