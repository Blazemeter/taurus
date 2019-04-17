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

    }

    apiritif.put_into_thread_store(vars)


class TestAPI(unittest.TestCase, ):

    def setUp(self):
        (self.vars,) = apiritif.get_from_thread_store()

    def test_1_url0(self):
        with apiritif.transaction('url_0'):
            response = apiritif.http.get('url_0')

    def test_2_t1(self):
        with apiritif.transaction('t_1'):
            response = apiritif.http.get('url_1.0')
            response = apiritif.http.get('url_1.1', headers={
                'o': 'ne',
                't': 'wo',
            })

    def test_3_t2(self):
        with apiritif.transaction('t_2'):
            response = apiritif.http.get('url_2.0')
            sleep(2.0)
            with apiritif.transaction('t_22'):
                response = apiritif.http.get('url_22.0')
                sleep(3.0)
