# coding=utf-8

import logging
import random
import string
import sys
import unittest
from time import time, sleep

import apiritif


class TestAPI(unittest.TestCase, ):

    def setUp(self):
        self.vars = {

        }
        apiritif.put_into_thread_store(func_mode=False)

    def _1_url0(self):
        with apiritif.smart_transaction('url_0'):
            response = apiritif.http.get('url_0')

    def _2_t1(self):
        with apiritif.smart_transaction('t_1'):
            response = apiritif.http.get('url_1.0')
            response = apiritif.http.get('url_1.1', headers={
                'o': 'ne',
                't': 'wo',
            })

    def _3_t2(self):
        with apiritif.smart_transaction('t_2'):
            response = apiritif.http.get('url_2.0')
            sleep(2.0)
            with apiritif.transaction('t_22'):
                response = apiritif.http.get('url_22.0')
                sleep(3.0)

    def test_test_requests(self):
        self._1_url0()
        self._2_t1()
        self._3_t2()
