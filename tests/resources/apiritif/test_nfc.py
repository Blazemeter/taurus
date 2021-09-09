# coding=utf-8

import logging
import random
import string
import sys
import unittest
from time import time, sleep
import os
import apiritif


class TestSc1(unittest.TestCase):

    def setUp(self):
        self.vars = {}
        timeout = 2.0
        graceful_flag = os.environ.get('GRACEFUL')

        if graceful_flag and os.path.exists(graceful_flag):
            os.remove(graceful_flag)

        apiritif.put_into_thread_store(timeout=timeout, func_mode=True, scenario_name='sc1')

    def tearDown(self):
        graceful_flag = os.environ.get('GRACEFUL')
        if graceful_flag and os.path.exists(graceful_flag):
            os.remove(graceful_flag)

    def _1_httpsblazedemocomsetup1(self):
        with apiritif.smart_transaction('https://blazedemo.com/setup1'):
            response = apiritif.http.get('https://blazedemo.com/setup2', timeout=2.0)

    def _1_httpsblazedemocomsetup2(self):
        with apiritif.smart_transaction('https://blazedemo.com/setup2'):
            response = apiritif.http.get('https://blazedemo.com/setup2', timeout=2.0)

    def _2_httpsblazedemocommain1(self):
        with apiritif.smart_transaction('https://blazedemo.com/main1'):
            response = apiritif.http.get('https://blazedemo.com/main1', timeout=2.0)

    def _2_httpsblazedemocommain2(self):
        with apiritif.smart_transaction('https://bad_url.com/main2'):
            #raise BaseException('111')

            graceful_flag = os.environ.get('GRACEFUL')

            with open(graceful_flag, 'a+') as _f:
                _f.write('')

            #raise RuntimeError('it''s me)')

            response = apiritif.http.get('https://blazedemo.com/main2', timeout=2.0)

    def _2_httpsblazedemocommain3(self):
        with apiritif.smart_transaction('https://blazedemo.com/main3'):
            response = apiritif.http.get('https://blazedemo.com/main3', timeout=2.0)

    def _3_httpsblazedemocomteardown1(self):
        with apiritif.smart_transaction('https://blazedemo.com/teardown1'):
            response = apiritif.http.get('https://blazedemo.com/teardown1', timeout=2.0)

    def _3_httpsblazedemocomteardown2(self):
        with apiritif.smart_transaction('https://blazedemo.com/teardown2'):
            response = apiritif.http.get('https://blazedemo.com/teardown2', timeout=2.0)

    def test_sc1(self):
        try:
            apiritif.set_stage("setup")     # can't be interrupted
            self._1_httpsblazedemocomsetup1()
            self._1_httpsblazedemocomsetup2()

            apiritif.set_stage("main")      # interruptable by error or graceful flag
            self._2_httpsblazedemocommain1()
            self._2_httpsblazedemocommain2()
            self._2_httpsblazedemocommain3()
        finally:
            apiritif.set_stage("teardown")      # can't be interrupted
            self._3_httpsblazedemocomteardown1()
            self._3_httpsblazedemocomteardown2()
