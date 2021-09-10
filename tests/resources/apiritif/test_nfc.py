# coding=utf-8

import logging
import random
import string
import sys
import unittest
from time import time, sleep
import apiritif
log = logging.getLogger('apiritif.http')
log.addHandler(logging.StreamHandler(sys.stdout))
log.setLevel(logging.DEBUG)

class TestSc1(unittest.TestCase):

    def setUp(self):
        self.vars = {}
        timeout = 2.0
        # graceful_flag = os.environ.get('GRACEFUL')
        #
        # if graceful_flag and os.path.exists(graceful_flag):
        #     os.remove(graceful_flag)

        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, scenario_name='sc1')

    # def tearDown(self):
    #     graceful_flag = os.environ.get('GRACEFUL')
    #     if graceful_flag and os.path.exists(graceful_flag):
    #         os.remove(graceful_flag)

    def _1_httpsblazedemocomsetup1(self):
        with apiritif.smart_transaction('https://blazedemo.com/setup1'):
            response = apiritif.http.get('https://blazedemo.com/setup1', timeout=2.0)

    def _2_httpsblazedemocomsetup2(self):
        with apiritif.smart_transaction('https://blazedemo.com/setup2'):
            response = apiritif.http.get('https://blazedemo.com/setup2', timeout=2.0)

    def _3_httpsblazedemocommain1(self):
        with apiritif.smart_transaction('https://blazedemo.com/main1'):
            response = apiritif.http.get('https://blazedemo.com/main1', timeout=2.0)

    def _4_httpsblazedemocommain2(self):
        with apiritif.smart_transaction('https://blazedemo.com/main2'):
            #raise BaseException('111')

            # graceful_flag = os.environ.get('GRACEFUL')
            #
            # with open(graceful_flag, 'a+') as _f:
            #     _f.write('')
            #
            # raise RuntimeError('it''s me)')

            response = apiritif.http.get('https://blazedemo.com/main2', timeout=2.0)

    def _5_httpsblazedemocommain3(self):
        with apiritif.smart_transaction('https://blazedemo.com/main3'):
            response = apiritif.http.get('https://blazedemo.com/main3', timeout=2.0)

    def _6_httpsblazedemocomteardown1(self):
        with apiritif.smart_transaction('https://blazedemo.com/teardown1'):
            response = apiritif.http.get('https://blazedemo.com/teardown1', timeout=2.0)

    def _7_httpsblazedemocomteardown2(self):
        with apiritif.smart_transaction('https://blazedemo.com/teardown2'):
            response = apiritif.http.get('https://blazedemo.com/teardown2', timeout=2.0)

    def test_sc1(self):
        try:
            self._1_httpsblazedemocomsetup1()
            self._2_httpsblazedemocomsetup2()
            self._3_httpsblazedemocommain1()
            self._4_httpsblazedemocommain2()
            self._5_httpsblazedemocommain3()
        finally:
            apiritif.set_stage("teardown")      # can't be interrupted
            self._6_httpsblazedemocomteardown1()
            self._7_httpsblazedemocomteardown2()
