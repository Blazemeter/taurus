# coding=utf-8

import logging
import random
import string
import sys
import unittest
from time import time, sleep

import apiritif


class TestLocSc(unittest.TestCase):

    def setUp(self):
        self.vars = {}
        
        timeout = 30.0
        apiritif.put_into_thread_store(timeout=timeout, func_mode=False, scenario_name='loc_sc')
    

    def _1_localhost(self):
        with apiritif.smart_transaction('localhost'):
            response = apiritif.http.get('localhost', cert="configs/alice.p12")

    def _2_blazedemocom(self):
        with apiritif.smart_transaction('blazedemo.com'):
            response = apiritif.http.get('blazedemo.com', cert="configs/alice.p12")

    def test_locsc(self):
        self._1_localhost()
        self._2_blazedemocom()
