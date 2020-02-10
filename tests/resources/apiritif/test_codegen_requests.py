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
        self.vars = {}
        apiritif.put_into_thread_store(func_mode=False)

    def _1_apiritif(self):
        with apiritif.smart_transaction('apiritif'):
            response = apiritif.http.get('http://localhost:8000/')

    def test_test_requests(self):
        self._1_apiritif()
