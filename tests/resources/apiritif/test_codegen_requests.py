
import logging
import random
import string
import sys
import time
import unittest

import apiritif


def setup():
    vars = {

    }

    apiritif.put_into_thread_store(vars)


class TestAPI(unittest.TestCase):

    def setUp(self):
        (self.vars,) = apiritif.get_from_thread_store()

    def test_1_apiritif(self):
        with apiritif.transaction('apiritif'):
            response = apiritif.http.get('http://localhost:8000/')
