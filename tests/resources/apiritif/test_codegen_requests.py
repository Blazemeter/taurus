
import logging
import random
import string
import sys
import time
import unittest

import apiritif


class TestAPI(unittest.TestCase):

    def __init__(self, methodName='runTest'):
        super(TestAPI, self).__init__(methodName)
    

    def test_1_apiritif(self):
        with apiritif.transaction('apiritif'):
            response = apiritif.http.get('http://localhost:8000/')
    
