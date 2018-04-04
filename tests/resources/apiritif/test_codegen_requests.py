
import logging
import random
import string
import sys
import time
import unittest

import apiritif

vars = {}


class TestAPI(unittest.TestCase):
    

    def test_1_apiritif(self):
        with apiritif.transaction('apiritif'):
            response = apiritif.http.get('http://localhost:8000/')
    
