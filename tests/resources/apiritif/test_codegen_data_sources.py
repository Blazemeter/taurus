
import logging
import random
import string
import sys
import time
import unittest

import apiritif

vars = {

}
urls_feeder = apiritif.feeders.CSVFeeder('urls.csv', vars)


class TestAPI(unittest.TestCase):
    
    def test_1_httplocalhost8000url(self):
        with apiritif.transaction('http://localhost:8000/{}'.format(vars['url'])):
            response = apiritif.http.get('http://localhost:8000/{}'.format(vars['url']))
