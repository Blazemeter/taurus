from unittest import TestCase

from apiritif import http


class TestRequests(TestCase):
    def test_body_string(self):
        http.get('http://blazedemo.com/', data='MY PERFECT BODY')
        
    def test_body_json(self):
        http.get('http://blazedemo.com/', json={'foo': 'bar'})

    def test_url_params(self):
        http.get('http://blazedemo.com/', params={'foo': 'bar'})

