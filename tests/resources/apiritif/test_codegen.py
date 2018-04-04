
import logging
import random
import string
import sys
import time
import unittest

import apiritif

log = logging.getLogger('apiritif.http')
log.addHandler(logging.StreamHandler(sys.stdout))
log.setLevel(logging.DEBUG)


class TestAPI(unittest.TestCase):

    def __init__(self, methodName='runTest'):
        super(TestAPI, self).__init__(methodName)
        self.vars = {}
        self.target = apiritif.http.target('https://jsonplaceholder.typicode.com')
        self.target.keep_alive(True)
        self.target.auto_assert_ok(True)
        self.target.use_cookies(True)
        self.target.allow_redirects(True)
        self.target.timeout(5.0)
    

    def test_1_just_get(self):
        with apiritif.transaction('just get'):
            response = self.target.get('/')
    

    def test_2_get_posts(self):
        with apiritif.transaction('get posts'):
            response = self.target.get('/posts')
            response.assert_jsonpath('$.[0].userId', expected_value=1)
        self.vars['userID'] = response.extract_jsonpath('$.[5].userId', 'NOT_FOUND')
    

    def test_3_get_posts_of_certain_user(self):
        with apiritif.transaction('get posts of certain user'):
            response = self.target.get('/posts?userId={}'.format(self.vars['userID']))
        self.vars['postID'] = response.extract_jsonpath('$.[0].id', 'NOT_FOUND')
    

    def test_4_get_comments_on_post(self):
        with apiritif.transaction('get comments on post'):
            response = self.target.get('/posts/{}/comments'.format(self.vars['postID']))
            response.assert_jsonpath('$[0].email', expected_value=None)
    

    def test_5_add_into_posts(self):
        with apiritif.transaction('add into posts'):
            response = self.target.post('/posts', headers={'content-type': 'application/json'}, json={'body': 'bar', 'title': 'foo', 'userId': self.vars['userID']})
        self.vars['addedID'] = response.extract_jsonpath('$.id', 'NOT_FOUND')
    

    def test_6_delete_from_posts(self):
        with apiritif.transaction('delete from posts'):
            response = self.target.delete('/posts/{}'.format(self.vars['postID']))
    
