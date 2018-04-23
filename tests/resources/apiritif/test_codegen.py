
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

vars = {}
target = apiritif.http.target('https://jsonplaceholder.typicode.com')
target.keep_alive(True)
target.auto_assert_ok(True)
target.use_cookies(True)
target.allow_redirects(True)
target.timeout(5.0)


class TestWithExtractors(unittest.TestCase):
    

    def test_1_just_get(self):
        with apiritif.transaction('just get'):
            response = target.get('/')
    

    def test_2_get_posts(self):
        with apiritif.transaction('get posts'):
            response = target.get('/posts')
            response.assert_jsonpath('$.[0].userId', expected_value=1)
        vars['userID'] = response.extract_jsonpath('$.[5].userId', 'NOT_FOUND')
    

    def test_3_get_posts_of_certain_user(self):
        with apiritif.transaction('get posts of certain user'):
            response = target.get('/posts?userId={}'.format(vars['userID']))
        vars['postID'] = response.extract_jsonpath('$.[0].id', 'NOT_FOUND')
    

    def test_4_get_comments_on_post(self):
        with apiritif.transaction('get comments on post'):
            response = target.get('/posts/{}/comments'.format(vars['postID']))
            response.assert_jsonpath('$[0].email', expected_value=None)
    

    def test_5_add_into_posts(self):
        with apiritif.transaction('add into posts'):
            response = target.post('/posts', headers={'content-type': 'application/json'}, json={'body': 'bar', 'title': 'foo', 'userId': vars['userID']})
        vars['addedID'] = response.extract_jsonpath('$.id', 'NOT_FOUND')
    

    def test_6_delete_from_posts(self):
        with apiritif.transaction('delete from posts'):
            response = target.delete('/posts/{}'.format(vars['postID']))
    
