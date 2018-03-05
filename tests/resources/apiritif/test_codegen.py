
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
 
 

class TestAPIRequests(unittest.TestCase):

    def test_requests(self):
        target = apiritif.http.target('https://jsonplaceholder.typicode.com')
        target.keep_alive(True)
        target.auto_assert_ok(True)
        target.use_cookies(True)
        target.allow_redirects(True)
        target.timeout(5.0)
         
        with apiritif.transaction('just get'):
            response = target.get('/')
         
        with apiritif.transaction('get posts'):
            response = target.get('/posts')
            response.assert_jsonpath('$.[0].userId', expected_value=1)
        userID = response.extract_jsonpath('$.[5].userId', 'NOT_FOUND')
         
        with apiritif.transaction('get posts of certain user'):
            response = target.get('/posts?userId={}'.format(userID))
        postID = response.extract_jsonpath('$.[0].id', 'NOT_FOUND')
         
        with apiritif.transaction('get comments on post'):
            response = target.get('/posts/{}/comments'.format(postID))
            response.assert_jsonpath('$[0].email', expected_value=None)
         
        with apiritif.transaction('add into posts'):
            response = target.post('/posts', headers={'content-type': 'application/json'}, json={'body': 'bar', 'title': 'foo', 'userId': userID})
        addedID = response.extract_jsonpath('$.id', 'NOT_FOUND')
         
        with apiritif.transaction('delete from posts'):
            response = target.delete('/posts/{}'.format(postID))
         
