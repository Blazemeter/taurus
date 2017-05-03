import time
import unittest
import logging
import sys

import apiritif


log = logging.getLogger('apiritif.http')
log.addHandler(logging.StreamHandler(sys.stdout))
log.setLevel(logging.DEBUG)

class TestRequests(unittest.TestCase):
    def setUp(self):
        self.target = apiritif.http.target('https://jsonplaceholder.typicode.com')
        self.target.keep_alive(True)
        self.target.auto_assert_ok(True)
        self.target.use_cookies(True)
        self.target.allow_redirects(True)
        self.target.timeout(5.0)

    def test_requests(self):
        with apiritif.transaction('just get'):
            response = self.target.get('/')

        with apiritif.transaction('get posts'):
            response = self.target.get('/posts')
        response.assert_jsonpath('$.[0].userId', expected_value=1)
        userID = response.extract_jsonpath('$.[5].userId', 'NOT_FOUND')

        with apiritif.transaction('get posts of certain user'):
            response = self.target.get('/posts?userId=' + str(userID))
        postID = response.extract_jsonpath('$.[0].id', 'NOT_FOUND')

        with apiritif.transaction('get comments on post'):
            response = self.target.get('/posts/' + str(postID) + '/comments')
        response.assert_jsonpath('$[0].email', expected_value=None)

        with apiritif.transaction('add into posts'):
            response = self.target.post('/posts', headers={'content-type': 'application/json'}, json={'body': 'bar', 'title': 'foo', 'userId': str(userID)})
        addedID = response.extract_jsonpath('$.id', 'NOT_FOUND')

        with apiritif.transaction('delete from posts'):
            response = self.target.delete('/posts/' + str(postID))

