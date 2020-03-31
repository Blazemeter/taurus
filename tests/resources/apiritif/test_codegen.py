# coding=utf-8

import logging
import random
import string
import sys
import unittest
from time import time, sleep

import apiritif

log = logging.getLogger('apiritif.http')
log.addHandler(logging.StreamHandler(sys.stdout))
log.setLevel(logging.DEBUG)


class TestWithExtractors(unittest.TestCase):

    def setUp(self):
        self.vars = {}

        timeout = 5.0
        self.target = apiritif.http.target('https://jsonplaceholder.typicode.com')
        self.target.keep_alive(True)
        self.target.auto_assert_ok(True)
        self.target.use_cookies(True)
        self.target.allow_redirects(True)
        self.target.timeout(5.0)

        apiritif.put_into_thread_store(scenario_name='with-extractors', func_mode=False, timeout=timeout)

    def _1_just_get(self):
        with apiritif.smart_transaction('just get'):
            response = self.target.get('/')

    def _2_get_posts(self):
        with apiritif.smart_transaction('get posts'):
            response = self.target.get('/posts')
            response.assert_jsonpath('$.[0].userId', expected_value=1)
            self.vars['userID'] = response.extract_jsonpath('$.[5].userId', 'NOT_FOUND')

    def _3_get_posts_of_certain_user(self):
        with apiritif.smart_transaction('get posts of certain user'):
            response = self.target.get('/posts?userId={}'.format(self.vars['userID']))
            self.vars['postID'] = response.extract_jsonpath('$.[0].id', 'NOT_FOUND')

    def _4_get_comments_on_post(self):
        with apiritif.smart_transaction('get comments on post'):
            response = self.target.get('/posts/{}/comments'.format(self.vars['postID']))
            response.assert_jsonpath('$[0].email', expected_value=None)

    def _5_add_into_posts(self):
        with apiritif.smart_transaction('add into posts'):
            response = self.target.post('/posts', headers={
                'content-type': 'application/json',
            }, json={
                'body': 'bar',
                'title': 'foo',
                'userId': self.vars['userID'],
            })
            self.vars['addedID'] = response.extract_jsonpath('$.id', 'NOT_FOUND')

    def _6_delete_from_posts(self):
        with apiritif.smart_transaction('delete from posts'):
            response = self.target.delete('/posts/{}'.format(self.vars['postID']))

    def test_with_extractors(self):
        self._1_just_get()
        self._2_get_posts()
        self._3_get_posts_of_certain_user()
        self._4_get_comments_on_post()
        self._5_add_into_posts()
        self._6_delete_from_posts()
