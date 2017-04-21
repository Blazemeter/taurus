import unittest

from apiritif import http, transaction

target = http.target('https://jsonplaceholder.typicode.com')
target.keep_alive(True)
target.auto_assert_ok(False)
target.use_cookies(True)


class TestRequests(unittest.TestCase):
    # will produce test-case sample with one sub-sample
    def test_1_single_request(self):
        target.get('/')

    # will produce test-case sample with two sub-samples
    def test_2_multiple_requests(self): 
        target.get('/')
        target.get('/2')

    # won't produce test-case sample, only transaction
    def test_3_toplevel_transaction(self):
        with transaction("Transaction"):
            target.get('/')
            target.get('/2')

    # won't produce test-case sample, only "Tran Name"
    # will also will skip "GET /" request, as it's not in the transaction.
    def test_4_mixed_transaction(self):
        target.get('/')
        with transaction("Transaction"):
            target.get('/2')

    # won't produce test-case sample, two separate ones
    def test_5_multiple_transactions(self): 
        with transaction("Transaction 1"):
            target.get('/')
            target.get('/2')

        with transaction("Transaction 2"):
            target.get('/')
            target.get('/2')
