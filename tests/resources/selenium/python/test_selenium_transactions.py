import time
import unittest

import apiritif


class TestSomething(unittest.TestCase):
    def test_me(self):
        with apiritif.transaction_logged('hello there'):
            time.sleep(0.1)
            self.assertEqual(2 + 2, 4)
