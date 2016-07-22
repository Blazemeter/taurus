import time
import unittest


class TestTasty(unittest.TestCase):
    def test_okay(self):
        "this is some test"
        self.assertEqual(eval("2 + 2"), 4)

    def test_failure(self):
        self.assertEqual(2 + 2 * 2, 8)

    def test_error(self):
        42 / 0
