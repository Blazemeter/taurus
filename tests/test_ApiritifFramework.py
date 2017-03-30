import unittest

from tests import __dir__


class TestApiritif(unittest.TestCase):
    def test_example(self):
        loader = unittest.TestLoader()
        suite = loader.discover(__dir__() + "/apiritif/", pattern="test_api_example.py")
        result = unittest.TextTestRunner(verbosity=2).run(suite)
        self.assertTrue(result.wasSuccessful())
        self.assertEqual(result.testsRun, 1)

    def test_cookies(self):
        loader = unittest.TestLoader()
        suite = loader.discover(__dir__() + "/apiritif/", pattern="test_cookies.py")
        result = unittest.TextTestRunner(verbosity=2).run(suite)
        self.assertTrue(result.wasSuccessful())
        self.assertEqual(result.testsRun, 1)
