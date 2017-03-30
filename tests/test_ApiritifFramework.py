import os
import unittest

from tests import __dir__


def run_apiritif_script(script_path):
    loader = unittest.TestLoader()
    suite = loader.discover(os.path.dirname(script_path), pattern=os.path.basename(script_path))
    return unittest.TextTestRunner(verbosity=2).run(suite)


class TestApiritif(unittest.TestCase):
    def test_example(self):
        result = run_apiritif_script(__dir__() + "/apiritif/test_api_example.py")
        self.assertTrue(result.wasSuccessful())
        self.assertEqual(result.testsRun, 1)

    def test_cookies(self):
        result = run_apiritif_script(__dir__() + "/apiritif/test_cookies.py")
        self.assertTrue(result.wasSuccessful())
        self.assertEqual(result.testsRun, 1)

    def test_assertions(self):
        result = run_apiritif_script(__dir__() + "/apiritif/test_assertions.py")
        self.assertTrue(result.wasSuccessful())
        self.assertEqual(result.testsRun, 3)

    def test_methods(self):
        result = run_apiritif_script(__dir__() + "/apiritif/test_methods.py")
        self.assertTrue(result.wasSuccessful())
        self.assertEqual(result.testsRun, 6)

    def test_body(self):
        result = run_apiritif_script(__dir__() + "/apiritif/test_body_data.py")
        self.assertTrue(result.wasSuccessful())
        self.assertEqual(result.testsRun, 3)
