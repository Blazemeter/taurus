import os
import unittest

from tests import __dir__


class TestApiritif(unittest.TestCase):
    @staticmethod
    def run_test_script(script_path):
        loader = unittest.TestLoader()
        suite = loader.discover(os.path.dirname(script_path), pattern=os.path.basename(script_path))
        return unittest.TextTestRunner(verbosity=2).run(suite)

    def test_example(self):
        result = self.run_test_script(__dir__() + "/apiritif/test_api_example.py")
        self.assertTrue(result.wasSuccessful())
        self.assertEqual(result.testsRun, 1)

    def test_cookies(self):
        result = self.run_test_script(__dir__() + "/apiritif/test_cookies.py")
        self.assertTrue(result.wasSuccessful())
        self.assertEqual(result.testsRun, 1)

    def test_assertions(self):
        result = self.run_test_script(__dir__() + "/apiritif/test_assertions.py")
        self.assertTrue(result.wasSuccessful())
        self.assertEqual(result.testsRun, 3)

    # TODO: test body data
    # TODO: test methods
