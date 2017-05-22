""" unit test """
import sys
import logging

from psutil import Popen

from bzt.utils import log_std_streams, get_uniq_name, JavaVM, ToolError
from tests import BZTestCase


class TestJavaVM(BZTestCase):
    def test_missed_tool(self):
        self.obj = JavaVM(logging.getLogger(''), tool_path='java-not-found')
        self.assertEqual(False, self.obj.check_if_installed())
        self.assertRaises(ToolError)


class TestLogStreams(BZTestCase):
    def test_streams(self):
        self.sniff_log(logging.getLogger(''))

        print('test1')

        with log_std_streams(logger=self.captured_logger, stdout_level=logging.DEBUG):
            print('test2')

        with log_std_streams(stdout_level=logging.DEBUG):
            print('test3')

        with log_std_streams(stdout_level=logging.DEBUG):
            sys.stdout.write('test3')

        with log_std_streams(logger=self.captured_logger, stdout_level=logging.DEBUG):
            process = Popen(['echo', '"test5"'])
            process.wait()

        missed_file = get_uniq_name('.', 'test6', '')

        with log_std_streams(logger=self.captured_logger, stderr_level=logging.WARNING):
            process = Popen(['dir', missed_file])
            process.wait()

        debug_buf = self.log_recorder.debug_buff.getvalue()
        warn_buf = self.log_recorder.warn_buff.getvalue()
        self.assertNotIn('test1', debug_buf)
        self.assertIn('test2', debug_buf)
        self.assertNotIn('test3', debug_buf)
        self.assertIn('test5', debug_buf)
        self.assertTrue(len(warn_buf) > 0)
