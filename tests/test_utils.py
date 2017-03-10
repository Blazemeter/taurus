""" unit test """
import logging

from psutil import Popen

from bzt.utils import log_std_streams, get_uniq_name
from tests.mocks import RecordingHandler
from tests import BZTestCase


class TestLogStreams(BZTestCase):
    def test_streams(self):
        self.log = logging.getLogger('')
        handler = RecordingHandler()
        self.log.addHandler(handler)

        print('test1')

        with log_std_streams(logger=self.log, stdout_level=logging.DEBUG):
            print('test2')

        with log_std_streams(stdout_level=logging.DEBUG):
            print('test3')

        with log_std_streams(logger=self.log, stdout_level=logging.DEBUG):
            process = Popen(['echo', '"test4"'])
            process.wait()

        missed_file = get_uniq_name('.', 'test5', '')

        with log_std_streams(logger=self.log, stderr_level=logging.WARNING):
            process = Popen(['dir', missed_file])
            process.wait()

        self.log.removeHandler(handler)

        debug_buf = handler.debug_buff.getvalue()
        warn_buf = handler.warn_buff.getvalue()
        self.assertNotIn('test1', debug_buf)
        self.assertIn('test2', debug_buf)
        self.assertNotIn('test3', debug_buf)
        self.assertIn('test4', debug_buf)
        self.assertTrue(len(warn_buf) > 0)
