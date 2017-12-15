# coding=utf-8
""" unit test """
import os
import sys
import logging
import tempfile

from psutil import Popen
from os.path import join

from bzt.six import PY2
from bzt.utils import log_std_streams, get_uniq_name, JavaVM, ToolError, is_windows
from tests import BZTestCase, RESOURCES_DIR
from tests.mocks import MockFileReader


class TestJavaVM(BZTestCase):
    def test_missed_tool(self):
        self.obj = JavaVM(logging.getLogger(''), tool_path='java-not-found')
        self.assertEqual(False, self.obj.check_if_installed())
        self.assertRaises(ToolError, self.obj.install)


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
            if is_windows():
                cmd = 'dir'
            else:
                cmd = 'ls'
            process = Popen([cmd, missed_file])
            process.wait()

        debug_buf = self.log_recorder.debug_buff.getvalue()
        warn_buf = self.log_recorder.warn_buff.getvalue()
        self.assertNotIn('test1', debug_buf)
        self.assertIn('test2', debug_buf)
        self.assertNotIn('test3', debug_buf)
        self.assertIn('test5', debug_buf)
        self.assertTrue(len(warn_buf) > 0)


class TestFileReader(BZTestCase):
    def setUp(self):
        super(TestFileReader, self).setUp()
        self.obj = MockFileReader()

    def configure(self, file_name):
        self.obj.name = file_name

    def tearDown(self):
        if self.obj and self.obj.fds:
            self.obj.fds.close()
        super(TestFileReader, self).tearDown()

    def test_file_len(self):
        self.configure(join(RESOURCES_DIR, 'jmeter', 'jtl', 'file.notfound'))
        self.sniff_log(self.obj.log)
        list(self.obj.get_lines(size=1))
        self.assertIn('File not appeared yet', self.log_recorder.debug_buff.getvalue())
        self.obj.name = join(RESOURCES_DIR, 'jmeter', 'jtl', 'unicode.jtl')
        lines = list(self.obj.get_lines(size=1))
        self.assertEqual(1, len(lines))
        lines = list(self.obj.get_lines(last_pass=True))
        self.assertEqual(13, len(lines))
        self.assertTrue(all(l.endswith('\n') for l in lines))

    def test_decode(self):
        old_string = "Тест.Эхо"
        fd, gen_file_name = tempfile.mkstemp()
        os.close(fd)
        with open(gen_file_name, 'wb') as fd:
            if PY2:
                fd.write(bytearray(old_string + '\n'))
            else:
                fd.write((old_string + '\n').encode(self.obj.SYS_ENCODING))

        try:
            self.configure(gen_file_name)
            lines = list(self.obj.get_lines(True))
            self.assertEqual(1, len(lines))
            new_string = lines[0].rstrip()
            if PY2:
                new_string = new_string.encode('utf-8')
            self.assertEqual(old_string, new_string)
        finally:
            if self.obj.fds:
                self.obj.fds.close()

            os.remove(gen_file_name)
