# coding=utf-8
""" unit test """
import os
import sys
import logging
import tempfile

from psutil import Popen
from os.path import join

from bzt import TaurusNetworkError
from bzt.six import PY2
from bzt.utils import log_std_streams, get_uniq_name, JavaVM, ToolError, is_windows, HTTPClient
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
            cmd = ['echo', '"test5"']
            if is_windows():
                cmd = ['cmd', '/c'] + cmd
            process = Popen(cmd)
            process.wait()

        missed_file = get_uniq_name('.', 'test6', '')

        with log_std_streams(logger=self.captured_logger, stderr_level=logging.WARNING):
            if is_windows():
                cmd = ['cmd', '/c', 'dir']
            else:
                cmd = ['ls']
            process = Popen(cmd + [missed_file])
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

        mod_str = old_string + '\n'
        if PY2:
            mod_str = bytearray(mod_str).decode('utf-8')    # convert to utf-8 on py2 for writing...

        with open(gen_file_name, 'wb') as fd:                   # use target system encoding for writing
            fd.write(mod_str.encode(self.obj.SYS_ENCODING))     # important on win where it's not 'utf-8'

        try:
            self.configure(gen_file_name)
            self.assertEqual('utf-8', self.obj.cp)
            lines = list(self.obj.get_lines(True))
            self.assertEqual(self.obj.SYS_ENCODING, self.obj.cp)    # on win self.obj.cp must be changed during of
            self.assertEqual(1, len(lines))                         # reading (see MockFileReader)
            new_string = lines[0].rstrip()
            if PY2:
                new_string = new_string.encode('utf-8')
            self.assertEqual(old_string, new_string)
        finally:
            if self.obj.fds:
                self.obj.fds.close()

            os.remove(gen_file_name)

    def test_decode_crash(self):
        self.configure(join(RESOURCES_DIR, 'jmeter', 'jtl', 'unicode.jtl'))
        self.obj.get_bytes(size=180)  # shouldn't crash with UnicodeDecodeError


class TestHTTPClient(BZTestCase):
    def test_proxy_setup(self):
        obj = HTTPClient()
        obj.add_proxy_settings({"address": "http://localhost:3128",
                                "username": "me",
                                "password": "too"})

        self.assertIn('http', obj.session.proxies)
        self.assertIn('https', obj.session.proxies)

        self.assertEqual(obj.session.proxies['http'], 'http://me:too@localhost:3128')
        self.assertEqual(obj.session.proxies['https'], 'http://me:too@localhost:3128')

    def test_proxy_ssl_cert(self):
        obj = HTTPClient()
        obj.add_proxy_settings({"ssl-cert": "i am server side cert",
                                "ssl-client-cert": "i am client side cert"})

        self.assertEqual(obj.session.verify, 'i am server side cert')
        self.assertEqual(obj.session.cert, 'i am client side cert')

    def test_jvm_args(self):
        obj = HTTPClient()
        obj.add_proxy_settings({"address": "http://localhost:3128",
                                "username": "me",
                                "password": "too"})
        jvm_args = obj.get_proxy_jvm_args()
        for protocol in ['http', 'https']:
            for key in ['proxyHost', 'proxyPort', 'proxyUser', 'proxyPass']:
                combo_key = protocol + '.' + key
                self.assertIn(combo_key, jvm_args)

    def test_download_file(self):
        obj = HTTPClient()
        fd, tmpfile = tempfile.mkstemp()
        os.close(fd)

        obj.download_file('http://httpbin.org/anything', tmpfile)

        self.assertTrue(os.path.exists(tmpfile))

        with open(tmpfile) as fds:
            contents = fds.read()

        self.assertGreaterEqual(len(contents), 0)

    def test_download_404(self):
        obj = HTTPClient()
        fd, tmpfile = tempfile.mkstemp()
        os.close(fd)

        self.assertRaises(TaurusNetworkError, lambda: obj.download_file('http://httpbin.org/status/404', tmpfile))

    def test_download_fail(self):
        obj = HTTPClient()
        fd, tmpfile = tempfile.mkstemp()
        os.close(fd)

        self.assertRaises(TaurusNetworkError, lambda: obj.download_file('http://non.existent.com/', tmpfile))

    def test_request(self):
        obj = HTTPClient()
        resp = obj.request('GET', 'http://httpbin.org/status/200')
        self.assertTrue(resp.ok)

    def test_request_fail(self):
        obj = HTTPClient()
        self.assertRaises(TaurusNetworkError, lambda: obj.request('GET', 'http://non.existent.com/'))
