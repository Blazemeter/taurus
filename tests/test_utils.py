# coding=utf-8
""" unit test """
import os
import sys
import logging

from psutil import Popen
from os.path import join

from bzt import TaurusNetworkError
from bzt.utils import log_std_streams, get_uniq_name, JavaVM, ToolError, is_windows, HTTPClient, BetterDict
from bzt.utils import ensure_is_dict, Environment, temp_file, communicate
from tests import BZTestCase, RESOURCES_DIR
from tests.mocks import MockFileReader


class MockPopen(object):
    def __init__(self, out, err):
        self.out = out
        self.err = err

    def communicate(self):
        return self.out, self.err


class TestEnvironment(BZTestCase):
    def test_nesting(self):
        v1 = 'val_param_name'
        v2 = 'path_param_name'
        v3 = 'const_val'
        os.environ[v1] = 'v1.1'
        os.environ[v2] = 'v1.2'
        os.environ[v3] = 'v1.3'

        e1 = Environment()
        e1.set({v1: 'local_val1.1'})
        e1.add_path({v2: 'param_val1.1'}, finish=True)
        e2 = Environment(parent=e1)
        e1.add_path({v2: 'param_val1.3'}, finish=True)
        os.environ[v1] = 'v2.1'
        os.environ[v2] = 'v2.2'
        os.environ[v3] = 'v2.3'
        e1.set({v1: 'local_val1.2'})
        e2.add_path({v2: 'param_val1.2'}, finish=True)

        self.assertEqual(e1.get(v1), 'local_val1.2')
        self.assertEqual(e2.get(v1), 'local_val1.1')
        self.assertEqual(e1.get(v2), os.pathsep.join(('v2.2', 'param_val1.1', 'param_val1.3')))
        self.assertEqual(e2.get(v2), os.pathsep.join(('v2.2', 'param_val1.1', 'param_val1.2')))
        self.assertEqual(e1.get(v3), 'v2.3')
        self.assertEqual(e2.get(v3), 'v2.3')


class TestBetterDict(BZTestCase):
    def _merge_and_compare(self, first, second, result):
        sample = BetterDict().merge(first)
        sample.merge(second)
        result = BetterDict().merge(result)
        self.assertEqual(sample, result)

    def _filter_and_compare(self, first, second, result, black_list=False):
        sample = BetterDict().merge(first)
        sample.filter(second, black_list=black_list)
        result = BetterDict().merge(result)
        self.assertEqual(sample, result)

    def test_merge_configs(self):
        a = {"modules": {"local": "class_name"}}
        b = {"modules": {"local": {"class": "another_class"}}}
        res = BetterDict()
        res.merge(a)
        res.merge(b)
        self.assertEqual(BetterDict.__name__, type(res["modules"]["local"]).__name__)
        modules = res["modules"]
        ensure_is_dict(modules, "local", "class")
        self.assertEqual("another_class", res["modules"]["local"]["class"])

    def test_merge_del(self):
        a = {
            "A": ["B", "C"],
            "B": {"A": "vA"}}
        b = {
            "^A": {"^D": "E"},
            "^X": "Y"}
        res = {"B": {"A": "vA"}}
        self._merge_and_compare(a, b, res)

    def test_merge_overwrite(self):
        a = {
            "A": ["B", "C"],
            "B": {"A": "vA"}}
        b = {"~B": {"~C": "vC"}}
        res = {
            "A": ["B", "C"],
            "B": {"C": "vC"}}
        self._merge_and_compare(a, b, res)

    def test_merge_list_elements(self):
        a = {
            "A": ["B", "C"],
            "B": {"A": "vA"},
            "D": ["E", "F"]}
        b = {
            "$A": ["nB"],
            "$B": {"nC": "vC"},
            "$C": ["D"]}
        res = {
            "A": ["nB", "C"],
            "B": {"A": "vA", "nC": "vC"},
            "D": ["E", "F"],
            "C": ["D"]}

        self._merge_and_compare(a, b, res)

    def test_filter_wl0(self):
        a = {
            "A": False,
            "C": {"D": "E", "G": "GG"},
            "F": ["FF"]}
        b = {
            "A": True,
            "!C": {"G": "H"}}
        res = {
            "A": False,
            "C": {"D": "E"}}

        self._filter_and_compare(a, b, res)

    def test_filter_wl1(self):
        a = {
            "A": ["B", "BB"],
            "C": {"D": "E", "G": "GG"},
            "F": ["FF"]}
        b = {
            "A": True,
            "!C": {"G": "H"}}
        res = {
            "A": ["B", "BB"],
            "C": {"D": "E"}}

        self._filter_and_compare(a, b, res)

    def test_filter_wl2(self):
        a = {
            "A": "B",
            "C": {"D": "E"}}
        b = {
            "A": {"B": "C"},
            "C": True}
        res = {
            "C": {"D": "E"}}

        self._filter_and_compare(a, b, res)

    def test_filter_bl0(self):
        a = {
            "A": ["B", "BB"],
            "C": {"D": "E", "G": "GG"},
            "F": ["FF"]}
        b = {
            "A": True,
            "!C": {"G": "H"}}
        res = {
            "F": ["FF"],
            "C": {"G": "GG"}}

        self._filter_and_compare(a, b, res, black_list=True)

    def test_filter_bl1(self):
        a = {
            "A": "B",
            "C": {"D": "E"}}
        b = {
            "A": {"B": "C"},
            "C": True}
        res = {
            "A": "B"}

        self._filter_and_compare(a, b, res, black_list=True)


class TestMisc(BZTestCase):
    def test_communicate(self):
        self.sniff_log()

        out = b'\xf1\xe5\xedoutput'     # on py2 bytes is just str synonym
        err = b'\xf1\xe5\xederror'

        obj = MockPopen(out, err)

        output = communicate(obj)

        self.assertEqual(output, ("output", "error"))


class TestJavaVM(BZTestCase):
    def test_missed_tool(self):
        self.obj = JavaVM()
        self.obj.tool_path = 'java-not-found'
        self.assertEqual(False, self.obj.check_if_installed())
        self.obj.install()

    def test_missed_req_tool(self):
        self.obj = JavaVM()
        self.obj.tool_path = 'java-not-found'
        self.obj.mandatory = True
        self.assertEqual(False, self.obj.check_if_installed())
        self.assertRaises(ToolError, self.obj.install)

    def test_get_version(self):
        self.obj = JavaVM()

        out1 = "openjdk version \"10.0.1\" 2018-04-17\nOpenJDK Runtime Environment (build " \
               "10.0.1+10-Ubuntu-3ubuntu1)\nOpenJDK 64-Bit Server VM (build 10.0.1+10-Ubuntu-3ubuntu1, mixed mode)"
        out2 = "java version \"1.8.0_151\"\nJava(TM) SE Runtime Environment (build 1.8.0_151-b12)\n" \
               "Java HotSpot(TM) 64-Bit Server VM (build 25.151-b12, mixed mode)"

        self.assertEqual("10", self.obj._get_version(out1))
        self.assertEqual("8", self.obj._get_version(out2))


class TestLogStreams(BZTestCase):
    def test_streams(self):
        self.sniff_log()

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
        gen_file_name = temp_file()

        mod_str = old_string + '\n'

        with open(gen_file_name, 'wb') as fd:  # use target system encoding for writing
            fd.write(mod_str.encode(self.obj.SYS_ENCODING))  # important on win where it's not 'utf-8'

        try:
            self.configure(gen_file_name)
            self.assertEqual('utf-8', self.obj.cp)
            lines = list(self.obj.get_lines(True))
            self.assertEqual(self.obj.SYS_ENCODING, self.obj.cp)  # on win self.obj.cp must be changed during of
            self.assertEqual(1, len(lines))  # reading (see MockFileReader)
            new_string = lines[0].rstrip()
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
        jvm_args = obj.get_proxy_props()
        for protocol in ['http', 'https']:
            for key in ['proxyHost', 'proxyPort', 'proxyUser', 'proxyPass']:
                combo_key = protocol + '.' + key
                self.assertIn(combo_key, jvm_args)

    def test_download_file(self):
        obj = HTTPClient()
        tmpfile = temp_file()

        obj.download_file('http://localhost:8000/', tmpfile)

        self.assertTrue(os.path.exists(tmpfile))

        with open(tmpfile) as fds:
            contents = fds.read()

        self.assertGreaterEqual(len(contents), 0)

    def test_download_404(self):
        obj = HTTPClient()
        tmpfile = temp_file()

        self.assertRaises(TaurusNetworkError, lambda: obj.download_file('http://localhost:8000/404', tmpfile))

    def test_download_fail(self):
        obj = HTTPClient()
        tmpfile = temp_file()

        self.assertRaises(TaurusNetworkError, lambda: obj.download_file('http://non.existent.com/', tmpfile))

    def test_request(self):
        obj = HTTPClient()
        resp = obj.request('GET', 'http://localhost:8000/')
        self.assertTrue(resp.ok)

    def test_request_fail(self):
        obj = HTTPClient()
        self.assertRaises(TaurusNetworkError, lambda: obj.request('GET', 'http://non.existent.com/'))
