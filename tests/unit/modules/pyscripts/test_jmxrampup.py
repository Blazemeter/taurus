import io
import logging
import socket
from multiprocessing import AuthenticationError
from unittest import mock

from bzt.modules.pyscripts.jmxrampup import JmeterRampupProcess
from tests.unit import BZTestCase


class TestJmeterRampupProcess(BZTestCase):
    def setUp(self):
        super(TestJmeterRampupProcess, self).setUp()
        self.log = logging.getLogger('')
        self.log.setLevel(logging.DEBUG)
        self.captured_logger = None

    def tearDown(self):
        pass

    def test_jmeter_rampup(self):
        rampup = JmeterRampupProcess([('localhost', 9000)],
                                     'localhost', 6000,
                                     'test_key', '.', logging.DEBUG)
        rampup.log = self.log
        self.sniff_log(rampup.log)

        real_timeout = socket.timeout
        data_to_send = {'ramp_up_duration': 10, 'ramp_up_steps': 2, 'concurrency': 5}
        with mock.patch.object(rampup, '_stop', side_effect=[False] * 2 + [True]):
            with mock.patch.object(rampup, '_get_current_concurrency', return_value="1"):
                with mock.patch.object(rampup, '_now', side_effect=[100_000, 100_001, 100_301, 100_601]):
                    with mock.patch('bzt.modules.pyscripts.jmxrampup.Client') as mock_client:
                        with mock.patch('bzt.modules.pyscripts.jmxrampup.socket') as mock_socket:
                            mock_client.return_value.__enter__.return_value.recv.side_effect = [data_to_send] + [ConnectionRefusedError] * 2
                            mock_socket.socket().recv().decode.side_effect = ["BeanShell ignored\n", "bsh % test1\n", real_timeout,
                                                                              "BeanShell ignored\n", "bsh % test2\n", BaseException]
                            mock_socket.timeout = real_timeout

                            rampup.run()

                            self.assertIn('Got new rampup configuration', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Rampup plan: deque([(1, 100000.0), (3, 100300.0), (5, 100600.0)])', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Setting concurrency: (1, 100000.0)', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Setting concurrency: (3, 100300.0)', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Setting concurrency: (5, 100600.0)', self.log_recorder.info_buff.getvalue())
                            mock_socket.socket().connect.assert_called_with(('localhost', 9001))
                            mock_socket.socket().sendall.assert_has_calls([
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                          b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","1");'),
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                     b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","3");'),
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                     b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","5");')
                            ])

                            self.assertIn('Beanshell recv: test1', self.log_recorder.debug_buff.getvalue())
                            self.assertIn('Beanshell recv: test2', self.log_recorder.debug_buff.getvalue())

    def test_jmeter_rampup_unauthenticated(self):
        rampup = JmeterRampupProcess([('localhost', 9000)],
                                     'localhost', 6000,
                                     'test_key', '.', logging.DEBUG)
        rampup.log = self.log
        self.sniff_log(rampup.log)

        with mock.patch.object(rampup, '_stop', side_effect=[True]):
            with mock.patch('builtins.print'):
                with mock.patch('bzt.modules.pyscripts.jmxrampup.sys.exit') as mock_sys_exit:
                    with mock.patch('bzt.modules.pyscripts.jmxrampup.Client', side_effect=AuthenticationError) as mock_client:
                        rampup.run()

                        mock_sys_exit.assert_called()
                        self.assertIn('Forbidden. Rampup server returns AuthenticationError.', self.log_recorder.err_buff.getvalue())

    def test_jmeter_rampup_get_current_concurrency(self):
        rampup = JmeterRampupProcess([('localhost', 9000)],
                                     'localhost', 6000,
                                     'test_key', '.', logging.DEBUG)
        rampup.log = self.log
        self.sniff_log(rampup.log)

        with mock.patch('builtins.open', side_effect=FileNotFoundError):
            self.assertEqual(0, int(rampup._get_current_concurrency("kpi.jtl")))

        kpi = b"1706516497121,884,Get Deck ID,200,OK,setUp 1-1,true,1560,1,1,312,local,130\n" \
              b"1706516500407,809,Draw Card,200,OK,main Thread-ThreadStarter 1-1,true,1778,5,5,303,local,119\n" \
              b"1706516501401,784,Draw Card,200,OK,main Thread-ThreadStarter 1-6,true,1781,10,10,318,local,118\n"
        with io.BytesIO(kpi) as f:
            f.read(65536)
            with mock.patch('builtins.open', return_value=f):
                self.assertEqual(10, int(rampup._get_current_concurrency("kpi.jtl")))

        kpi = b"1706516497121,884,Get Deck ID,200,OK,setUp 1-1,true,1560,1,1,312,local,130\n" \
              b"1706516500407,809,Draw Card,200,OK,main Thread-ThreadStarter 1-1,true,1778,5,5,303,local,119\n" \
              b"1706516501401,784,Draw Card,"  # not finished line, get previous one
        with io.BytesIO(kpi) as f:
            f.read(65536)
            with mock.patch('builtins.open', return_value=f):
                self.assertEqual(5, int(rampup._get_current_concurrency("kpi.jtl")))

        kpi = b"1706516497121,884,Get Deck ID,200,OK,setUp 1-1,true,1560,1,1,312,local,130\n" \
              b"1706516500407,809,Draw Card,200,OK,main Thread-ThreadStarter 1-1,true,1778,5,5,303,local,119\n" \
              b"WHATEVER\n"  # finished line, but unable to parse
        with io.BytesIO(kpi) as f:
            f.read(65536)
            with mock.patch('builtins.open', return_value=f):
                self.assertEqual(0, int(rampup._get_current_concurrency("kpi.jtl")))

    def test_jmeter_rampup_no_steps_no_duration(self):
        rampup = JmeterRampupProcess([('localhost', 9000)],
                                     'localhost', 6000,
                                     'test_key', '.', logging.DEBUG)
        rampup.log = self.log
        self.sniff_log(rampup.log)

        real_timeout = socket.timeout
        data_to_send = {'ramp_up_duration': None, 'ramp_up_steps': None, 'concurrency': 5}
        with mock.patch.object(rampup, '_stop', side_effect=[False, True]):
            with mock.patch.object(rampup, '_get_current_concurrency', return_value="1"):
                with mock.patch.object(rampup, '_now', side_effect=[100_000, 100_001, 100_301, 100_601]):
                    with mock.patch('bzt.modules.pyscripts.jmxrampup.Client') as mock_client:
                        with mock.patch('bzt.modules.pyscripts.jmxrampup.socket') as mock_socket:
                            mock_client.return_value.__enter__.return_value.recv.side_effect = [data_to_send, ConnectionRefusedError]
                            mock_socket.socket().recv().decode.side_effect = ["BeanShell ignored\n", "bsh % test1\n", real_timeout,
                                                                              "BeanShell ignored\n", "bsh % test2\n", BaseException]
                            mock_socket.timeout = real_timeout

                            rampup.run()

                            self.assertIn('Got new rampup configuration', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Rampup plan: deque([(1, 100000.0), (5, 100000.0)])', self.log_recorder.info_buff.getvalue())

                            self.assertIn('Setting concurrency: (1, 100000.0)', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Setting concurrency: (5, 100000.0)', self.log_recorder.info_buff.getvalue())
                            mock_socket.socket().connect.assert_called_with(('localhost', 9001))
                            mock_socket.socket().sendall.assert_has_calls([
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                          b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","1");'),
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                          b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","5");')
                            ])

                            self.assertIn('Beanshell recv: test1', self.log_recorder.debug_buff.getvalue())

    def test_jmeter_rampup_no_steps(self):
        rampup = JmeterRampupProcess([('localhost', 9000)],
                                     'localhost', 6000,
                                     'test_key', '.', logging.DEBUG)
        rampup.log = self.log
        self.sniff_log(rampup.log)

        real_timeout = socket.timeout
        data_to_send = {'ramp_up_duration': 10, 'ramp_up_steps': None, 'concurrency': 5}
        with mock.patch.object(rampup, '_stop', side_effect=[False] * 5 + [True]):
            with mock.patch.object(rampup, '_get_current_concurrency', return_value="1"):
                with mock.patch.object(rampup, '_now', side_effect=[100_000, 100_001, 100_151, 100_301, 100_451, 100_601]):
                    with mock.patch('bzt.modules.pyscripts.jmxrampup.Client') as mock_client:
                        with mock.patch('bzt.modules.pyscripts.jmxrampup.socket') as mock_socket:
                            mock_client.return_value.__enter__.return_value.recv.side_effect = [data_to_send] + [ConnectionRefusedError] * 5
                            mock_socket.socket().recv().decode.side_effect = ["BeanShell ignored\n", "bsh % test1\n", real_timeout,
                                                                              "BeanShell ignored\n", "bsh % test2\n",
                                                                              "BeanShell ignored\n", "bsh % test3\n",
                                                                              "BeanShell ignored\n", "bsh % test4\n", BaseException]
                            mock_socket.timeout = real_timeout

                            rampup.run()

                            self.assertIn('Got new rampup configuration', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Rampup plan: deque([(1, 100000.0), (2, 100150.0), (3, 100300.0), (4, 100450.0), (5, 100600.0)])', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Setting concurrency: (1, 100000.0)', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Setting concurrency: (2, 100150.0)', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Setting concurrency: (3, 100300.0)', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Setting concurrency: (4, 100450.0)', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Setting concurrency: (5, 100600.0)', self.log_recorder.info_buff.getvalue())
                            mock_socket.socket().connect.assert_called_with(('localhost', 9001))
                            mock_socket.socket().sendall.assert_has_calls([
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                          b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","1");'),
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                          b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","2");'),
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                          b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","3");'),
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                          b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","4");'),
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                          b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","5");')
                            ])

                            self.assertIn('Beanshell recv: test1', self.log_recorder.debug_buff.getvalue())
                            self.assertIn('Beanshell recv: test2', self.log_recorder.debug_buff.getvalue())

    def test_jmeter_rampup_bad_concurrency(self):
        rampup = JmeterRampupProcess([('localhost', 9000)],
                                     'localhost', 6000,
                                     'test_key', '.', logging.DEBUG)
        rampup.log = self.log
        self.sniff_log(rampup.log)

        real_timeout = socket.timeout
        data_to_send = {'ramp_up_duration': 10, 'ramp_up_steps': 2, 'concurrency': None}
        with mock.patch.object(rampup, '_stop', side_effect=[False, True]):
            with mock.patch.object(rampup, '_get_current_concurrency', return_value="1"):
                with mock.patch.object(rampup, '_now', side_effect=[100_000, 100_301, 100_601]):
                    with mock.patch('bzt.modules.pyscripts.jmxrampup.Client') as mock_client:
                        with mock.patch('bzt.modules.pyscripts.jmxrampup.socket') as mock_socket:
                            mock_client.return_value.__enter__.return_value.recv.side_effect = [data_to_send, ConnectionRefusedError]
                            mock_socket.socket().recv().decode.side_effect = ["BeanShell ignored\n", "bsh % test1\n", real_timeout,
                                                                              "BeanShell ignored\n", "bsh % test2\n", BaseException]
                            mock_socket.timeout = real_timeout

                            rampup.run()

                            self.assertIn('Got new rampup configuration', self.log_recorder.info_buff.getvalue())


    def test_jmeter_rampup_just_header_in_kpi(self):
        rampup = JmeterRampupProcess([('localhost', 9000)],
                                     'localhost', 6000,
                                     'test_key', '.', logging.DEBUG)
        rampup.log = self.log
        self.sniff_log(rampup.log)

        real_timeout = socket.timeout
        data_to_send = {'ramp_up_duration': 10, 'ramp_up_steps': 2, 'concurrency': 5}
        with mock.patch.object(rampup, '_stop', side_effect=[False] * 2 + [True]):
            with mock.patch.object(rampup, '_get_current_concurrency', return_value="grpThreads"):
                with mock.patch.object(rampup, '_now', side_effect=[100_000, 100_001, 100_301, 100_601]):
                    with mock.patch('bzt.modules.pyscripts.jmxrampup.Client') as mock_client:
                        with mock.patch('bzt.modules.pyscripts.jmxrampup.socket') as mock_socket:
                            mock_client.return_value.__enter__.return_value.recv.side_effect = [data_to_send] + [ConnectionRefusedError] * 2
                            mock_socket.socket().recv().decode.side_effect = ["BeanShell ignored\n", "bsh % test1\n", BaseException,
                                                                              "BeanShell ignored\n", "bsh % test2\n", ]
                            mock_socket.timeout = real_timeout

                            rampup.run()

                            self.assertIn('Got new rampup configuration', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Rampup plan: deque([(1, 100000.0), (3, 100300.0), (5, 100600.0)])', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Setting concurrency: (1, 100000.0)', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Setting concurrency: (3, 100300.0)', self.log_recorder.info_buff.getvalue())
                            self.assertIn('Setting concurrency: (5, 100600.0)', self.log_recorder.info_buff.getvalue())
                            mock_socket.socket().connect.assert_called_with(('localhost', 9001))
                            mock_socket.socket().sendall.assert_has_calls([
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                          b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","1");'),
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                          b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","3");'),
                                mock.call(b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_RampUp","0");'
                                          b'org.apache.jmeter.util.JMeterUtils.setProperty("BM_CTG_TargetLevel","5");')
                            ])

                            self.assertIn('Beanshell recv: test1', self.log_recorder.debug_buff.getvalue())
                            self.assertIn('Beanshell recv: test2', self.log_recorder.debug_buff.getvalue())