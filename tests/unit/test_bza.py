from logging import Logger
from unittest.mock import patch, PropertyMock
from socketio.exceptions import ConnectionError
import time

from bzt import TaurusNetworkError
from bzt.bza import User, BZAObject, HappysocksClient, HappysocksEngineNamespace
from tests.unit import BZTestCase
from tests.unit.mocks import BZMock, MockHappysocksServer, EngineEmul


class TestBZAClient(BZTestCase):
    def test_bza_py3_unicode_token(self):
        mock = BZMock()
        mock.mock_get.update({
            'https://a.blazemeter.com/api/v4/web/version': {"result": {}},
        })

        user = User()
        mock.apply(user)
        user.token = str("something:something")
        user.ping()


class TestBZAObject(BZTestCase):
    def test_ping(self):
        obj = User()
        obj.ping()

    def test_request(self):
        obj = BZAObject()
        try:
            obj._request('https://a.blazemeter.com/api/v4/web/version', data={"test": 1})
            self.fail()
        except TaurusNetworkError:
            pass


class TestHappysocksClient(BZTestCase):
    NAMESPACE = "/v1/engine"

    @patch('bzt.bza.socketio.Client')
    def test_construct_1(self, mock_socketio_class):
        # prepare mocks
        sio = mock_socketio_class
        # perform test
        HappysocksClient("https://happysocks-5100-tester-dev.blazemeter.net", "r-v4-64102f1ab8795890049369",
                         "ci12NC02NDEwMmYxYWI", True, False)
        sio.assert_called_once()
        args = sio.call_args
        self.assertEqual(args[1]["http_session"].verify, False)
        self.assertIsInstance(args[1]["logger"], Logger)
        self.assertIsInstance(args[1]["engineio_logger"], Logger)

    @patch('bzt.bza.socketio.Client')
    def test_construct_2(self, mock_socketio_class):
        # prepare mocks
        sio = mock_socketio_class
        # perform test
        HappysocksClient("https://happysocks-5100-tester-dev.blazemeter.net", "r-v4-64102f1ab8795890049369",
                         "ci12NC02NDEwMmYxYWI", False, True)
        sio.assert_called_once()
        args = sio.call_args
        self.assertEqual(args[1]["http_session"].verify, True)
        self.assertEqual(args[1]["logger"], False)
        self.assertEqual(args[1]["engineio_logger"], False)

    @patch('bzt.bza.socketio.Client')
    def test_connect_error(self, mock_socketio_class):
        # prepare mocks
        sio = mock_socketio_class.return_value
        sio.connect.side_effect = ConnectionError("Invalid hostname")
        # perform test
        client = HappysocksClient("http://invalid", "r-v4-64102f1ab8795890049369", "ci12NC02NDEwMmYxYWI")
        try:
            client.connect()
            self.fail("Expected TaurusNetworkError")
        except TaurusNetworkError:
            pass
        sio.connect.assert_called_once()

    @patch('bzt.bza.socketio.Client')
    def test_connect_success(self, mock_socketio_class):
        # prepare mocks
        sio = mock_socketio_class.return_value
        # perform test
        client = HappysocksClient("https://happysocks-5100-tester-dev.blazemeter.net", "r-v4-64102f1ab8795890049369",
                                  "ci12NC02NDEwMmYxYWI", True, True)
        client.connect()
        sio.connect.assert_called_once()
        args = sio.connect.call_args
        self.assertEqual(args[0][0], "https://happysocks-5100-tester-dev.blazemeter.net")
        self.assertEqual(args[1]["namespaces"], ["/v1/engine"])
        self.assertEqual(args[1]["transports"], ["websocket"])
        self.assertEqual(args[1]["socketio_path"], "/api-ws")
        self.assertEqual(args[1]["headers"],
                         {"x-bzm-session": "r-v4-64102f1ab8795890049369", "x-auth-token": "ci12NC02NDEwMmYxYWI"})

    @patch('bzt.bza.socketio.Client')
    def test_connect_success_trailing_slash(self, mock_socketio_class):
        # prepare mocks
        sio = mock_socketio_class.return_value
        # perform test
        client = HappysocksClient("https://happysocks-5100-tester-dev.blazemeter.net/", "r-v4-64102f1ab8795890049369",
                                  "ci12NC02NDEwMmYxYWI", True, True)
        client.connect()
        sio.connect.assert_called_once()
        args = sio.connect.call_args
        self.assertEqual(args[0][0], "https://happysocks-5100-tester-dev.blazemeter.net")
        self.assertEqual(args[1]["namespaces"], ["/v1/engine"])
        self.assertEqual(args[1]["transports"], ["websocket"])
        self.assertEqual(args[1]["socketio_path"], "/api-ws")
        self.assertEqual(args[1]["headers"],
                         {"x-bzm-session": "r-v4-64102f1ab8795890049369", "x-auth-token": "ci12NC02NDEwMmYxYWI"})

    @patch('bzt.bza.socketio.Client')
    def test_connect_success_hs_path(self, mock_socketio_class):
        # prepare mocks
        sio = mock_socketio_class.return_value
        # perform test
        client = HappysocksClient("https://prod-rc.blazemeter.com/hs", "r-v4-64102f1ab8795890049369",
                                  "ci12NC02NDEwMmYxYWI", True, True)
        client.connect()
        sio.connect.assert_called_once()
        args = sio.connect.call_args
        self.assertEqual(args[0][0], "https://prod-rc.blazemeter.com")
        self.assertEqual(args[1]["namespaces"], ["/v1/engine"])
        self.assertEqual(args[1]["transports"], ["websocket"])
        self.assertEqual(args[1]["socketio_path"], "/hs/api-ws")
        self.assertEqual(args[1]["headers"],
                         {"x-bzm-session": "r-v4-64102f1ab8795890049369", "x-auth-token": "ci12NC02NDEwMmYxYWI"})

    @patch('bzt.bza.socketio.Client')
    def test_disconnect_not_connected(self, mock_socketio_class):
        # prepare mocks
        sio = mock_socketio_class.return_value
        sio._reconnect_abort = None
        # perform test
        client = HappysocksClient("https://happysocks-5100-tester-dev.blazemeter.net", "r-v4-64102f1ab8795890049369",
                                  "ci12NC02NDEwMmYxYWI", True, True)
        client.disconnect()
        sio.disconnect.assert_called_once()

    @patch('bzt.bza.socketio.Client')
    def test_disconnect_while_reconnecting(self, mock_socketio_class):
        # prepare mocks
        sio = mock_socketio_class.return_value
        # perform test
        client = HappysocksClient("https://happysocks-5100-tester-dev.blazemeter.net", "r-v4-64102f1ab8795890049369",
                                  "ci12NC02NDEwMmYxYWI", True, True)
        client.disconnect()
        sio._reconnect_abort.set.assert_called_once()
        sio.disconnect.assert_called_once()

    @patch('bzt.bza.socketio.Client')
    def test_disconnect_connected(self, mock_socketio_class):
        # prepare mocks
        sio = mock_socketio_class.return_value
        # perform test
        client = HappysocksClient("https://happysocks-5100-tester-dev.blazemeter.net", "r-v4-64102f1ab8795890049369",
                                  "ci12NC02NDEwMmYxYWI", True, True)
        client.connect()
        client.disconnect()
        sio.connect.assert_called_once()
        sio.disconnect.assert_called_once()

    @patch('bzt.bza.socketio.Client')
    def test_send_engine_metrics(self, mock_socketio_class):
        # prepare mocks
        sio = mock_socketio_class.return_value
        # perform test
        client = HappysocksClient("https://prod-rc.blazemeter.com/hs", "r-v4-64102f1ab8795890049369",
                                  "ci12NC02NDEwMmYxYWI", True, True)
        client.send_engine_metrics([
            {
                'metadata': {
                    'source': 'local',
                    'entityId': 'r-v4-64102f1ab8795890049369',
                    'masterId': 100,
                    'calibrationId': 200,
                    'calibrationStepId': 300,
                },
                'timestamp': 1678892271398,
                'values': {
                    'cpu': 9.4,
                    'mem': 5560.0,
                }
            }
        ], HappysocksEngineNamespace.METRICS_EVENT)
        # verify
        sio.emit.assert_called_once()
        args = sio.emit.call_args
        self.assertEqual(args[0][0], "metrics")
        self.assertEqual(args[0][1], [{'metadata': {'source': 'local', 'entityId': 'r-v4-64102f1ab8795890049369',
                                                    'masterId': 100, 'calibrationId': 200, 'calibrationStepId': 300},
                                       'timestamp': 1678892271398, 'values': {'cpu': 9.4, 'mem': 5560.0}}])
        self.assertEqual(args[0][2], "/v1/engine")
        self.assertTrue(args[1]["callback"] is not None)


class TestHappysocksClientMockServer(BZTestCase):
    NAMESPACE = "/v1/engine"

    def setUp(self):
        super().setUp()
        self.server = MockHappysocksServer()
        self.server.start()
        self.address = f"http://{self.server.server_host}:{self.server.server_port}"
        self.client = HappysocksClient(self.address, "r-v4-64102f1ab8795890049369", "ci12NC02NDEwMmYxYWI", False, False)

    def tearDown(self):
        super().tearDown()
        if self.client:
            self.client.disconnect()
        self.server.stop()

    def test_connect_success(self):
        self.client.connect()
        self.assertTrue(self.server.engine_namespace.connect_event.isSet())

    def test_connect_error(self):
        self.server.engine_namespace.accept_connect = False
        try:
            self.client.connect()
            self.fail("Expected TaurusNetworkError")
        except TaurusNetworkError:
            pass

    def test_send_metrics_success(self):
        self.server.engine_namespace.metrics_response = {}
        self.client.connect()
        metrics_to_send = [
            {
                'metadata': {
                    'source': 'local',
                    'entityId': 'r-v4-64102f1ab8795890049369',
                    'masterId': 100,
                    'calibrationId': 200,
                    'calibrationStepId': 300,
                },
                'timestamp': 1678892271398,
                'values': {
                    'cpu': 9.4,
                    'mem': 5560.0,
                }
            }
        ]
        self.client.send_engine_metrics(metrics_to_send, HappysocksEngineNamespace.METRICS_EVENT)
        self.server.engine_namespace.metrics_event.wait()
        self.assertEqual(self.server.engine_namespace.received_metrics, metrics_to_send)

    def test_send_metrics_error(self):
        self.server.engine_namespace.metrics_response = {"error": "Missing entityId at index 0"}
        self.client.connect()
        metrics_to_send = [
            {
                'timestamp': 1678892271398,
                'values': {
                    'cpu': 9.4,
                    'mem': 5560.0,
                }
            }
        ]
        self.client.send_engine_metrics(metrics_to_send, HappysocksEngineNamespace.METRICS_EVENT)
        self.server.engine_namespace.metrics_event.wait()
        self.assertEqual(self.server.engine_namespace.received_metrics, metrics_to_send)
        # error while processing metrics in happysocks is handled by HappysocksEngineNamespace callback and logged
        # we have no reliable way of waiting for the callback before disconnect
        time.sleep(0.1)

    def test_send_metrics_not_connected(self):
        self.server.engine_namespace.metrics_response = {}
        metrics_to_send = [
            {
                'metadata': {
                    'source': 'local',
                    'entityId': 'r-v4-64102f1ab8795890049369',
                    'masterId': 100,
                    'calibrationId': 200,
                    'calibrationStepId': 300,
                },
                'timestamp': 1678892271398,
                'values': {
                    'cpu': 9.4,
                    'mem': 5560.0,
                }
            }
        ]
        try:
            self.client.send_engine_metrics(metrics_to_send, HappysocksEngineNamespace.METRICS_EVENT)
            self.fail("Expected TaurusNetworkError")
        except TaurusNetworkError:
            pass

    def test_get_load_from_config(self):
        engine = EngineEmul()
        obj = BZAObject()
        concurrency, duration = obj._get_load_from_config(engine.config)
        self.assertEqual(1, concurrency)
        self.assertEqual(1, duration)

        engine.config['execution'] = [{"concurrency": {"local": 10}, "ramp-up": "2m", "hold-for": "10m"}]
        engine.config['provisioning'] = "local"
        concurrency, duration = obj._get_load_from_config(engine.config)
        self.assertEqual(10, concurrency)
        self.assertEqual(12, duration)
