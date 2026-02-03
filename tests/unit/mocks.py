""" test """
import datetime
import os
import random
import sys
import socketio
import threading
import time
from _socket import SOCK_STREAM, AF_INET
from collections import Counter
from random import random
from socketio.exceptions import ConnectionRefusedError
from gevent import pywsgi
from geventwebsocket.handler import WebSocketHandler

import requests

from bzt.bza import call_with_retry
from bzt.modules import TransactionListener
from bzt.modules.aggregator import DataPoint, KPISet, ResultsReader, AggregatorListener, ConsolidatingAggregator
from bzt.modules.functional import FunctionalResultsReader, FunctionalAggregatorListener
from bzt.utils import b, load_class, to_json, get_full_path, get_uniq_name, FileReader, is_windows, temp_file
from bzt.engine import Engine, Configuration, Singletone, Service, Provisioning, Reporter, ScenarioExecutor

from tests.unit.base import TEST_DIR, ROOT_LOGGER, BUILD_DIR

try:
    from exceptions import KeyboardInterrupt
except ImportError:
    # noinspection PyUnresolvedReferences
    from builtins import KeyboardInterrupt


def r(mul=5):
    return 1 + int(mul * random()) / 1000.0


def rc():
    return "%s00" % (int(4 * random()) + 1)


def err():
    if int(50 * random()) == 0:
        return "Some Error"
    else:
        return None


def random_sample(ts, label='', conc=1):
    return ts, label, conc, r(), r(), r(), rc(), err()


def random_datapoint(n):
    point = DataPoint(n)
    overall = point[DataPoint.CURRENT].setdefault('', KPISet())
    overall[KPISet.CONCURRENCY] = int(r(100) * 100)
    overall[KPISet.SAMPLE_COUNT] = int(100 * r(1000)) + 1
    overall[KPISet.SUCCESSES] = int(overall[KPISet.SAMPLE_COUNT] * random())
    overall[KPISet.FAILURES] = overall[KPISet.SAMPLE_COUNT] - overall[KPISet.SUCCESSES]
    overall[KPISet.BYTE_COUNT] = int(random() * 1000) + 1
    overall[KPISet.PERCENTILES]['25.0'] = r(10)
    overall[KPISet.PERCENTILES]['50.0'] = r(20)
    overall[KPISet.PERCENTILES]['75.0'] = r(30)
    overall[KPISet.PERCENTILES]['90.0'] = r(40)
    overall[KPISet.PERCENTILES]['99.0'] = r(50)
    overall[KPISet.PERCENTILES]['100.0'] = r(100)
    overall[KPISet.RESP_CODES][rc()] = 1

    overall[KPISet.AVG_RESP_TIME] = r(100)
    overall[KPISet.AVG_CONN_TIME] = overall[KPISet.AVG_RESP_TIME] / 3.0
    overall[KPISet.AVG_LATENCY] = 2.0 * overall[KPISet.AVG_RESP_TIME] / 3.0

    overall.sum_rt = overall[KPISet.AVG_RESP_TIME] * overall[KPISet.SAMPLE_COUNT]
    overall.sum_cn = overall[KPISet.AVG_CONN_TIME] * overall[KPISet.SAMPLE_COUNT]
    overall.sum_lt = overall[KPISet.AVG_LATENCY] * overall[KPISet.SAMPLE_COUNT]
    cumul = point[DataPoint.CUMULATIVE].setdefault('', KPISet())
    cumul.merge_kpis(overall)
    cumul.recalculate()

    point.recalculate()

    overall[KPISet.AVG_RESP_TIME] = r(100)
    overall[KPISet.AVG_CONN_TIME] = overall[KPISet.AVG_RESP_TIME] / 3.0
    overall[KPISet.AVG_LATENCY] = 2.0 * overall[KPISet.AVG_RESP_TIME] / 3.0
    return point


class MockFileReader(FileReader):
    SYS_ENCODING = 'cp1251' if is_windows() else 'utf-8'


class DummyOut:
    def write(self, _):
        pass

    def flush(self):
        pass

    def isatty(self):
        return False

    def getvalue(self):
        return ""


class EngineEmul(Engine):
    def __init__(self, custom_configs=None):
        super(EngineEmul, self).__init__(ROOT_LOGGER)
        self.aggregator.add_underling = lambda _: None
        directory = get_full_path(TEST_DIR)
        prefix = datetime.datetime.now().strftime(self.ARTIFACTS_DIR)
        self.config.merge({
            "provisioning": "local",
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__,
                "local": ModuleMock.__module__ + "." + ModuleMock.__name__},
            "settings": {
                "check-updates": False,
            }})

        if custom_configs:
            self.config.merge(custom_configs)
        else:
            self.config.merge({
                "settings": {
                    "artifacts-dir": get_uniq_name(directory=directory, prefix=prefix),
                }
            })

        self.check_interval = 0.1
        self.create_artifacts_dir()
        self.prepare_exc = None
        self.was_finalize = False
        self.temp_pythonpath = BUILD_DIR + 'pyinstaller/'
        self.user_pythonpath = self.temp_pythonpath
        os.environ['PYTHONPATH'] = self.temp_pythonpath

    def dump_config(self):
        """ test """
        fname = temp_file()
        self.config.dump(fname, Configuration.JSON)
        with open(fname) as fh:
            ROOT_LOGGER.debug("JSON:\n%s", fh.read())

    def prepare(self):
        if self.prepare_exc:
            raise self.prepare_exc
        return super(EngineEmul, self).prepare()


class ModuleMock(ScenarioExecutor, Provisioning, Reporter, Service):
    """ mock """

    def __init__(self):
        super(ModuleMock, self).__init__()
        self.postproc_exc = None
        self.check_exc = None
        self.prepare_exc = None
        self.startup_exc = None
        self.shutdown_exc = None

        self.check_iterations = sys.maxsize

        self.was_shutdown = False
        self.was_startup = False
        self.was_prepare = False
        self.was_check = False
        self.was_postproc = False

        self.is_has_results = False

    def prepare(self):
        """
        :raise self.prepare_exc:
        """
        self.log.info("Preparing mock")
        self.was_prepare = True
        self.check_iterations = int(self.settings.get('check_iterations', "2"))
        self.postproc_exc = self.get_exc("postproc")
        self.check_exc = self.get_exc("check")
        self.prepare_exc = self.get_exc("prepare")
        self.startup_exc = self.get_exc("startup")
        self.shutdown_exc = self.get_exc("shutdown")

        if isinstance(self.engine.aggregator, ResultsReader):
            reader = MockReader()
            for num in range(0, self.check_iterations):
                for quan in range(0, int(random.random() * 10)):
                    reader.data.append(random_sample(num))
            # noinspection PyUnresolvedReferences
            self.engine.aggregator.add_reader(reader)

        if self.prepare_exc:
            raise self.prepare_exc

    def startup(self):
        """
        :raise self.startup_exc:
        """
        self.log.info("Startup mock")
        self.was_startup = True
        if self.startup_exc:
            raise self.startup_exc

    def check(self):
        """
        :return: :raise self.check_exc:
        """
        self.was_check = True
        self.log.info("Checks remaining: %s", self.check_iterations)
        self.check_iterations -= 1
        if not self.check_iterations:
            if self.check_exc:
                raise self.check_exc
            else:
                return True
        return False

    def shutdown(self):
        """
        :raise self.shutdown_exc:
        """
        self.log.info("Shutdown mock")
        self.was_shutdown = True
        if self.shutdown_exc:
            raise self.shutdown_exc

    def post_process(self):
        """
        :raise self.postproc_exc:
        """
        self.log.info("Postproc mock")
        self.was_postproc = True
        if self.postproc_exc:
            raise self.postproc_exc

    def get_exc(self, param):
        """
        :type param: str
        :return:
        """
        name = self.settings.get(param, "")
        if name:
            cls = load_class(name)
            return cls()
        return None

    def resource_files(self):
        """
        :return:
        """
        self.execution.get('files', [], force_set=True).append(__file__)
        return [__file__]

    def has_results(self):
        return self.is_has_results

    def install_required_tools(self):
        self.log.debug("All is good")

    def get_error_diagnostics(self):
        return ['DIAGNOSTICS']


class MockListener(Reporter, AggregatorListener):
    def __init__(self):
        super(MockListener, self).__init__()
        self.results = []

    def aggregated_second(self, data):
        aggregator = self.engine.aggregator
        if isinstance(aggregator, ConsolidatingAggregator):
            data = aggregator.converter(data)
        self.results.append(data)


class MockReader(ResultsReader, AggregatorListener):
    """
    test
    """

    def _ramp_up_exclude(self):
        return False    # must be implemented as abstract method

    def __init__(self):
        super(MockReader, self).__init__()
        self.results = []
        self.data = []
        self.add_listener(self)
        self.track_percentiles = [0, 50, 90, 99, 99.5, 100]

    def _read(self, final_pass=False):
        """
        Emulating read samples

        :type final_pass: bool
        :return:
        """
        while self.data:
            yield self.data.pop(0)

    def aggregated_second(self, data):
        """
        Store and assert aggregate sequence

        :type data: dict
        :raise AssertionError:
        """
        if self.results:
            if self.results[-1]["ts"] >= data["ts"]:
                raise AssertionError("TS sequence wrong: %s>=%s" % (self.results[-1]["ts"], data["ts"]))
        self.results.append(data)


class MockFunctionalReader(FunctionalResultsReader):
    def __init__(self):
        super(MockFunctionalReader, self).__init__()
        self.data = []

    def read(self, last_pass=False):
        while self.data:
            yield self.data.pop(0)


class MockFunctionalListener(FunctionalAggregatorListener):
    def __init__(self):
        self.results = []

    def aggregated_results(self, result, cumulative_results):
        self.results.append(result)


# noinspection PyUnusedLocal
def download_progress_mock(blocknum, blocksize, totalsize):
    pass


class ResultChecker(AggregatorListener):
    def __init__(self, callback):
        super(ResultChecker, self).__init__()
        self.callback = callback

    def aggregated_second(self, data):
        self.callback(data)


class SocketEmul(object):
    # noinspection PyUnusedLocal
    def __init__(self, family=AF_INET, atype=SOCK_STREAM, proto=0, _sock=None):
        self.recv_data = b("")
        self.sent_data = b("")

    def connect(self, address):
        ROOT_LOGGER.debug("Emulated connect to %s", address)

    # noinspection PyUnusedLocal
    def recv(self, buf_size, flags=None):
        data = self.recv_data[:buf_size]
        self.recv_data = self.recv_data[buf_size + 1:]
        ROOT_LOGGER.debug("Emulated recv: %s", data)
        return data

    # noinspection PyUnusedLocal
    def send(self, data, flags=None):
        self.sent_data += data
        ROOT_LOGGER.debug("Emulated send: %s", data)

    def setblocking(self, state):
        ROOT_LOGGER.debug("Emulate setblocking=%s", state)

    def fileno(self):
        return 0

    def close(self):
        ROOT_LOGGER.debug("Emulated close")


class BZMock(object):
    def __init__(self, obj=None):
        """
        :type obj: bzt.bza.BZAObject
        """
        super(BZMock, self).__init__()
        locs = [{'id': 'aws', 'sandbox': False, 'title': 'AWS'},
                {'id': 'us-east-1', 'sandbox': False, 'title': 'East'},
                {'id': 'us-west', 'sandbox': False, 'title': 'Dallas (Rackspace)'},
                {'id': 'harbor-sandbox', 'sandbox': True, 'title': 'Sandbox'},
                {'id': 'non-harbor-sandbox', 'sandbox': True, 'title': 'Sandbox Neverexisting'}, ]
        self.mock_get = {
            'https://a.blazemeter.com/api/v4/web/version': {},
            'https://a.blazemeter.com/api/v4/user': {'id': 1,
                                                     'defaultProject': {'id': 1, 'accountId': 1, 'workspaceId': 1}},
            'https://a.blazemeter.com/api/v4/accounts': {"result": [{'id': 1, 'owner': {'id': 1}}]},
            'https://a.blazemeter.com/api/v4/workspaces?accountId=1&enabled=true&limit=100&skip=0': {
                "result": [{'id': 1, 'enabled': True}]},
            'https://a.blazemeter.com/api/v4/workspaces?accountId=2&enabled=true&limit=100&skip=0': {
                "result": [{'id': 2, 'enabled': True}]},
            'https://a.blazemeter.com/api/v4/workspaces?accountId=3&enabled=true&limit=100&skip=0': {
                "result": [{'id': 3, 'enabled': True}]},
            'https://a.blazemeter.com/api/v4/tests?workspaceId=1&name=Taurus+Cloud+Test': {"result": []},
            'https://a.blazemeter.com/api/v4/projects/1': {"result": None},
            'https://a.blazemeter.com/api/v4/projects/2': {"result": None},
            'https://a.blazemeter.com/api/v4/projects/3': {"result": None},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1&limit=100&skip=0': {"result": []},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=2&limit=100&skip=0': {"result": []},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=3&limit=100&skip=0': {"result": []},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1&name=myproject&limit=100&skip=0': {"result": []},
            'https://a.blazemeter.com/api/v4/projects?workspaceId=1&name=Proj+name&limit=100&skip=0': {"result": []},
            'https://a.blazemeter.com/api/v4/tests/1/files': {"result": []},
            'https://a.blazemeter.com/api/v4/workspaces/1': {"result": {"id": 1, "locations": locs, "enabled": True}},
        }

        self.mock_post = {}
        self.mock_patch = {}
        self.requests = []

        if obj is not None:
            self.apply(obj)

    def apply(self, obj):
        obj.http_session = self
        obj.http_request = self._request_mock

    @call_with_retry
    def _request_mock(self, method, url, **kwargs):
        """
        :param method:
        :param url:
        :param kwargs:
        :rtype: requests.Response
        """
        # TODO: make it simplier, mocking and replacing requests.request of BZAObject
        if method == 'GET':
            resp = self.mock_get[url]
        elif method == 'POST':
            resp = self.mock_post[url]
        elif method == 'PATCH':
            resp = self.mock_patch[url]
        else:
            raise ValueError()

        response = requests.Response()

        if isinstance(resp, list):
            resp = resp.pop(0)

        data = kwargs['data']
        ROOT_LOGGER.debug("Emulated %s %s %s: %s", method, url, ("%s" % data)[:4096], resp)
        self.requests.append({"method": method, "url": url, "data": data})
        if isinstance(resp, BaseException):
            raise resp
        response._content = to_json(resp)
        response.status_code = 200
        return response


class SingletoneServiceMock(ModuleMock, Singletone, Service):
    pass


class DummyListener(TransactionListener):
    def __init__(self):
        self.transactions = Counter()

    def transaction_started(self, sender, label, start_time):
        self.transactions[label] += 1

    def transaction_ended(self, sender, label, end_time):
        self.transactions[label] += 1


class MockEngineNamespace(socketio.Namespace):
    """
    Listens to socket.io events for engine namespace.
    """
    NAMESPACE = "/v1/engine"
    METRICS_EVENT = 'metrics'

    def __init__(self):
        super().__init__(MockEngineNamespace.NAMESPACE)
        self.connect_event = threading.Event()
        self.metrics_event = threading.Event()
        self.accept_connect = True
        self.metrics_response = {}
        self.received_metrics = None

    def on_connect(self, sid, environ, auth):
        try:
            if self.accept_connect:
                return True
            raise ConnectionRefusedError("authentication failed")
        finally:
            self.connect_event.set()

    def on_disconnect(self, sid):
        pass

    def on_metrics(self, sid, data):
        try:
            self.received_metrics = data
            return self.metrics_response
        finally:
            self.metrics_event.set()


class MockHappysocksServer:

    def __init__(self) -> None:
        super().__init__()
        # create a Socket.IO server
        self._sio = socketio.Server(async_mode='gevent')
        self._engine_namespace = MockEngineNamespace()
        self._sio.register_namespace(self._engine_namespace)
        # wrap with a WSGI application
        self._app = socketio.WSGIApp(self._sio, socketio_path="api-ws")
        self._server = None
        self._thread = threading.Thread(target=self._run, name="mock-happysocks-server", daemon=True)
        self._started_event = threading.Event()

    def start(self):
        if self._started_event.isSet():
            return
        self._thread.start()
        self._started_event.wait()
        # wait until bind port is known
        while self.server_port == 0:
            time.sleep(0.05)

    def _run(self):
        # WSGIServer must be created in thread
        self._server = pywsgi.WSGIServer("localhost:0", self._app, handler_class=WebSocketHandler)
        self._started_event.set()
        self._server.serve_forever()

    @property
    def engine_namespace(self) -> MockEngineNamespace:
        return self._engine_namespace

    @property
    def server_port(self):
        return self._server.server_port

    @property
    def server_host(self):
        return self._server.server_host

    def stop(self):
        self._server.stop()
        self._thread.join()
        self._started_event.clear()
