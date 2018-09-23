import logging
import random
import time
import unittest

from bzt.modules.monitoring import Monitoring, MonitoringListener, MonitoringCriteria
from bzt.modules.monitoring import ServerAgentClient, GraphiteClient, LocalClient, LocalMonitor
from bzt.six import PY3, b
from bzt.utils import BetterDict
from tests import BZTestCase
from tests.mocks import EngineEmul, SocketEmul


class TestMonitoring(BZTestCase):
    def setUp(self):
        super(TestMonitoring, self).setUp()
        LocalMonitor._instance = None

    def test_server_agent(self):
        obj = Monitoring()
        obj.engine = EngineEmul()
        obj.parameters.merge({
            "server-agent": [{
                "address": "127.0.0.1:4444",
                "metrics": [
                    "cpu",
                    "disks"
                ]
            }, {
                "address": "10.0.0.1",
                "metrics": [
                    "something1",
                    "something2"
                ]
            }]
        })

        listener = LoggingMonListener()
        obj.add_listener(listener)

        widget = obj.get_widget()
        obj.add_listener(widget)

        crit_conf = BetterDict.from_dict({"threshold": 5, "subject": "127.0.0.1:4444/cpu"})
        criteria = MonitoringCriteria(crit_conf, obj)
        obj.add_listener(criteria)

        obj.client_classes = {'server-agent': ServerAgentClientEmul}

        obj.prepare()
        obj.startup()

        for _ in range(1, 10):
            obj.clients[0].socket.recv_data += b("%s\t%s\n" % (random.random(), random.random()))
            obj.check()
            logging.debug("Criteria state: %s", criteria)
            time.sleep(obj.engine.check_interval)

        obj.shutdown()
        obj.post_process()

        self.assertEquals(b("test\ninterval:1\nmetrics:cpu\tdisks\nexit\n"), obj.clients[0].socket.sent_data)

    def test_graphite(self):
        obj = Monitoring()
        obj.engine = EngineEmul()
        obj.parameters.merge({
            "graphite": [{
                "address": "people.com:1066",
                "label": "Earth",
                "metrics": [
                    "body",
                    "brain"]}, {
                "address": "http://spirits.net",
                "metrics": [
                    "transparency",
                    "usability"
                ]}]
        })
        obj.client_classes = {'graphite': GraphiteClientEmul}
        obj.prepare()
        obj.startup()
        obj.check()

        obj.clients[0]._last_check -= obj.clients[0].interval * 2
        obj.check()

        obj.clients[0]._last_check -= obj.clients[0].interval * 2
        obj.clients[0].prepared_data = "wrong data"
        obj.check()

        obj.shutdown()
        obj.post_process()

    def test_local_with_engine(self):
        config = {'interval': '5m', 'metrics': ['cpu', 'engine-loop']}
        obj = LocalClient(logging.getLogger(''), 'label', config, EngineEmul())
        obj.connect()
        data = obj.get_data()
        self.assertTrue(300, obj.interval)
        self.assertTrue(all('source' in item and 'ts' in item for item in data))

    def test_deprecated_local(self):
        # strange metrics, anyway full list of available ones will be used
        config = {'metrics': ['memory', 'disk']}

        obj = LocalClient(logging.getLogger(''), 'label', config)
        obj.engine = EngineEmul()
        obj.connect()
        data = obj.engine_resource_stats()
        self.assertTrue(all(val is not None for val in [
            data.cpu, data.disk_usage, data.mem_usage, data.mem_usage, data.rx,
            data.tx, data.dru, data.dwu, data.engine_loop, data.conn_all]))

    def test_all_metrics(self):
        config = {'interval': '1m', 'metrics': LocalClient.AVAILABLE_METRICS}
        obj = LocalClient(logging.getLogger(''), 'label', config, EngineEmul())
        obj.connect()
        self.assertEqual(60, obj.interval)
        data = obj.get_data()
        for item in data:
            metrics = set(item.keys()) - {'ts', 'source'}
            self.assertEqual(1, len(metrics))
            self.assertIn(metrics.pop(), LocalClient.AVAILABLE_METRICS)
        self.assertEqual(len(data), len(LocalClient.AVAILABLE_METRICS))

    def test_local_without_engine(self):
        config = {'metrics': ['cpu']}
        obj = LocalClient(logging.getLogger(''), 'label', config, EngineEmul())
        obj.connect()
        data = obj.get_data()
        self.assertEqual(obj.interval, obj.engine.check_interval)
        self.assertTrue(all('source' in item.keys() and 'ts' in item.keys() for item in data))

    def test_multiple_local_monitorings(self):
        config1 = {'metrics': ['mem', 'engine-loop']}
        config2 = {'metrics': ['cpu', 'mem']}

        obj = Monitoring()
        obj.engine = EngineEmul()
        obj.parameters.merge({
            'local': [config1, config2]})

        obj.prepare()
        self.assertEqual(1, len(obj.clients))
        self.assertEqual({'mem', 'cpu', 'engine-loop'}, set(obj.clients[0].metrics))
        self.assertTrue(isinstance(obj.clients[0].monitor, LocalMonitor))

        data1 = obj.clients[0].get_data()
        data2 = obj.clients[0].get_data()
        for item1, item2 in zip(data1, data2):
            self.assertEqual(item1, item2)

        metrics = []
        for element in data1:
            self.assertIn('source', element)
            self.assertIn('ts', element)
            for key in element:
                if key not in ('source', 'ts'):
                    metrics.append(key)

        for config in (config1, config2):
            self.assertTrue(all(m in metrics for m in config['metrics']))

    @unittest.skipUnless(PY3, "py3 only")
    def test_server_agent_encoding(self):
        obj = Monitoring()
        obj.engine = EngineEmul()
        obj.parameters.merge({
            "server-agent": [{
                "address": "127.0.0.1:4444",
                "metrics": [
                    "cpu",
                    "disks"
                ]
            }]
        })

        obj.client_classes = {'server-agent': ServerAgentClientEmul}
        obj.prepare()

        self.assertEquals(b("test\n"), obj.clients[0].socket.sent_data)

    def test_psutil_potential_bugs(self):
        conf = {'metrics': ['cpu', 'mem', 'disks', 'conn-all']}
        client = LocalClient(logging.getLogger(''), 'label', conf, EngineEmul())
        client.connect()

        import psutil

        try:
            net_io_counters = psutil.net_io_counters
            disk_io_counters = psutil.disk_io_counters
            psutil.net_io_counters = lambda: None
            psutil.disk_io_counters = lambda: None

            client.monitor.resource_stats()  # should throw no exception
        finally:
            psutil.net_io_counters = net_io_counters
            psutil.disk_io_counters = disk_io_counters


class LoggingMonListener(MonitoringListener):
    def monitoring_data(self, data):
        logging.debug("Data: %s", data)


class ServerAgentClientEmul(ServerAgentClient):
    def __init__(self, parent_logger, label, config, engine):
        super(ServerAgentClientEmul, self).__init__(parent_logger, label, config, engine)
        self.socket = SocketEmul()
        self.socket.recv_data = b("Yep\n")
        self.select = self.select_emul

    def select_emul(self, reads, writes, excepts, timeout):
        return reads, writes, []


class GraphiteClientEmul(GraphiteClient):
    prepared_data = [{'target': 'usability', 'datapoints': [[1, 1], [2, 2]]}]

    def _data_transfer(self):
        return self.prepared_data
