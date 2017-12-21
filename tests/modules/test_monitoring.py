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

        crit_conf = BetterDict()
        crit_conf.merge({"threshold": 5, "subject": "127.0.0.1:4444/cpu"})
        criteria = MonitoringCriteria(crit_conf, obj)
        obj.add_listener(criteria)

        obj.client_classes = {'server-agent': ServerAgentClientEmul}

        obj.prepare()
        obj.startup()

        for _ in range(1, 10):
            obj.clients[0].socket.recv_data += b("%s\t%s\n" % (random.random(), random.random()))
            obj.check()
            logging.debug("Criteria state: %s", criteria)
            time.sleep(1)

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

        obj.clients[0].check_time -= obj.clients[0].interval * 2
        obj.check()

        obj.clients[0].check_time -= obj.clients[0].interval * 2
        obj.clients[0].prepared_data = "wrong data"
        obj.check()

        obj.shutdown()
        obj.post_process()

    def test_local_with_engine(self):
        config = {'metrics': ['cpu', 'engine-loop']}
        obj = LocalClient(logging.getLogger(''), 'label', config)
        obj.engine = EngineEmul()
        obj.connect()
        data = obj.get_data()
        self.assertTrue(all('source' in item.keys() and 'ts' in item.keys() for item in data))
        return data

    def test_local_without_engine(self):
        config = {'metrics': ['cpu']}
        obj = LocalClient(logging.getLogger(''), 'label', config)
        obj.connect()
        data = obj.get_data()
        self.assertTrue(all('source' in item.keys() and 'ts' in item.keys() for item in data))
        return data

    def test_multiple_local_monitorings_cpu(self):
        # psutil.cpu_percent() has interesting semantics.
        # It will often return 0.0 , 50.0 or 100.0 if called too frequently,
        # which turns out to be the case when multiple LocalClient objects are used.
        config = {'metrics': ['cpu']}
        client1 = LocalClient(logging.getLogger(''), 'label', config)
        client2 = LocalClient(logging.getLogger(''), 'label', config)

        client1.connect()
        client2.connect()

        self.assertIsInstance(client1.monitor, LocalMonitor)
        self.assertIsInstance(client2.monitor, LocalMonitor)
        self.assertIs(client1.monitor, client2.monitor)

        data1 = client1.get_data()
        data2 = client2.get_data()
        for item1, item2 in zip(data1, data2):
            self.assertNotEqual(item1['cpu'], 0)
            self.assertNotEqual(item2['cpu'], 0)
            self.assertEqual(item1['cpu'], item2['cpu'])

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
        client = LocalClient(logging.getLogger(''), 'label', {'metrics': ['cpu', 'mem', 'disks', 'conn-all']})
        client.engine = EngineEmul()
        client.connect()

        try:
            import psutil
            net_io_counters = psutil.net_io_counters
            disk_io_counters = psutil.disk_io_counters
            psutil.net_io_counters = lambda: None
            psutil.disk_io_counters = lambda: None

            client.monitor.get_resource_stats()  # should throw no exception
        finally:
            psutil.net_io_counters = net_io_counters
            psutil.disk_io_counters = disk_io_counters


class LoggingMonListener(MonitoringListener):
    def monitoring_data(self, data):
        logging.debug("Data: %s", data)


class ServerAgentClientEmul(ServerAgentClient):
    def __init__(self, parent_logger, label, config):
        super(ServerAgentClientEmul, self).__init__(parent_logger, label, config)
        self.socket = SocketEmul()
        self.socket.recv_data = b("Yep\n")
        self.select = self.select_emul

    def select_emul(self, reads, writes, excepts, timeout):
        return reads, writes, []


class GraphiteClientEmul(GraphiteClient):
    prepared_data = [{'target': 'usability', 'datapoints': [[1, 1], [2, 2]]}]

    def _data_transfer(self):
        return self.prepared_data
