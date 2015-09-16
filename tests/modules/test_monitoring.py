import logging
import random
import time

from bzt.modules.monitoring import Monitoring, MonitoringListener, MonitoringCriteria, ServerAgentClient
from bzt.utils import BetterDict
from tests import BZTestCase
from tests.mocks import EngineEmul, SocketEmul


class TestMonitoring(BZTestCase):
    def test_simple(self):
        obj = Monitoring()
        obj.server_agent_class = ServerAgentClientEmul
        obj.engine = EngineEmul()
        obj.parameters.merge({
            "server-agents": {
                "127.0.0.1:4444": {
                    "metrics": [
                        "cpu",
                        "disks"
                    ]
                }
            }
        })

        listener = LoggingMonListener()
        obj.add_listener(listener)

        widget = obj.get_widget()
        obj.add_listener(widget)

        crit_conf = BetterDict()
        crit_conf.merge({"threshold": 5, "subject": "127.0.0.1:4444/cpu"})
        criteria = MonitoringCriteria(crit_conf, obj)
        obj.add_listener(criteria)

        obj.prepare()

        obj.startup()

        for _ in range(1, 10):
            obj.clients[0].socket.recv_data += "%s\t%s\n" % (random.random(), random.random())
            obj.check()
            logging.debug("Criteria state: %s", criteria)
            time.sleep(1)

        obj.shutdown()
        obj.post_process()

        self.assertEquals("test\ninterval:1\nmetrics:cpu\tdisks\nexit\n", obj.clients[0].socket.sent_data)


class LoggingMonListener(MonitoringListener):
    def monitoring_data(self, data):
        logging.debug("Data: %s", data)


class ServerAgentClientEmul(ServerAgentClient):
    def __init__(self, parent_logger, label, config):
        super(ServerAgentClientEmul, self).__init__(parent_logger, label, config)
        self.socket = SocketEmul()
        self.socket.recv_data = "Yep\n"
        self.select = self.select_emul

    def select_emul(self, reads, writes, excepts, timeout):
        return reads, writes, []
