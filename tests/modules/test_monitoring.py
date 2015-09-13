import logging

from bzt.modules.monitoring import Monitoring, MonitoringListener
from tests import BZTestCase


class TestMonitoring(BZTestCase):
    def test_simple(self):
        obj = Monitoring()
        obj.parameters.merge({
            "127.0.0.1:4444": {
                
            }
        })

        obj.prepare()
        listener = LoggingMonListener()
        obj.add_listener(listener)
        obj.startup()
        obj.check()
        obj.shutdown()
        obj.post_process()


class LoggingMonListener(MonitoringListener):
    def monitoring_data(self, data):
        logging.debug("Data: %s", data)
