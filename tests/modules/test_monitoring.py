import logging
import time

from bzt.modules.monitoring import Monitoring, MonitoringListener, MonitoringCriteria
from bzt.utils import BetterDict
from tests import BZTestCase
from tests.mocks import EngineEmul


class TestMonitoring(BZTestCase):
    def test_simple(self):
        obj = Monitoring()
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
            obj.check()
            logging.debug("Criteria state: %s", criteria)
            time.sleep(1)

        obj.shutdown()
        obj.post_process()


class LoggingMonListener(MonitoringListener):
    def monitoring_data(self, data):
        logging.debug("Data: %s", data)
