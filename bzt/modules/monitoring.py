""" Monitoring service subsystem """
# sidebar widget
# write to file?
from abc import abstractmethod

from bzt.engine import EngineModule


class Monitoring(EngineModule):
    def __init__(self):
        super(Monitoring, self).__init__()
        self.listeners = []

    def add_listener(self, listener):
        assert isinstance(listener, MonitoringListener)
        self.listeners.append(listener)

    def prepare(self):
        # check for connectivity
        # build config str
        # address/port
        pass

    def startup(self):
        # start
        super(Monitoring, self).startup()

    def check(self):
        # get mon data
        # notify listeners
        return super(Monitoring, self).check()

    def shutdown(self):
        # close conn
        super(Monitoring, self).shutdown()

    def post_process(self):
        # shutdown agent?
        super(Monitoring, self).post_process()


class MonitoringListener(object):
    @abstractmethod
    def monitoring_data(self, data):
        pass
