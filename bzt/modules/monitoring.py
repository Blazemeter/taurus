""" Monitoring service subsystem """
# sidebar widget
# write to file?
from abc import abstractmethod
import socket

from urwid import Pile

from bzt.engine import EngineModule
from bzt.modules.console import WidgetProvider
from bzt.six import iteritems


class Monitoring(EngineModule, WidgetProvider):
    """
    :type clients: dict[str,ServerAgentClient]
    """

    def __init__(self):
        super(Monitoring, self).__init__()
        self.listeners = []
        self.clients = {}

    def add_listener(self, listener):
        assert isinstance(listener, MonitoringListener)
        self.listeners.append(listener)

    def prepare(self):
        for address, config in iteritems(self.parameters):
            if ':' in address:
                port = address[address.index(":") + 1:]
                address = address[:address.index(":")]
            else:
                port = None
            client = ServerAgentClient(self.log, address, port)
            client.connect()
            # check for connectivity
            # build config str
            # address/port
            self.clients['address'] = client

    def startup(self):
        # start
        super(Monitoring, self).startup()

    def check(self):
        # get mon data
        # notify listeners
        return super(Monitoring, self).check()

    def shutdown(self):
        for client in self.clients.values():
            client.disconnect()
        super(Monitoring, self).shutdown()

    def post_process(self):
        # shutdown agent?
        super(Monitoring, self).post_process()

    def get_widget(self):
        return MonitoringWidget([])


class MonitoringListener(object):
    @abstractmethod
    def monitoring_data(self, data):
        pass


class MonitoringWidget(Pile):
    pass


class ServerAgentClient(object):
    def __init__(self, parent_logger, address, port=None):
        """
        :type parent_logger: logging.Logger
        """
        super(ServerAgentClient, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.address = address
        self.port = int(port) if port is not None else 4444
        self.socket = socket.socket()

    def connect(self):
        self.socket.connect((self.address, self.port))
        self.socket.send("test\n")
        resp = self.socket.recv(3)
        assert resp == 'Yep'
        self.log.debug("Connected to serverAgent at %s:%s successfully", self.address, self.port)

    def disconnect(self):
        self.socket.send("exit\n")
        self.socket.close()
