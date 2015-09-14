""" Monitoring service subsystem """
# sidebar widget
# write to file?
from abc import abstractmethod
import socket
import select
import time

from urwid import Pile

from bzt.engine import EngineModule
from bzt.modules.console import WidgetProvider
from bzt.six import iteritems


class Monitoring(EngineModule, WidgetProvider):
    """
    :type clients: list[ServerAgentClient]
    :type listeners: list[MonitoringListener]
    """

    def __init__(self):
        super(Monitoring, self).__init__()
        self.listeners = []
        self.clients = []

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
            client = ServerAgentClient(self.log, address, port, config)
            client.connect()
            self.clients.append(client)

    def startup(self):
        for client in self.clients:
            client.start()
        super(Monitoring, self).startup()

    def check(self):
        results = []
        for client in self.clients:
            results.extend(client.get_data())

        for listener in self.listeners:
            listener.monitoring_data(results)
        return super(Monitoring, self).check()

    def shutdown(self):
        for client in self.clients:
            client.disconnect()
        super(Monitoring, self).shutdown()

    def post_process(self):
        # shutdown agent?
        super(Monitoring, self).post_process()

    def get_widget(self):
        widget = MonitoringWidget()
        self.add_listener(widget)
        return widget


class MonitoringListener(object):
    @abstractmethod
    def monitoring_data(self, data):
        pass


class ServerAgentClient(object):
    def __init__(self, parent_logger, address, port, config):
        """
        :type parent_logger: logging.Logger
        """
        super(ServerAgentClient, self).__init__()
        self.host_label = config.get("host-label")
        self._partial_buffer = ""
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.address = address
        self.port = int(port) if port is not None else 4444
        metrics = config.get('metrics', ValueError("Metrics list required"))
        self._result_fields = [x for x in metrics]
        self._metrics_command = "\t".join([x for x in metrics])
        self.socket = socket.socket()

    def connect(self):
        try:
            self.socket.connect((self.address, self.port))
            self.socket.send("test\n")
            resp = self.socket.recv(4)
            assert resp == "Yep\n"
            self.log.debug("Connected to serverAgent at %s:%s successfully", self.address, self.port)
        except:
            self.log.error("Failed to connect to serverAgent at %s:%s" % (self.address, self.port))
            raise

    def disconnect(self):
        self.log.debug("Closing connection with %s:%s", self.address, self.port)
        try:
            self.socket.send("exit\n")
        finally:
            self.socket.close()

    def start(self):
        command = "metrics:%s\n" % self._metrics_command
        self.log.debug("Sending metrics command: %s", command)
        self.socket.send(command)
        self.socket.setblocking(False)

    def get_data(self):
        """
        :rtype: list[dict]
        """
        readable, writable, errored = select.select([self.socket], [self.socket], [self.socket], 0)
        self.log.debug("Stream states: %s / %s / %s", readable, writable, errored)
        for _ in errored:
            self.log.warning("Failed to get monitoring data from %s:%s", self.address, self.port)

        source = self.host_label if self.host_label else '%s:%s' % (self.address, self.port)

        res = []
        for _sock in readable:
            self._partial_buffer += _sock.recv(1024)
            while "\n" in self._partial_buffer:
                line = self._partial_buffer[:self._partial_buffer.index("\n")]
                self._partial_buffer = self._partial_buffer[self._partial_buffer.index("\n") + 1:]
                self.log.debug("Data line: %s", line)
                values = line.split("\t")
                item = {x: values.pop(0) for x in self._result_fields}
                item['ts'] = int(time.time())
                item['source'] = source
                res.append(item)

        return res


class MonitoringWidget(Pile, MonitoringListener):
    def __init__(self):
        super(MonitoringWidget, self).__init__([])

    def monitoring_data(self, data):
        pass
