""" Monitoring service subsystem """
from abc import abstractmethod
from collections import OrderedDict
import socket
import select
import time

from urwid import Pile, Text

from bzt.engine import EngineModule, Service
from bzt.modules.console import WidgetProvider
from bzt.modules.passfail import FailCriteria
from bzt.six import iteritems
from bzt.utils import dehumanize_time


class Monitoring(Service, WidgetProvider):
    """
    :type clients: list[ServerAgentClient]
    :type listeners: list[MonitoringListener]
    """

    def __init__(self):
        super(Monitoring, self).__init__()
        self.listeners = []
        self.clients = []
        self.server_agent_class = ServerAgentClient

    def add_listener(self, listener):
        assert isinstance(listener, MonitoringListener)
        self.listeners.append(listener)

    def prepare(self):
        for label, config in iteritems(self.parameters.get("server-agents")):
            client = self.server_agent_class(self.log, label, config)
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

        if results:
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
    def __init__(self, parent_logger, label, config):
        """
        :type parent_logger: logging.Logger
        :type config: dict
        """
        super(ServerAgentClient, self).__init__()
        self.host_label = label
        self.address = config.get("address", label)
        if ':' in self.address:
            self.port = int(self.address[self.address.index(":") + 1:])
            self.address = self.address[:self.address.index(":")]
        else:
            self.port = 4444

        self._partial_buffer = ""
        self.log = parent_logger.getChild(self.__class__.__name__)
        metrics = config.get('metrics', ValueError("Metrics list required"))
        self._result_fields = [x for x in metrics]  # TODO: handle more complex metric specifications and labeling
        self._metrics_command = "\t".join([x for x in metrics])
        self.socket = socket.socket()
        self.select = select.select
        self.interval = int(dehumanize_time(config.get("interval", 1)))

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
        except BaseException as exc:
            self.log.warning("Error during disconnecting from agent: %s", exc)
        finally:
            self.socket.close()

    def start(self):
        self.socket.send("interval:%s\n" % self.interval if self.interval > 0 else 1)
        command = "metrics:%s\n" % self._metrics_command
        self.log.debug("Sending metrics command: %s", command)
        self.socket.send(command)
        self.socket.setblocking(False)

    def get_data(self):
        """
        :rtype: list[dict]
        """
        readable, writable, errored = self.select([self.socket], [self.socket], [self.socket], 0)
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
                item = {x: float(values.pop(0)) for x in self._result_fields}
                item['ts'] = int(time.time())
                item['source'] = source
                res.append(item)

        return res


class MonitoringWidget(Pile, MonitoringListener):
    def __init__(self):
        self.host_metrics = OrderedDict()
        self.display = Text("")
        super(MonitoringWidget, self).__init__([self.display])

    def monitoring_data(self, data):
        for item in data:
            if item['source'] not in self.host_metrics:
                self.host_metrics[item['source']] = OrderedDict()

            for key in sorted(item.keys()):
                if key not in ("source", "ts"):
                    color = ''
                    if key in self.host_metrics[item['source']]:
                        if self.host_metrics[item['source']][key][0] < item[key]:
                            color = 'warmer'
                        elif self.host_metrics[item['source']][key][0] > item[key]:
                            color = 'colder'

                    self.host_metrics[item['source']][key] = (item[key], color)

        text = []
        for host, metrics in iteritems(self.host_metrics):
            text.append(('stat-hdr', " %s \n" % host))

            maxwidth = max([len(key) for key in metrics.keys()])

            for metric, value in iteritems(metrics):
                values = (' ' * (maxwidth - len(metric)), metric, value[0])
                text.append((value[1], "  %s%s: %.3f\n" % values))

        self.display.set_text(text)
        self._invalidate()


class MonitoringCriteria(MonitoringListener, FailCriteria):
    def __init__(self, config, owner):
        """
        :type config: bzt.utils.BetterDict
        :type owner: EngineModule
        """
        super(MonitoringCriteria, self).__init__(config, owner)
        for service in self.owner.engine.services:
            if isinstance(service, Monitoring):
                service.add_listener(self)

    def monitoring_data(self, data):
        for item in data:
            val = self.get_value(item)
            if val is not None:
                self.process_criteria_logic(item['ts'], val)

    def _get_field_functor(self, subject, percentage):
        if '/' not in subject:
            raise ValueError("Wrong syntax for monitoring criteria subject")
        host = subject[:subject.index('/')]
        metric = subject[subject.index('/') + 1:]
        return lambda x: (x[metric] if x['source'] == host and metric in x else None)
