""" Monitoring service subsystem """
import json
import select
import socket
import time
from abc import abstractmethod
from collections import OrderedDict

from urwid import Pile, Text

from bzt.engine import EngineModule, Service
from bzt.modules.console import WidgetProvider
from bzt.modules.passfail import FailCriteria
from bzt.six import iteritems, urlopen, urlencode
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
        self.client_classes = {
            'server-agent': ServerAgentClient,
            'graphite': GraphiteClient,
        }

    def add_listener(self, listener):
        assert isinstance(listener, MonitoringListener)
        self.listeners.append(listener)

    def prepare(self):
        for client_name in self.parameters:

            if client_name in self.client_classes:
                client_class = self.client_classes[client_name]
            else:
                if client_name == 'server-agents':
                    self.log.warning('Monitoring: obsolete config file format detected.')
                continue

            for config in self.parameters.get(client_name):
                if isinstance(config, str):
                    self.log.warning('Monitoring: obsolete config file format detected.')
                client = client_class(self.log, client_name, config)
                self.clients.append(client)
                client.connect()

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


class MonitoringClient(object):
    @abstractmethod
    def connect(self):
        pass

    @abstractmethod
    def start(self):
        pass

    @abstractmethod
    def get_data(self):
        pass

    @abstractmethod
    def disconnect(self):
        pass


class GraphiteClient(MonitoringClient):
    def __init__(self, parent_logger, label, config):
        super(GraphiteClient, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.config = config
        self.address = self.config.get('address')
        self.interval = int(dehumanize_time(self.config.get('interval', '5s')))
        params = [('target', field) for field in self.config.get('metrics', ValueError("Metrics list required"))]
        from_t = int(dehumanize_time(self.config.get('from', self.interval * 1000)))
        until_t = int(dehumanize_time(self.config.get('until', 0)))
        params += [
            ('from', '-%ss' % from_t),
            ('until', '-%ss' % until_t),
            ('format', 'json')
        ]

        url = self.address + '/render?' + urlencode(params)
        if not url.startswith('http'):
            url = 'http://' + url
        self.url = url
        if label:
            self.host_label = label
        else:
            self.host_label = self.address
        self.start_time = None
        self.check_time = None
        self.timeout = int(dehumanize_time(self.config.get('timeout', '5s')))

        # TODO: handle more complex metric specifications and labeling
        # TODO: smart profiling for parameter back-time (if it set up to 'auto')
        # variants: interval*X, series of requests, ???

    def _get_response(self):  # TODO: add timeout
        str_data = urlopen(self.url, timeout=self.timeout)
        json_list = json.load(str_data)

        assert all('target' in dic.keys() for dic in json_list), 'Fail to receive metrics from %s' % self.address
        return json_list

    def connect(self):
        self._get_response()

    def start(self):
        self.check_time = int(time.time())
        self.start_time = self.check_time

    def get_data(self):
        current_time = int(time.time())
        if current_time < self.check_time + self.interval:
            return []
        self.check_time = current_time

        try:
            json_list = self._get_response()
        except BaseException as error:
            self.log.warning(error.message)
            return []

        res = []
        for element in json_list:
            item = {
                'ts': int(time.time()),
                'source': '%s' % self.host_label}

            # TODO for targets process wildcards, functions, etc.
            for datapoint in reversed(element['datapoints']):
                if datapoint[0] is not None:
                    item[element['target']] = datapoint[0]
                    break

            res.append(item)
        return res

    def disconnect(self):
        pass


class ServerAgentClient(MonitoringClient):
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

            if len(metrics):
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
