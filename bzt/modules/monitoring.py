""" Monitoring service subsystem """
import json
import select
import socket
import time
import traceback
import subprocess
from abc import abstractmethod
from collections import OrderedDict, namedtuple

import psutil
from bzt import TaurusNetworkError, TaurusInternalException, TaurusConfigError
from urwid import Pile, Text

from bzt.engine import Service, Singletone
from bzt.modules.console import WidgetProvider, PrioritizedWidget
from bzt.modules.passfail import FailCriterion
from bzt.six import iteritems, urlopen, urlencode, b, stream_decode
from bzt.utils import dehumanize_time


class Monitoring(Service, WidgetProvider, Singletone):
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
            'local': LocalClient,
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
                    self.log.warning('Monitoring: obsolete configuration detected: %s', config)
                if 'label' in config:
                    label = config['label']
                else:
                    label = None
                client = client_class(self.engine, self.log, label, config)
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

    def get_widget(self):
        widget = MonitoringWidget()
        self.add_listener(widget)
        return widget


class MonitoringListener(object):
    @abstractmethod
    def monitoring_data(self, data):
        pass


class MonitoringClient(object):
    def connect(self):
        pass

    @abstractmethod
    def get_data(self):
        pass

    def disconnect(self):
        pass

    def start(self):
        pass


class LocalClient(MonitoringClient):
    """
    :type monitor: LocalMonitor
    """
    AVAILABLE_METRICS = ['cpu', 'mem', 'disk-space', 'engine-loop', 'bytes-recv',
                         'bytes-sent', 'disk-read', 'disk-write', 'conn-all']

    def __init__(self, engine, parent_logger, label, config):
        super(LocalClient, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.engine = engine
        self.config = config

        if label:
            self.label = label
        else:
            self.label = 'local'
        self.monitor = None

        exc = TaurusConfigError('Metric is required in Local monitoring client')
        metric_names = self.config.get('metrics', exc)

        bad_list = set(metric_names) - set(self.AVAILABLE_METRICS)
        if bad_list:
            self.log.warning('Wrong metrics found: %s', bad_list)

        good_list = set(metric_names) & set(self.AVAILABLE_METRICS)
        if not good_list:
            raise exc
        self.metrics = good_list

        self.interval = self.config.get('interval', None)
        if not self.interval:
            self.interval = self.engine.check_interval

        self.last_check = None
        self.cached_data = None

    def connect(self):
        self.monitor = LocalMonitor.get_instance(self.log, self.engine)

    def get_data(self):

        now = time.time()

        if not self.cached_data or now - self.last_check > self.interval:
            self.last_check = now
            self.cached_data = []
            metric_values = self.monitor.get_resource_stats(self.metrics)

            for name in self.metrics:
                self.cached_data.append({
                    'source': self.label,
                    'ts': now,
                    name: metric_values[name]})

        return self.cached_data


class LocalMonitor(object):
    __instance = None

    def __init__(self, parent_logger, engine):
        self.__informed_on_mem_issue = False
        if self.__instance is not None:
            raise TaurusInternalException("LocalMonitor can't be instantiated twice, use get_instance()")
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.engine = engine
        self.__disk_counters = None
        self.__net_counters = None
        self.__counters_ts = None
        self.__cached_stats = None

    @classmethod
    def get_instance(cls, parent_logger, engine):
        if cls.__instance is None:
            cls.__instance = LocalMonitor(parent_logger, engine)
        return cls.__instance

    def get_resource_stats(self, metrics):
        if not self.__counters_ts:
            self.__disk_counters = self.__get_disk_counters()
            self.__net_counters = psutil.net_io_counters()
            self.__counters_ts = time.time()
            time.sleep(0.2)  # small enough for human, big enough for machine

        now = time.time()
        interval = now - self.__counters_ts

        # don't recalculate stats too frequently
        if interval >= self.engine.check_interval or self.__cached_stats is None:
            self.__cached_stats = self._calc_resource_stats(interval, metrics)
            self.__counters_ts = now

        return self.__cached_stats

    def _calc_resource_stats(self, interval, metrics):
        """
        Get local resource stats

        :return: dict
        """
        result = {}

        if 'mem' in metrics:
            try:
                result['mem'] = psutil.virtual_memory().percent
            except KeyError:
                result['mem'] = None
                if not self.__informed_on_mem_issue:
                    self.log.debug("Failed to get memory usage: %s", traceback.format_exc())
                    self.log.warning("Failed to get memory usage, use -v to get more detailed error info")
                    self.__informed_on_mem_issue = True

        if 'disk-space' in metrics:
            if self.engine:
                result['disk-space'] = psutil.disk_usage(self.engine.artifacts_dir).percent
            else:
                result['disk-space'] = None

        if 'engine-loop' in metrics:
            if self.engine:
                result['engine-loop'] = self.engine.engine_loop_utilization
            else:
                result['engine-loop'] = None

        if 'conn-all' in metrics:
            # take all connections without address resolution
            output = subprocess.check_output(['netstat', '-an'])
            output_lines = stream_decode(output).split('\n')    # in py3 stream has 'bytes' type
            est_lines = [line for line in output_lines if line.find('EST') != -1]
            metrics['conn-all'] = len(est_lines)

        if 'cpu' in metrics:
            result['cpu'] = psutil.cpu_percent()

        if 'bytes-recv' in metrics or 'bytes-sent' in metrics:
            net = psutil.net_io_counters()
            if net is not None:
                tx_bytes = int((net.bytes_sent - self.__net_counters.bytes_sent) / interval)
                rx_bytes = int((net.bytes_recv - self.__net_counters.bytes_recv) / interval)
                self.__net_counters = net
            else:
                rx_bytes = 0.0
                tx_bytes = 0.0

            if 'bytes-recv' in metrics:
                result['bytes-recv'] = rx_bytes
            if 'bytes-sent' in metrics:
                result['bytes-sent'] = tx_bytes

        if 'disk-read' in metrics or 'disk-write' in metrics:
            disk = self.__get_disk_counters()
            if disk is not None:
                dru = int((disk.read_bytes - self.__disk_counters.read_bytes) / interval)
                dwu = int((disk.write_bytes - self.__disk_counters.write_bytes) / interval)
                self.__disk_counters = disk
            else:
                dru = 0.0
                dwu = 0.0

            if 'disk-read' in metrics:
                result['disk-read'] = dru
            if 'disk-write' in metrics:
                result['disk-write'] = dwu

        return result

    def __get_disk_counters(self):
        counters = None
        try:
            counters = psutil.disk_io_counters()
        except RuntimeError as exc:
            self.log.debug("Failed to get disk metrics: %s", exc)
        if counters is None:
            counters = psutil._common.sdiskio(0, 0, 0, 0, 0, 0)  # pylint: disable=protected-access
            # noinspection PyProtectedMember
        return counters


class GraphiteClient(MonitoringClient):
    def __init__(self, engine, parent_logger, label, config):
        super(GraphiteClient, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.engine = engine
        self.config = config
        exc = TaurusConfigError('Graphite client requires address parameter')
        self.address = self.config.get("address", exc)
        self.interval = int(dehumanize_time(self.config.get('interval', '5s')))
        self.url = self._get_url()
        if label:
            self.host_label = label
        else:
            self.host_label = self.address
        self.start_time = None
        self.check_time = None
        self.timeout = int(dehumanize_time(self.config.get('timeout', '5s')))

    def _get_url(self):
        exc = TaurusConfigError('Graphite client requires metrics list')
        params = [('target', field) for field in self.config.get('metrics', exc)]
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
        return url

    def _data_transfer(self):
        str_data = urlopen(self.url, timeout=self.timeout)
        return json.load(str_data)

    def _get_response(self):
        json_list = self._data_transfer()
        assert all('target' in dic.keys() for dic in json_list), "Key 'target' not found in graphite response"
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
        except BaseException:
            self.log.debug("Metrics receiving: %s", traceback.format_exc())
            self.log.warning("Fail to receive metrics from %s", self.address)
            return []

        res = []
        for element in json_list:
            item = {
                'ts': int(time.time()),
                'source': '%s' % self.host_label}

            for datapoint in reversed(element['datapoints']):
                if datapoint[0] is not None:
                    item[element['target']] = datapoint[0]
                    break

            res.append(item)
        return res


class ServerAgentClient(MonitoringClient):
    def __init__(self, engine, parent_logger, label, config):
        """
        :type parent_logger: logging.Logger
        :type config: dict
        """
        super(ServerAgentClient, self).__init__()
        self.engine = engine
        self.host_label = label
        exc = TaurusConfigError('ServerAgent client requires address parameter')
        self.address = config.get("address", exc)
        if ':' in self.address:
            self.port = int(self.address[self.address.index(":") + 1:])
            self.address = self.address[:self.address.index(":")]
        else:
            self.port = 4444

        self._partial_buffer = ""
        self.log = parent_logger.getChild(self.__class__.__name__)
        exc = TaurusConfigError('ServerAgent client requires metrics list')
        metrics = config.get('metrics', exc)
        self._result_fields = [x for x in metrics]  # TODO: handle more complex metric specifications and labeling
        self._metrics_command = "\t".join([x for x in metrics])
        self.socket = socket.socket()
        self.select = select.select
        self.interval = int(dehumanize_time(config.get("interval", 1)))

    def connect(self):
        try:
            self.socket.connect((self.address, self.port))
            self.socket.send(b("test\n"))
            resp = self.socket.recv(4)
            assert resp == b("Yep\n")
            self.log.debug("Connected to serverAgent at %s:%s successfully", self.address, self.port)
        except BaseException as exc:
            self.log.warning("Error during connecting to agent at %s:%s: %s", self.address, self.port, exc)
            msg = "Failed to connect to serverAgent at %s:%s" % (self.address, self.port)
            raise TaurusNetworkError(msg)

    def disconnect(self):
        self.log.debug("Closing connection with agent at %s:%s...", self.address, self.port)
        try:
            self.socket.send(b("exit\n"))
        except BaseException as exc:
            self.log.warning("Error during disconnecting from agent at %s:%s: %s", self.address, self.port, exc)
        finally:
            self.socket.close()

    def start(self):
        self.socket.send(b("interval:%s\n" % self.interval if self.interval > 0 else 1))
        command = "metrics:%s\n" % self._metrics_command
        self.log.debug("Sending metrics command: %s", command)
        self.socket.send(b(command))
        self.socket.setblocking(False)

    def get_data(self):
        """
        :rtype: list[dict]
        """
        readable, writable, errored = self.select([self.socket], [self.socket], [self.socket], 0)
        self.log.debug("Stream states: %s / %s / %s", readable, writable, errored)
        for _ in errored:
            self.log.warning("Failed to get monitoring data from agent at %s:%s", self.address, self.port)

        source = self.host_label if self.host_label else '%s:%s' % (self.address, self.port)

        res = []
        for _sock in readable:
            self._partial_buffer += _sock.recv(1024).decode()
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


class MonitoringWidget(Pile, MonitoringListener, PrioritizedWidget):
    def __init__(self):
        self.host_metrics = OrderedDict()
        self.display = Text("")
        super(MonitoringWidget, self).__init__([self.display])
        PrioritizedWidget.__init__(self, priority=20)

    def monitoring_data(self, data):
        for item in data:
            if item['source'] not in self.host_metrics:
                self.host_metrics[item['source']] = OrderedDict()

            for key in item:
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

            if metrics:
                maxwidth = max([len(key) for key in metrics])

                for metric, value in iteritems(metrics):
                    if value[0] is None:
                        rendered = 'N/A'
                    elif isinstance(value[0], float):
                        rendered = "{:.3f}".format(value[0])
                    elif isinstance(value[0], int):
                        rendered = "{:,}".format(value[0])
                    else:
                        rendered = value[0]
                    values = (' ' * (maxwidth - len(metric)), metric, rendered)
                    text.append((value[1], "  %s%s: %s\n" % values))

        self.display.set_text(text)
        self._invalidate()


class MonitoringCriteria(MonitoringListener, FailCriterion):
    def __init__(self, config, owner):
        """
        :type config: bzt.utils.BetterDict
        :type owner: bzt.engine.EngineModule
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
            raise TaurusConfigError("Wrong syntax for monitoring criteria subject: %s" % subject)
        host = subject[:subject.index('/')]
        metric = subject[subject.index('/') + 1:]
        return lambda x: (x[metric] if x['source'] == host and metric in x else None)
