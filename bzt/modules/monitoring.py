""" Monitoring service subsystem """
import os
import re
from logging import Logger

import select
import socket
import subprocess
import time
import traceback
from abc import abstractmethod, ABC
from collections import OrderedDict, namedtuple
from typing import List, Optional

import csv
import psutil
from urllib.parse import urlencode
from urwid import Pile, Text

from bzt import TaurusNetworkError, TaurusInternalException, TaurusConfigError
from bzt.engine import Service, Singletone
from bzt.modules.console import PrioritizedWidget
from bzt.modules.passfail import FailCriterion
from bzt.utils import iteritems, b, stream_decode, dehumanize_time, BetterDict, is_windows


class Monitoring(Service, Singletone):
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
        super(Monitoring, self).prepare()
        clients = (param for param in self.parameters if param not in ('run-at', 'module'))
        for client_name in clients:
            if client_name in self.client_classes:
                client_class = self.client_classes[client_name]
            else:
                self.log.warning('Unknown monitoring found: %s', client_name)
                continue
            for config in self.parameters.get(client_name, []):
                label = config.get('label', None)

                if client_name == 'local':
                    if any([client for client in self.clients
                            if isinstance(client, self.client_classes[client_name])]):
                        break  # skip the second and following local monitoring clients
                    else:
                        if len(self.parameters.get(client_name, [])) > 1:
                            self.log.warning('LocalMonitoring client found twice, configs will be joined')
                        config = BetterDict()
                        for cfg in self.parameters.get(client_name, []):
                            config.merge(cfg)

                client = client_class(self.log, label, config, self.engine)
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
    def monitoring_data(self, data: List[dict]):
        pass


class MonitoringClient(object):
    def __init__(self, parent_log, engine):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.engine = engine
        self._last_check = 0  # the last check was long time ago
        self.logs_file = None

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

    def __init__(self, parent_log, label, config, engine=None):
        super(LocalClient, self).__init__(parent_log, engine)

        self.config = config

        if not engine:
            self.log.warning('Deprecated constructor detected!')
            self.config['metrics'] = self.AVAILABLE_METRICS  # be generous to old clients

        if label:
            self.label = label
        else:
            self.label = 'local'

        self.monitor = None
        self.interval = None
        self.metrics = None
        self._cached_data = None

    # emulation of deprecated interface
    def engine_resource_stats(self):
        stats = namedtuple("ResourceStats", (
            'cpu', 'disk_usage', 'mem_usage', 'rx', 'tx', 'dru', 'dwu', 'engine_loop', 'conn_all'))

        res_stats = self._get_resource_stats()

        cpu = res_stats.get('cpu', None)
        disk_usage = res_stats.get('disk-space', None)
        mem_usage = res_stats.get('mem', None)
        rx = res_stats.get('bytes-recv', None)
        tx = res_stats.get('bytes-sent', None)
        dru = res_stats.get('disk-read', None)
        dwu = res_stats.get('disk-write', None)
        engine_loop = res_stats.get('engine-loop', None)
        conn_all = res_stats.get('conn-all', None)

        return stats(cpu=cpu, disk_usage=disk_usage, mem_usage=mem_usage, rx=rx, tx=tx,
                     dru=dru, dwu=dwu, engine_loop=engine_loop, conn_all=conn_all)

    def _get_resource_stats(self):
        if not self.monitor:
            raise TaurusInternalException('Local monitor must be instantiated')
        return self.monitor.resource_stats()

    def connect(self):
        exc = TaurusConfigError('Metric is required in Local monitoring client')
        metric_names = self.config.get('metrics', exc)

        bad_list = set(metric_names) - set(self.AVAILABLE_METRICS)
        if bad_list:
            self.log.warning('Wrong metrics found: %s', bad_list)

        good_list = set(metric_names) & set(self.AVAILABLE_METRICS)
        if not good_list:
            raise exc

        self.metrics = list(set(good_list))

        self.monitor = LocalMonitorFactory.create_local_monitor(self.log, self.metrics, self.engine)
        self.log.info(f'Using {self.monitor.__class__.__name__} for engine health monitoring')
        self.interval = dehumanize_time(self.config.get("interval", self.engine.check_interval))

        if self.config.get("logging", False):
            self.logs_file = self.engine.create_artifact("local_monitoring_logs", ".csv")
            with open(self.logs_file, "a", newline='') as mon_logs:
                logs_writer = csv.writer(mon_logs, delimiter=',')
                metrics = ['ts'] + sorted([metric for metric in good_list])
                logs_writer.writerow(metrics)

    def get_data(self):
        now = time.time()

        if now > self._last_check + self.interval:
            self._last_check = now
            self._cached_data = []
            metric_values = self._get_resource_stats()

            if self.logs_file:
                with open(self.logs_file, "a", newline='') as mon_logs:
                    line = [str(round(now))] + [str(metric_values[x]) for x in sorted(metric_values.keys())]
                    logs_writer = csv.writer(mon_logs, delimiter=',')
                    logs_writer.writerow(line)

            for name in self.metrics:
                self._cached_data.append({
                    'source': self.label,
                    'ts': now,
                    name: metric_values[name]})

        return self._cached_data


class BaseLocalMonitor(ABC):
    def __init__(self, parent_logger, metrics: List[str], engine):
        if not engine:
            raise TaurusInternalException('Local monitor requires valid engine instance')
        self._informed_on_mem_issue = False
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.metrics = metrics
        self.engine = engine
        self._disk_counters = None
        self._net_counters = None
        self._last_check = None

    def resource_stats(self):
        if not self._last_check:
            self._disk_counters = self.__get_disk_counters()
            self._net_counters = self.__get_net_counters()
            self._last_check = time.time()
            time.sleep(0.2)  # small enough for human, big enough for machine

        now = time.time()
        interval = now - self._last_check
        self._last_check = now
        return self._calc_resource_stats(interval)

    def _calc_resource_stats(self, interval):
        """
        Get local resource stats

        :return: dict
        """
        result = {}

        if 'mem' in self.metrics:
            result['mem'] = self._get_mem_info()

        if 'disk-space' in self.metrics:
            result['disk-space'] = self.__get_disk_usage(self.engine.artifacts_dir).percent

        if 'engine-loop' in self.metrics:
            result['engine-loop'] = self.engine.engine_loop_utilization

        if 'conn-all' in self.metrics:
            try:
                # take all connections without address resolution
                output = subprocess.check_output(['netstat', '-an'])
                output_lines = stream_decode(output).split('\n')  # in py3 stream has 'bytes' type
                est_lines = [line for line in output_lines if line.find('EST') != -1]
                result['conn-all'] = len(est_lines)
            except BaseException as exc:
                self.log.debug("Failed to get connections info: %s", exc)
                result['conn-all'] = 0

        if 'cpu' in self.metrics:
            result['cpu'] = self._get_cpu_percent()

        if 'bytes-recv' in self.metrics or 'bytes-sent' in self.metrics:
            net = self.__get_net_counters()
            if net is not None:
                tx_bytes = int((net.bytes_sent - self._net_counters.bytes_sent) / float(interval))
                rx_bytes = int((net.bytes_recv - self._net_counters.bytes_recv) / float(interval))
                self._net_counters = net
            else:
                rx_bytes = 0.0
                tx_bytes = 0.0

            if 'bytes-recv' in self.metrics:
                result['bytes-recv'] = rx_bytes
            if 'bytes-sent' in self.metrics:
                result['bytes-sent'] = tx_bytes

        if 'disk-read' in self.metrics or 'disk-write' in self.metrics:
            disk = self.__get_disk_counters()
            if disk is not None:
                dru = int((disk.read_bytes - self._disk_counters.read_bytes) / float(interval))
                dwu = int((disk.write_bytes - self._disk_counters.write_bytes) / float(interval))
                self._disk_counters = disk
            else:
                dru = 0.0
                dwu = 0.0

            if 'disk-read' in self.metrics:
                result['disk-read'] = dru
            if 'disk-write' in self.metrics:
                result['disk-write'] = dwu

        return result

    def __get_disk_counters(self):
        counters = None
        try:
            counters = psutil.disk_io_counters()
        except (RuntimeError, PermissionError) as exc:
            self.log.debug("Failed to get disk io counters: %s", exc)
        if counters is None:
            counters = psutil._common.sdiskio(0, 0, 0, 0, 0, 0)  # pylint: disable=protected-access
            # noinspection PyProtectedMember
        return counters

    def __get_net_counters(self):
        counters = None
        try:
            counters = psutil.net_io_counters()
        except (RuntimeError, PermissionError) as exc:
            self.log.debug("Failed to get net io counters: %s", exc)
        if counters is None:
            counters = psutil._common.snetio(0, 0, 0, 0, 0, 0, 0, 0)  # pylint: disable=protected-access
            # noinspection PyProtectedMember
        return counters

    def __get_disk_usage(self, path):
        disk = None
        try:
            disk = psutil.disk_usage(path)
        except (RuntimeError, PermissionError) as exc:
            self.log.debug("Failed to get disk usage metrics: %s", exc)
        if disk is None:
            disk = psutil._common.sdiskusage(0, 0, 0, 0)  # pylint: disable=protected-access
            # noinspection PyProtectedMember
        return disk

    @abstractmethod
    def _get_mem_info(self):
        pass

    @abstractmethod
    def _get_cpu_percent(self):
        pass


class StandardLocalMonitor(BaseLocalMonitor):

    def __init__(self, parent_logger, metrics, engine):
        super().__init__(parent_logger, metrics, engine)

    def _get_mem_info(self):
        try:
            return psutil.virtual_memory().percent
        except AttributeError:
            if not self._informed_on_mem_issue:
                self.log.debug("Failed to get memory usage: %s", traceback.format_exc())
                self.log.warning("Failed to get memory usage, use -v to get more detailed error info")
                self._informed_on_mem_issue = True

    def _get_cpu_percent(self):
        cpu = None
        try:
            cpu = psutil.cpu_percent()
        except (RuntimeError, PermissionError) as exc:
            self.log.debug("Failed to get cpu percent metrics: %s", exc)
        if cpu is None:
            cpu = 0

        return cpu


class BaseCgroupsLocalMonitor(BaseLocalMonitor, ABC):
    """
    Base class for cgroups and cgroups2 local monitors.
    """

    def __init__(self, cgroup_fs_path: str, parent_logger: Logger, metrics: List[str], engine):
        super().__init__(parent_logger, metrics, engine)
        self._log = parent_logger.getChild(self.__class__.__name__)
        self._cgroup_fs_path = cgroup_fs_path
        # max cpu usage in microseconds per one second
        self._max_cpu_usage = self._get_max_cpu_usage()
        self._max_memory_usage = self._get_max_memory_usage()
        self._cpu_usage1_usec = None
        self._cpu_usage1_time = None

    def _get_max_cpu_usage(self) -> Optional[int]:
        """
        Returns maximum possible cpu usage per one second.
        """
        max_cpu_usage = self._get_cgroups_max_cpu_usage()
        if not max_cpu_usage:
            # if no cgroups limit is in place, then maximum possible cpu usage depends on the number of available cpus
            max_cpu_usage = psutil.cpu_count() * 1000000  # number of cpus * microseconds in one second
        return max_cpu_usage

    @abstractmethod
    def _get_cgroups_max_cpu_usage(self) -> Optional[int]:
        pass

    def _get_max_memory_usage(self) -> Optional[int]:
        max_memory_usage = self._get_cgroups_max_memory_usage()
        if not max_memory_usage:
            # if no cgroups limit is in place, then maximum possible memory usage is total available system memory
            try:
                max_memory_usage = psutil.virtual_memory().total
            except AttributeError:
                self._log.warning(f'Unable to determine total available memory', exc_info=True)
        return max_memory_usage

    @abstractmethod
    def _get_cgroups_max_memory_usage(self) -> Optional[int]:
        pass

    def _get_cpu_percent(self):
        """
        Returns average cpu usage in percentage between the last and this measurement. First call returns 0.
        """
        cpu_delta = None
        total_delta = None
        cpu_usage = 0
        try:
            cpu_usage2_time = time.time()
            cpu_usage2_usec = self._get_cgroups_cpu_usage_snapshot()
            if cpu_usage2_usec and self._cpu_usage1_usec:
                # elapsed cpu time our cgroup consumed in time period between measurements
                cpu_delta = cpu_usage2_usec - self._cpu_usage1_usec
            if self._cpu_usage1_time:
                time_delta = cpu_usage2_time - self._cpu_usage1_time
                # max possible cpu usage per one second adjusted to elapsed time between measurements
                total_delta = self._max_cpu_usage * time_delta
            if cpu_delta and total_delta:
                cpu_usage = round((cpu_delta / total_delta) * 100, 1)
            self._cpu_usage1_usec = cpu_usage2_usec
            self._cpu_usage1_time = cpu_usage2_time
        except BaseException:
            self._log.warning(f'Unable to determine cpu usage', exc_info=True)
        return cpu_usage

    @abstractmethod
    def _get_cgroups_cpu_usage_snapshot(self) -> Optional[int]:
        pass

    def _get_mem_info(self):
        """
        Returns memory usage in percentage of available memory.
        """
        memory_usage_pct = None
        try:
            memory_usage = self._get_cgroups_current_memory_usage()
            if self._max_memory_usage and memory_usage:
                memory_usage_pct = round((memory_usage / self._max_memory_usage) * 100, 1)
        except BaseException:
            self._log.warning(f'Unable to determine memory usage', exc_info=True)
        return memory_usage_pct

    @abstractmethod
    def _get_cgroups_current_memory_usage(self) -> Optional[int]:
        pass

    def _read_int_key_from_cgroups(self, filename: str, semantics: str, key: str) -> Optional[int]:
        path = os.path.join(self._cgroup_fs_path, filename)
        if not os.path.exists(path) or not os.path.isfile(path):
            self._log.warning(f'Unable to determine cgroups {semantics}. File {path} not found')
            return None
        try:
            with open(path) as f:
                lines = f.read().splitlines()
        except IOError:
            self._log.warning(f'Unable to determine cgroups {semantics}. File {path} is not readable')
            return None
        # the file contains multiple lines. Look for line “{key} 80173951“
        return Cgroups2LocalMonitor._get_key_value(lines, key)

    def _read_int_from_cgroups(self, filename: str, semantics: str) -> Optional[int]:
        path = os.path.join(self._cgroup_fs_path, filename)
        if not os.path.exists(path) or not os.path.isfile(path):
            self._log.warning(f'Unable to determine cgroups {semantics}. File {path} not found')
            return None

        try:
            with open(path) as f:
                content = f.readline()
        except IOError:
            self._log.warning(f'Unable to determine cgroups {semantics}. File {path} is not readable')
            return None

        try:
            int_value = int(content)
        except ValueError:
            self._log.debug(f'Unable to determine cgroups {semantics}. Value {content} is not an integer.')
            return None
        # max value can be represented as -1 or max 64bit value
        if int_value < 0 or int_value > 9000000000000000000:
            self._log.debug(f'Unable to determine cgroups {semantics}. Value {int_value} is outside of accepted range.')
            int_value = None
        return int_value

    @staticmethod
    def _get_key_value(lines: List[str], key: str):
        key_value_p = re.compile(r'^([^ ]+)\s([^ ]+)$')
        for line in lines:
            m = key_value_p.match(line)
            if m:
                k = m.group(1)
                if k == key:
                    try:
                        return int(m.group(2))
                    except ValueError:
                        pass
        return None


class Cgroups1LocalMonitor(BaseCgroupsLocalMonitor):
    """
    Local monitor that takes some health metrics from cgroups1 virtual file system for accuracy.
    """
    REQUIRED_FILES = [os.path.join('cpu', 'cpuacct.usage'), os.path.join('memory', 'memory.usage_in_bytes')]

    def __init__(self, cgroup_fs_path: str, parent_logger: Logger, metrics: List[str], engine):
        super().__init__(cgroup_fs_path, parent_logger, metrics, engine)
        self._log = parent_logger.getChild(self.__class__.__name__)

    def _get_cgroups_current_memory_usage(self) -> Optional[int]:
        total_memory_usage = self._read_int_from_cgroups(os.path.join('memory', 'memory.usage_in_bytes'),
                                                         'current memory usage')
        cache_memory_usage = self._read_int_key_from_cgroups(os.path.join('memory', 'memory.stat'), 'cache memory',
                                                             'total_inactive_file')
        if total_memory_usage and cache_memory_usage:
            total_memory_usage = total_memory_usage - cache_memory_usage
        return total_memory_usage

    def _get_cgroups_cpu_usage_snapshot(self) -> Optional[int]:
        cpu_usage = self._read_int_from_cgroups(os.path.join('cpu', 'cpuacct.usage'), 'cpu usage')
        if cpu_usage:
            # cgroups1 cpuacct.usage contains cpu usage in nanoseconds, but we need value in microseconds
            cpu_usage = round(cpu_usage / 1000)
        return cpu_usage

    def _get_cgroups_max_cpu_usage(self) -> Optional[int]:
        """
        Returns maximum possible cpu usage per one second determined from cgroups1 limits. Returns None if there is
        no cgroups limit set.
        """
        quota_us = self._read_int_from_cgroups(os.path.join('cpu', 'cpu.cfs_quota_us'), 'max cpu quota us')
        period_us = self._read_int_from_cgroups(os.path.join('cpu', 'cpu.cfs_period_us'), 'max cpu period us')

        if quota_us and period_us:
            cpu_periods_count = 1 / (period_us / 1000000)
            return int(cpu_periods_count * quota_us)
        self._log.debug(f'No cgroups1 cpu usage limit set: {quota_us} {period_us}')
        return None

    def _get_cgroups_max_memory_usage(self) -> Optional[int]:
        return self._read_int_from_cgroups(os.path.join('memory', 'memory.limit_in_bytes'), 'max memory usage')


class Cgroups2LocalMonitor(BaseCgroupsLocalMonitor):
    """
    Local monitor that takes some health metrics from cgroups2 virtual file system for accuracy.
    """
    REQUIRED_FILES = ['cpu.stat', 'memory.current']

    def __init__(self, cgroup_fs_path: str, parent_logger: Logger, metrics: List[str], engine):
        super().__init__(cgroup_fs_path, parent_logger, metrics, engine)
        self._log = parent_logger.getChild(self.__class__.__name__)

    def _get_cgroups_current_memory_usage(self) -> Optional[int]:
        total_memory_usage = self._read_int_from_cgroups('memory.current', 'current memory usage')
        cache_memory_usage = self._read_int_key_from_cgroups('memory.stat', 'cache memory', 'inactive_file')
        if total_memory_usage and cache_memory_usage:
            total_memory_usage = total_memory_usage - cache_memory_usage
        return total_memory_usage

    def _get_cgroups_cpu_usage_snapshot(self) -> Optional[int]:
        # the file /sys/fs/cgroup/cpu.stat contains multiple lines. Look for line “usage_usec 80173951“
        return self._read_int_key_from_cgroups('cpu.stat', 'cpu usage', 'usage_usec')

    def _get_cgroups_max_cpu_usage(self) -> Optional[int]:
        """
        Returns maximum possible cpu usage per one second determined from cgroups2 limits. Returns None if there is
        no cgroups2 limit set.
        """
        path = os.path.join(self._cgroup_fs_path, 'cpu.max')
        if not os.path.exists(path) or not os.path.isfile(path):
            self._log.warning(f'Unable to determine cgroups2 max cpu usage. File {path} not found')
            return None

        max_cpu_p = re.compile(r'^([^ ]+)\s([^ ]+)$')
        try:
            with open(path) as f:
                content = f.readline()
        except IOError:
            self._log.warning(f'Unable to determine cgroups2 max cpu usage. File {path} is not readable')
            return None
        m = max_cpu_p.match(content)
        if m:
            try:
                quota_us = int(m.group(1))
            except ValueError:
                quota_us = None
            try:
                period_us = int(m.group(2))
            except ValueError:
                period_us = None
            if quota_us and period_us:
                cpu_periods_count = 1 / (period_us / 1000000)
                return int(cpu_periods_count * quota_us)
        self._log.debug(f'No cgroups2 cpu usage limit set: {content}')
        return None

    def _get_cgroups_max_memory_usage(self) -> Optional[int]:
        return self._read_int_from_cgroups('memory.max', 'max memory usage')


class LocalMonitorFactory:

    @classmethod
    def create_local_monitor(cls, parent_logger: Logger, metrics: List[str], engine,
                             mtab_path=None) -> BaseLocalMonitor:
        """
        Constructs concrete instance of BaseLocalMonitor depending on OS and cgroups version.
        """
        log = parent_logger.getChild(cls.__name__)
        if not mtab_path:
            if is_windows():
                return StandardLocalMonitor(parent_logger, metrics, engine)
            mtab_path = '/etc/mtab'
        cgroups_version, cgroups_fs_path = cls._detect_cgroup_info(log, mtab_path)
        if cgroups_version == 2:
            return Cgroups2LocalMonitor(cgroups_fs_path, parent_logger, metrics, engine)
        elif cgroups_version == 1:
            return Cgroups1LocalMonitor(cgroups_fs_path, parent_logger, metrics, engine)
        else:
            return StandardLocalMonitor(parent_logger, metrics, engine)

    @classmethod
    def _detect_cgroup_info(cls, log: Logger, mtab_path: str):
        """
        Detects version of cgroups and base path of cgroups filesystem. By default, it is '/sys/fs/cgroup',
        but it could be customized.
        """
        if not os.path.exists(mtab_path) or not os.path.isfile(mtab_path):
            log.warning(f'Unable to detect cgroups version. File {mtab_path} not found')
            return None, None
        try:
            with open(mtab_path) as f:
                lines = f.read().splitlines()
        except IOError:
            log.warning(f'Unable to detect cgroups version. File {mtab_path} is not readable')
            return None, None

        mtab_p = re.compile(r'^([^ ]+)\s([^ ]+)\s([^ ]+).*$')
        cgroups_version = None
        cgroups_fs_path = None
        for line in lines:
            m = mtab_p.match(line)
            if m:
                fs_type = m.group(3)
                if fs_type == 'cgroup2':
                    base_path = m.group(2)
                    # verify the path contains files we expect to be there, perhaps not all controllers are active
                    files_exist = all([os.path.exists(os.path.join(base_path, required_file)) for required_file in
                                       Cgroups2LocalMonitor.REQUIRED_FILES])
                    if files_exist:
                        return 2, base_path
                elif fs_type == 'cgroup':
                    base_path = os.path.dirname(m.group(2))
                    # verify the path contains files we expect to be there, perhaps not all controllers are active
                    files_exist = all([os.path.exists(os.path.join(base_path, required_file)) for required_file in
                                       Cgroups1LocalMonitor.REQUIRED_FILES])
                    if files_exist:
                        cgroups_version = 1
                        cgroups_fs_path = base_path
        return cgroups_version, cgroups_fs_path


class GraphiteClient(MonitoringClient):
    def __init__(self, parent_log, label, config, engine):
        super(GraphiteClient, self).__init__(parent_log, engine)
        self.config = config
        exc = TaurusConfigError('Graphite client requires address parameter')
        self.address = self.config.get("address", exc)
        self.timeout = int(dehumanize_time(self.config.get("timeout", "5s")))
        self.interval = int(dehumanize_time(self.config.get("interval", "5s")))     # interval for client
        self._cached_data = None
        self.url = self._get_url()

        if label:
            self.host_label = label
        else:
            self.host_label = self.address.replace('http://', '').replace('/', '').replace(':', '_')

        if self.config.get("logging", False):
            self.logs_file = self.engine.create_artifact("Graphite_logs_{}".format(self.host_label), ".csv")
            with open(self.logs_file, "a", newline='') as sa_logs:
                logs_writer = csv.writer(sa_logs, delimiter=',')
                metrics = ['ts'] + [metric for metric in self.config.get("metrics")]
                logs_writer.writerow(metrics)

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
        http_client = self.engine.get_http_client()
        response = http_client.request('GET', self.url, timeout=self.timeout)
        return response.json()

    def _get_response(self):
        try:
            json_list = self._data_transfer()
            msg = "Key 'target' not found in graphite response"
            assert all('target' in dic.keys() for dic in json_list), msg
            return json_list
        except BaseException:
            self.log.debug("Metrics receiving: %s", traceback.format_exc())
            self.log.warning("Fail to receive metrics from %s", self.address)
            return []

    def get_data(self):
        now = time.time()

        if now > self._last_check + self.interval:
            self._cached_data = []
            self._last_check = now
            json_list = self._get_response()
            data_line = [now]

            for element in json_list:
                item = {
                    'ts': now,
                    'source': '%s' % self.host_label}

                for datapoint in reversed(element['datapoints']):
                    if datapoint[0] is not None:
                        item[element['target']] = datapoint[0]
                        data_line.append(datapoint[0])
                        break

                self._cached_data.append(item)

            if self.logs_file:
                with open(self.logs_file, "a", newline='') as g_logs:
                    logs_writer = csv.writer(g_logs, delimiter=',')
                    logs_writer.writerow(data_line)

        return self._cached_data


class ServerAgentClient(MonitoringClient):
    def __init__(self, parent_log, label, config, engine):
        """
        :type parent_log: logging.Logger
        :type config: dict
        """
        super(ServerAgentClient, self).__init__(parent_log, engine)
        self.host_label = label
        exc = TaurusConfigError('ServerAgent client requires address parameter')
        self.address = config.get("address", exc)
        if ':' in self.address:
            self.port = int(self.address[self.address.index(":") + 1:])
            self.address = self.address[:self.address.index(":")]
        else:
            self.port = 4444

        self._partial_buffer = ""
        exc = TaurusConfigError('ServerAgent client requires metrics list')
        metrics = config.get('metrics', exc)

        self._result_fields = [x for x in metrics]

        self._metrics_command = "\t".join([x for x in metrics])
        self.socket = socket.socket()
        self.select = select.select

        self.config = config

        # interval for server (ServerAgent)
        self.interval = int(dehumanize_time(config.get("interval", "1s")))

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

        if self.config.get("logging", False):
            self.logs_file = self.engine.create_artifact("SAlogs_{}_{}".format(self.address, self.port), ".csv")
            with open(self.logs_file, "a", newline='') as sa_logs:
                logs_writer = csv.writer(sa_logs, delimiter=',')
                metrics = ['ts'] + sorted([metric for metric in self._result_fields])
                logs_writer.writerow(metrics)

    def disconnect(self):
        self.log.debug("Closing connection with agent at %s:%s...", self.address, self.port)
        try:
            self.socket.send(b("exit\n"))
        except BaseException as exc:
            self.log.warning("Error during disconnecting from agent at %s:%s: %s", self.address, self.port, exc)
        finally:
            self.socket.close()

    def start(self):
        self.socket.send(b("interval:%s\n" % self.interval))
        command = "metrics:%s\n" % self._metrics_command
        self.log.debug("Sending metrics command: %s", command)
        self.socket.send(b(command))
        self.socket.setblocking(False)

    def get_data(self):
        """
        :rtype: list[dict]
        """
        now = time.time()
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
                item['ts'] = now
                item['source'] = source
                res.append(item)

                if self.logs_file:
                    with open(self.logs_file, "a", newline='') as sa_logs:
                        line = [str(round(item['ts']))] + line[:-1].split("\t")
                        logs_writer = csv.writer(sa_logs, delimiter=',')
                        logs_writer.writerow(line)

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

            if metrics:
                maxwidth = max([len(key) for key in metrics])

                for metric, value in iteritems(metrics):
                    if value[0] is None:
                        rendered = 'N/A'
                    elif isinstance(value[0], int):
                        rendered = "{:,}".format(value[0])
                    elif isinstance(value[0], float):
                        rendered = "{:.3f}".format(value[0])
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
