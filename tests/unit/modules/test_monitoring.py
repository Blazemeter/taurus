import csv
import os.path
import time
import tempfile
import shutil
from collections import namedtuple
from typing import List
from unittest.mock import patch

from bzt.modules.monitoring import Monitoring, MonitoringListener, MonitoringCriteria, StandardLocalMonitor, \
    LocalMonitorFactory, Cgroups2LocalMonitor, BaseLocalMonitor, Cgroups1LocalMonitor
from bzt.modules.monitoring import ServerAgentClient, GraphiteClient, LocalClient
from bzt.utils import b, BetterDict
from tests.unit import BZTestCase, ROOT_LOGGER, EngineEmul, RESOURCES_DIR
from tests.unit.mocks import SocketEmul


class TestMonitoring(BZTestCase):
    def setUp(self):
        super(TestMonitoring, self).setUp()

    def test_server_agent(self):
        obj = Monitoring()
        obj.engine = EngineEmul()
        obj.parameters.merge({
            "server-agent": [{
                "address": "127.0.0.1:4444",
                "logging": "True",
                "metrics": [
                    "cpu",
                    "disks"
                ]
            }, {
                "address": "10.0.0.1",
                "metrics": [
                    "something1",
                    "something2"
                ]
            }]
        })

        listener = LoggingMonListener()
        obj.add_listener(listener)

        widget = obj.get_widget()
        obj.add_listener(widget)

        crit_conf = BetterDict.from_dict({"condition": ">", "threshold": 5, "subject": "127.0.0.1:4444/cpu"})
        criteria = MonitoringCriteria(crit_conf, obj)
        obj.add_listener(criteria)

        obj.client_classes = {'server-agent': ServerAgentClientEmul}

        obj.prepare()
        obj.startup()

        for i in range(1, 10):
            obj.clients[0].socket.recv_data += b("%s\t%s\t\n" % (i, i*10))
            obj.check()
            ROOT_LOGGER.debug("Criteria state: %s", criteria)
            time.sleep(obj.engine.check_interval)

        obj.shutdown()
        obj.post_process()

        self.assertEquals(b("test\ninterval:1\nmetrics:cpu\tdisks\nexit\n"), obj.clients[0].socket.sent_data)

        self.assertIsNotNone(obj.clients[0].logs_file)
        with open(obj.clients[0].logs_file) as serveragent_logs:
            logs_reader = csv.reader(serveragent_logs)
            logs_reader = list(logs_reader)
        self.assertEquals(['ts', 'cpu', 'disks'], logs_reader[0])
        for i in range(1, 10):
            self.assertEquals([str(i), str(i * 10)], logs_reader[i][1:])

    def test_graphite(self):
        obj = Monitoring()
        obj.engine = EngineEmul()
        obj.parameters.merge({
            "graphite": [{
                "address": "people.com:1066",
                "label": "Earth",
                "logging": True,
                "metrics": [
                    "body",
                    "brain"]}, {
                "address": "http://spirits.net",
                "metrics": [
                    "transparency",
                    "usability"
                ]}]
        })
        obj.client_classes = {'graphite': GraphiteClientEmul}
        obj.prepare()
        obj.startup()
        obj.check()

        obj.clients[0]._last_check -= obj.clients[0].interval * 2
        obj.check()

        obj.clients[0]._last_check -= obj.clients[0].interval * 2
        obj.clients[0].prepared_data = "wrong data"
        obj.check()

        obj.shutdown()
        obj.post_process()

        self.assertIsNotNone(obj.clients[0].logs_file)
        self.assertIsNone(obj.clients[1].logs_file)
        with open(obj.clients[0].logs_file) as graphite_logs:
            logs_reader = csv.reader(graphite_logs)
            logs_reader = list(logs_reader)
        self.assertEquals(['ts', 'body', 'brain'], logs_reader[0])
        self.assertEquals(['2'], logs_reader[1][1:])

    def test_local_with_engine(self):
        config = {'interval': '5m', 'metrics': ['cpu', 'engine-loop']}
        obj = LocalClient(ROOT_LOGGER, 'label', config, EngineEmul())
        obj.connect()
        data = obj.get_data()
        self.assertTrue(300, obj.interval)
        self.assertTrue(all('source' in item and 'ts' in item for item in data))

    def test_deprecated_local(self):
        # strange metrics, anyway full list of available ones will be used
        config = {'metrics': ['memory', 'disk']}

        obj = LocalClient(ROOT_LOGGER, 'label', config)
        obj.engine = EngineEmul()
        obj.connect()
        data = obj.engine_resource_stats()
        self.assertTrue(all(val is not None for val in [
            data.cpu, data.disk_usage, data.mem_usage, data.mem_usage, data.rx,
            data.tx, data.dru, data.dwu, data.engine_loop, data.conn_all]))

    def test_all_metrics(self):
        config = {'interval': '1m', 'metrics': LocalClient.AVAILABLE_METRICS}
        obj = LocalClient(ROOT_LOGGER, 'label', config, EngineEmul())
        obj.connect()
        self.assertEqual(60, obj.interval)
        data = obj.get_data()
        for item in data:
            metrics = set(item.keys()) - {'ts', 'source'}
            self.assertEqual(1, len(metrics))
            self.assertIn(metrics.pop(), LocalClient.AVAILABLE_METRICS)
        self.assertEqual(len(data), len(LocalClient.AVAILABLE_METRICS))

    def test_local_without_engine(self):
        config = {'metrics': ['cpu']}
        obj = LocalClient(ROOT_LOGGER, 'label', config, EngineEmul())
        obj.connect()
        data = obj.get_data()
        self.assertEqual(obj.interval, obj.engine.check_interval)
        self.assertTrue(all('source' in item.keys() and 'ts' in item.keys() for item in data))

    def test_multiple_local_monitorings(self):
        config1 = {'metrics': ['mem', 'engine-loop']}
        config2 = {'metrics': ['cpu', 'mem']}

        obj = Monitoring()
        obj.engine = EngineEmul()
        obj.parameters.merge({
            'local': [config1, config2]})

        obj.prepare()
        self.assertEqual(1, len(obj.clients))
        self.assertEqual({'mem', 'cpu', 'engine-loop'}, set(obj.clients[0].metrics))
        self.assertTrue(isinstance(obj.clients[0].monitor, BaseLocalMonitor))

        obj.prepare()
        self.assertEqual(1, len(obj.clients))
        self.assertEqual({'mem', 'cpu', 'engine-loop'}, set(obj.clients[0].metrics))
        self.assertTrue(isinstance(obj.clients[0].monitor, BaseLocalMonitor))

        data1 = obj.clients[0].get_data()
        obj.clients[0].interval = 1     # take cached data
        data2 = obj.clients[0].get_data()
        obj.clients[0].interval = 0     # throw cache
        data3 = obj.clients[0].get_data()
        for item1, item2, item3 in zip(data1, data2, data3):
            self.assertEqual(item1, item2)
            self.assertNotEqual(item2, item3)

        metrics = []
        for element in data1:
            self.assertIn('source', element)
            self.assertIn('ts', element)
            for key in element:
                if key not in ('source', 'ts'):
                    metrics.append(key)

        for config in (config1, config2):
            self.assertTrue(all(m in metrics for m in config['metrics']))

    def test_logs(self):
        config = {'logging': True, 'metrics': ['bytes-sent', 'mem', 'cpu']}
        obj = LocalClient(ROOT_LOGGER, 'label', config, EngineEmul())
        obj.connect()
        patch = obj._get_resource_stats
        obj._get_resource_stats = lambda: {'mem': '4', 'bytes-sent': '2', 'cpu': '3'}
        try:
            obj.get_data()
        finally:
            obj._get_resource_stats = patch
        self.assertIn('logging', obj.config)
        self.assertEqual(['bytes-sent', 'cpu', 'mem'], sorted(obj.config['metrics']))
        self.assertIsNotNone(obj.logs_file)
        with open(obj.logs_file) as monitoring_logs:
            logs_reader = csv.reader(monitoring_logs)
            logs_reader = list(logs_reader)
        self.assertEqual(['ts', 'bytes-sent', 'cpu', 'mem'], logs_reader[0])
        self.assertEqual(['2', '3', '4'], logs_reader[1][1:])

    def test_server_agent_encoding(self):
        obj = Monitoring()
        obj.engine = EngineEmul()
        obj.parameters.merge({
            "server-agent": [{
                "address": "127.0.0.1:4444",
                "metrics": [
                    "cpu",
                    "disks"
                ]
            }]
        })

        obj.client_classes = {'server-agent': ServerAgentClientEmul}
        obj.prepare()

        self.assertEquals(b("test\n"), obj.clients[0].socket.sent_data)

    def test_psutil_internal_package_usage(self):
        import psutil

        # Explicitly check named tuples construction - we use code from internal psutil package
        # (was moved between packages _common->_ntuples in 7.2.0, has different fields based on OS,
        # -> can be changed without warning in future)

        _ = psutil._ntuples.sdiskio( (0,)*(len(psutil._ntuples.sdiskio._fields)))  # pylint: disable=protected-access
        _ = psutil._ntuples.snetio( (0,)*(len(psutil._ntuples.snetio._fields)))  # pylint: disable=protected-access
        _ = psutil._ntuples.sdiskusage( (0,)*(len(psutil._ntuples.sdiskusage._fields)))  # pylint: disable=protected-access

    def test_psutil_potential_bugs(self):
        conf = {'metrics': ['cpu', 'mem', 'disks', 'conn-all']}
        client = LocalClient(ROOT_LOGGER, 'label', conf, EngineEmul())
        client.connect()

        import psutil

        try:
            net_io_counters = psutil.net_io_counters
            disk_io_counters = psutil.disk_io_counters
            psutil.net_io_counters = lambda: None
            psutil.disk_io_counters = lambda: None

            client.monitor.resource_stats()  # should throw no exception
        finally:
            psutil.net_io_counters = net_io_counters
            psutil.disk_io_counters = disk_io_counters


class LoggingMonListener(MonitoringListener):
    def monitoring_data(self, data):
        ROOT_LOGGER.debug("Data: %s", data)


class ServerAgentClientEmul(ServerAgentClient):
    def __init__(self, parent_logger, label, config, engine):
        super(ServerAgentClientEmul, self).__init__(parent_logger, label, config, engine)
        self.socket = SocketEmul()
        self.socket.recv_data = b("Yep\n")
        self.select = self.select_emul

    def select_emul(self, reads, writes, excepts, timeout):
        return reads, writes, []


class GraphiteClientEmul(GraphiteClient):
    prepared_data = [{'target': 'usability', 'datapoints': [[1, 1], [2, 2]]}]

    def _data_transfer(self):
        return self.prepared_data


class TestStandardLocalMonitor(BZTestCase):

    @patch('bzt.modules.monitoring.psutil')
    def test_get_mem_stats_failure(self, psutil):
        svmem = namedtuple('svmem', ['available'])
        psutil.virtual_memory.return_value = svmem(580373951)
        monitor = StandardLocalMonitor(ROOT_LOGGER, ['mem'], EngineEmul())
        stats = monitor.resource_stats()
        self.assertIsNone(stats['mem'])


class TestLocalMonitorFactory(BZTestCase):

    def test_create_local_monitor_mtab_cgroups1(self):
        dir = tempfile.mkdtemp(prefix='mtab-cgroups1-')
        try:
            # mtab file must contain a cgroup path that exists on the local system
            mtab_template_path = os.path.join(RESOURCES_DIR, 'monitoring', 'mtab', 'mtab-cgroups1')
            mtab_path = os.path.join(dir, 'mtab')
            self.__copy_file_template(mtab_template_path, mtab_path, {'/sys/fs/cgroup/': os.path.join(dir, 'ignored')})
            # create expected files
            self.__mk_files(dir,
                            [os.path.join('cpu', 'cpuacct.usage'), os.path.join('memory', 'memory.usage_in_bytes')])
            monitor = LocalMonitorFactory.create_local_monitor(ROOT_LOGGER, ['cpu', 'mem'], EngineEmul(), mtab_path)
            self.assertIsInstance(monitor, Cgroups1LocalMonitor)
        finally:
            shutil.rmtree(dir)

    def test_create_local_monitor_mtab_cgroups2(self):
        dir = tempfile.mkdtemp(prefix='mtab-cgroups2-')
        try:
            # mtab file must contain a cgroup path that exists on the local system
            mtab_template_path = os.path.join(RESOURCES_DIR, 'monitoring', 'mtab', 'mtab-cgroups2')
            mtab_path = os.path.join(dir, 'mtab')
            self.__copy_file_template(mtab_template_path, mtab_path, {'/sys/fs/cgroup': dir})
            # create expected files
            self.__mk_files(dir, ['cpu.stat', 'memory.current'])
            monitor = LocalMonitorFactory.create_local_monitor(ROOT_LOGGER, ['cpu', 'mem'], EngineEmul(), mtab_path)
            self.assertIsInstance(monitor, Cgroups2LocalMonitor)
        finally:
            shutil.rmtree(dir)

    def __mk_files(self, base_path: str, file_paths: List[str]):
        for file_path in file_paths:
            full_path = os.path.join(base_path, file_path)
            os.makedirs(os.path.dirname(full_path), exist_ok=True)
            with open(full_path, "w"):
                pass
    def __copy_file_template(self, template_path: str, target_path: str, mapping: dict):
        with open(template_path) as f:
            content = f.read()
        for k, v in mapping.items():
            content = content.replace(k, v)
        with open(target_path, "w") as f:
            f.write(content)

    @patch('bzt.modules.monitoring.open')
    def test_create_local_monitor_mtab_not_readable(self, open):
        open.side_effect = IOError('Test error')
        dir = tempfile.mkdtemp(prefix='mtab-cgroups2-')
        try:
            # mtab file must contain a cgroup path that exists on the local system
            mtab_template_path = os.path.join(RESOURCES_DIR, 'monitoring', 'mtab', 'mtab-cgroups2')
            mtab_path = os.path.join(dir, 'mtab')
            self.__copy_file_template(mtab_template_path, mtab_path, {'/sys/fs/cgroup': dir})
            # create expected files
            self.__mk_files(dir, ['cpu.stat', 'memory.current'])
            monitor = LocalMonitorFactory.create_local_monitor(ROOT_LOGGER, ['cpu', 'mem'], EngineEmul(), mtab_path)
            self.assertIsInstance(monitor, StandardLocalMonitor)
        finally:
            shutil.rmtree(dir)

    @patch('bzt.modules.monitoring.is_windows')
    def test_create_local_monitor(self, is_windows):
        test_params = [
            (True, None, StandardLocalMonitor),
            (False, None, BaseLocalMonitor),
            (False, 'mtab-nonexistent', StandardLocalMonitor),
            (False, 'mtab-empty', StandardLocalMonitor),
        ]
        for windows, mtab_file, expected_monitor_cls in test_params:
            with self.subTest(str((windows, mtab_file, expected_monitor_cls))):
                is_windows.return_value = windows
                mtab_path = os.path.join(RESOURCES_DIR, 'monitoring', 'mtab', mtab_file) if mtab_file else None
                monitor = LocalMonitorFactory.create_local_monitor(ROOT_LOGGER, ['cpu', 'mem'], EngineEmul(), mtab_path)
                self.assertIsInstance(monitor, expected_monitor_cls)


class TestCgroups1LocalMonitor(BZTestCase):

    def test_get_mem_stats_no_limit(self):
        cgroups_path = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups1', 'no_mem_limit')
        monitor = Cgroups1LocalMonitor(cgroups_path, ROOT_LOGGER, ['mem'], EngineEmul())
        stats = monitor.resource_stats()
        self.assertGreater(stats['mem'], 0.0)

    def test_get_mem_stats(self):
        test_params = [
            ('nonexistent', None),
            ('with_mem_limit', 9.6),
            ('no_total_inactive_file', 12.5),
            ('zero_total_inactive_file', 12.5),
            ('invalid_total_inactive_file', 12.5),
        ]
        for cgroups_dir, value_eq in test_params:
            with self.subTest(str((cgroups_dir, value_eq))):
                cgroups_path = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups1',
                                            cgroups_dir) if cgroups_dir else None
                monitor = Cgroups1LocalMonitor(cgroups_path, ROOT_LOGGER, ['mem'], EngineEmul())
                stats = monitor.resource_stats()
                self.assertEqual(stats['mem'], value_eq)

    @patch('bzt.modules.monitoring.open')
    def test_get_mem_stats_not_readable(self, open):
        open.side_effect = IOError('Test error')
        cgroups_path = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups1', 'with_mem_limit')
        monitor = Cgroups1LocalMonitor(cgroups_path, ROOT_LOGGER, ['mem'], EngineEmul())
        stats = monitor.resource_stats()
        self.assertIsNone(stats['mem'])

    def test_get_cpu_stats(self):
        test_params = [
            ('nonexistent1', 'nonexistent2', 0.0, 0.0, 0.4),
            # cpu usage could be any value depending on the number of cpu cores available and sleep accuracy
            ('no_cpu_limit1', 'no_cpu_limit2', 0.1, 100.0, 0.4),
            # should be about 50%, extra range due to 400ms sleep accuracy
            ('with_cpu_limit1', 'with_cpu_limit2', 30.0, 70.0, 0.4),
            # should be about 25%, extra range due to 400ms sleep accuracy
            ('with_small_cpu_period1', 'with_small_cpu_period2', 10.0, 40.0, 0.4),
            # invalid value in cpuacct.usage
            ('with_invalid_cpu_usage1', 'with_invalid_cpu_usage1', 0.0, 0.0, 0.4),
            # absent cpuacct.usage file
            ('with_absent_cpu_usage1', 'with_absent_cpu_usage1', 0.0, 0.0, 0.4),
        ]
        for cgroups_dir1, cgroups_dir2, value_gte, value_lte, sleep_sec in test_params:
            with self.subTest(str((cgroups_dir1, cgroups_dir2, value_gte, value_lte))):
                cgroups_path1 = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups1',
                                             cgroups_dir1) if cgroups_dir1 else None
                cgroups_path2 = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups1',
                                             cgroups_dir2) if cgroups_dir2 else None
                monitor = Cgroups1LocalMonitor(cgroups_path1, ROOT_LOGGER, ['cpu'], EngineEmul())
                stats = monitor.resource_stats()
                self.assertEqual(stats['cpu'], 0)
                # resource_stats() requires a short time period to elapse
                time.sleep(sleep_sec)
                monitor._cgroup_fs_path = cgroups_path2
                stats = monitor.resource_stats()
                self.assertGreaterEqual(stats['cpu'], value_gte)
                self.assertLessEqual(stats['cpu'], value_lte)

    @patch('bzt.modules.monitoring.open')
    def test_get_cpu_stats_not_readable(self, open):
        open.side_effect = IOError('Test error')
        cgroups_path1 = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups1', 'with_cpu_limit1')
        cgroups_path2 = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups1', 'with_cpu_limit2')
        monitor = Cgroups1LocalMonitor(cgroups_path1, ROOT_LOGGER, ['cpu'], EngineEmul())
        monitor.resource_stats()
        # resource_stats() requires a short time period to elapse
        time.sleep(0.1)
        monitor._cgroup_fs_path = cgroups_path2
        stats = monitor.resource_stats()
        self.assertEqual(stats['cpu'], 0)


class TestCgroups2LocalMonitor(BZTestCase):

    def test_get_mem_stats_no_limit(self):
        cgroups_path = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups2', 'no_mem_limit')
        monitor = Cgroups2LocalMonitor(cgroups_path, ROOT_LOGGER, ['mem'], EngineEmul())
        stats = monitor.resource_stats()
        self.assertGreater(stats['mem'], 0.0)

    @patch('bzt.modules.monitoring.psutil')
    def test_get_mem_stats_limit_failure(self, psutil):
        svmem = namedtuple('svmem', ['available'])
        psutil.virtual_memory.return_value = svmem(580373951)
        cgroups_path = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups2', 'no_mem_limit')
        monitor = Cgroups2LocalMonitor(cgroups_path, ROOT_LOGGER, ['mem'], EngineEmul())
        stats = monitor.resource_stats()
        self.assertIsNone(stats['mem'])

    def test_get_mem_stats(self):
        test_params = [
            ('nonexistent', None),
            ('with_mem_limit', 9.6),
            ('no_inactive_file', 12.5),
            ('zero_inactive_file', 12.5),
            ('invalid_inactive_file', 12.5),
        ]
        for cgroups_dir, value_eq in test_params:
            with self.subTest(str((cgroups_dir, value_eq))):
                cgroups_path = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups2',
                                            cgroups_dir) if cgroups_dir else None
                monitor = Cgroups2LocalMonitor(cgroups_path, ROOT_LOGGER, ['mem'], EngineEmul())
                stats = monitor.resource_stats()
                self.assertEqual(stats['mem'], value_eq)

    @patch('bzt.modules.monitoring.open')
    def test_get_mem_stats_not_readable(self, open):
        open.side_effect = IOError('Test error')
        cgroups_path = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups2', 'with_mem_limit')
        monitor = Cgroups2LocalMonitor(cgroups_path, ROOT_LOGGER, ['mem'], EngineEmul())
        stats = monitor.resource_stats()
        self.assertIsNone(stats['mem'])

    def test_get_cpu_stats(self):
        test_params = [
            ('nonexistent1', 'nonexistent2', 0.0, 0.0, 0.4),
            # cpu usage could be any value depending on the number of cpu cores available and sleep accuracy
            ('no_cpu_limit1', 'no_cpu_limit2', 0.1, 100.0, 0.4),
            ('invalid_cpu_quota1', 'invalid_cpu_quota2', 0.1, 100.0, 0.4),
            # should be about 50%, extra range due to 400ms sleep accuracy
            ('with_cpu_limit1', 'with_cpu_limit2', 30.0, 70.0, 0.4),
            # should be about 25%, extra range due to 400ms sleep accuracy
            ('with_small_cpu_period1', 'with_small_cpu_period2', 10.0, 40.0, 0.4),
            # invalid usage_usec value in cpu.stat
            ('with_invalid_cpu_usage1', 'with_invalid_cpu_usage1', 0.0, 0.0, 0.4),
            # absent usage_usec in cpu.stat
            ('with_absent_cpu_usage1', 'with_absent_cpu_usage1', 0.0, 0.0, 0.4),
        ]
        for cgroups_dir1, cgroups_dir2, value_gte, value_lte, sleep_sec in test_params:
            with self.subTest(str((cgroups_dir1, cgroups_dir2, value_gte, value_lte))):
                cgroups_path1 = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups2',
                                             cgroups_dir1) if cgroups_dir1 else None
                cgroups_path2 = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups2',
                                             cgroups_dir2) if cgroups_dir2 else None
                monitor = Cgroups2LocalMonitor(cgroups_path1, ROOT_LOGGER, ['cpu'], EngineEmul())
                stats = monitor.resource_stats()
                self.assertEqual(stats['cpu'], 0)
                # resource_stats() requires a short time period to elapse
                time.sleep(sleep_sec)
                monitor._cgroup_fs_path = cgroups_path2
                stats = monitor.resource_stats()
                self.assertGreaterEqual(stats['cpu'], value_gte)
                self.assertLessEqual(stats['cpu'], value_lte)

    @patch('bzt.modules.monitoring.open')
    def test_get_cpu_stats_not_readable(self, open):
        open.side_effect = IOError('Test error')
        cgroups_path1 = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups2', 'with_cpu_limit1')
        cgroups_path2 = os.path.join(RESOURCES_DIR, 'monitoring', 'cgroups2', 'with_cpu_limit2')
        monitor = Cgroups2LocalMonitor(cgroups_path1, ROOT_LOGGER, ['cpu'], EngineEmul())
        monitor.resource_stats()
        # resource_stats() requires a short time period to elapse
        time.sleep(0.1)
        monitor._cgroup_fs_path = cgroups_path2
        stats = monitor.resource_stats()
        self.assertEqual(stats['cpu'], 0)
