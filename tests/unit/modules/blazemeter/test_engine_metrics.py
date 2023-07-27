import sys

from modules.blazemeter.engine_metrics import HSReportingBuffer, HappysocksMetricsConverter, \
    HappySocksConcurrencyConverter
from unit import BZTestCase, random_datapoint


class TestEngineMetricsBuffer(BZTestCase):

    def test_empty(self):
        buffer = HSReportingBuffer()
        data = buffer.get_data()
        self.assertEqual(data, [])

    def test_push_get(self):
        buffer = HSReportingBuffer()
        buffer.record_data([
            {'source': 'local', 'ts': 1678892271.3985019, 'cpu': 9.4},
            {'source': 'local', 'ts': 1678892271.3985019, 'mem': 55.6},
            {'source': 'local', 'ts': 1678892271.3985019, 'bytes-sent': 26302},
        ])
        data1 = buffer.get_data()
        self.assertIsInstance(data1, list)
        self.assertEqual(len(data1), 3)
        data2 = buffer.get_data()
        self.assertIsInstance(data2, list)
        self.assertEqual(len(data2), 3)
        self.assertEqual(data1, data2)

    def test_limit_reached(self):
        buffer = HSReportingBuffer(2, True)
        buffer.record_data([
            {'source': 'local', 'ts': 1678892271.3985019, 'cpu': 9.4},
            {'source': 'local', 'ts': 1678892271.3985019, 'mem': 55.6},
            {'source': 'local', 'ts': 1678892271.3985019, 'bytes-sent': 26302},
        ])
        data = buffer.get_data()
        self.assertIsInstance(data, list)
        self.assertEqual(len(data), 2)
        self.assertEqual(len(list(filter(lambda item: 'mem' in item, data))), 1)
        self.assertEqual(len(list(filter(lambda item: 'bytes-sent' in item, data))), 1)

    def test_clear(self):
        buffer = HSReportingBuffer(2, False)
        buffer.record_data([
            {'timestamp': 1678892271398, 'concurrency': 7},
        ])
        buffer.clear()
        data = buffer.get_data()
        self.assertIsInstance(data, list)
        self.assertEqual(len(data), 0)


class TestHappysocksMetricsConverter(BZTestCase):

    def setUp(self):
        super().setUp()
        sys.stdout = self.stdout_backup  # super().setUp() disables sys.stdout, restore it

    def tearDown(self):
        super().tearDown()

    def test_empty(self):
        result = HappysocksMetricsConverter.to_metrics_batch([], 'r-v4-64102f1ab8795890049369', 100, 200, 300)
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 0)

    def test_known_metrics(self):
        result = HappysocksMetricsConverter.to_metrics_batch([
            {'source': 'local', 'ts': 1678892271.3985019, 'cpu': 9.4},
            {'source': 'local', 'ts': 1678892271.3985019, 'mem': 55.6},
            {'source': 'local', 'ts': 1678892271.3985019, 'bytes-recv': 2485},
            {'source': 'local', 'ts': 1678892271.3985019, 'conn-all': 63},
            {'source': 'local', 'ts': 1678892271.3985019, 'engine-loop': 10},
            {'source': 'local', 'ts': 1678892271.3985019, 'disk-space': 30.6},
        ], 'r-v4-64102f1ab8795890049369', 100, 200, 300)
        self.assertEqual(result, [
            {
                'metadata': {
                    'type': 'engine-health',
                    'source': 'local',
                    'entityId': 'r-v4-64102f1ab8795890049369',
                    'masterId': 100,
                    'calibrationId': 200,
                    'calibrationStepId': 300,
                },
                'timestamp': 1678892271398,
                'values': {
                    'cpu': 9.4,
                    'mem': 55.6,
                    'network_io': 2485,
                    'connections': 63,
                    'busy_taurus': 10,
                    'disk_space': 30.6,
                }
            },
        ])

    def test_multi_metrics(self):
        result = HappysocksMetricsConverter.to_metrics_batch([
            {'source': 'local', 'ts': 1678892271.3985019, 'cpu': 9.4, 'mem': 55.6},
            {'source': 'local', 'ts': 1678892300.4565019, 'mem': 35.6},
        ], 'r-v4-64102f1ab8795890049369', 100, 200, 300)
        self.assertEqual(result, [
            {
                'metadata': {
                    'type': 'engine-health',
                    'source': 'local',
                    'entityId': 'r-v4-64102f1ab8795890049369',
                    'masterId': 100,
                    'calibrationId': 200,
                    'calibrationStepId': 300,
                },
                'timestamp': 1678892271398,
                'values': {
                    'cpu': 9.4,
                    'mem': 55.6,
                }
            },
            {
                'metadata': {
                    'type': 'engine-health',
                    'source': 'local',
                    'entityId': 'r-v4-64102f1ab8795890049369',
                    'masterId': 100,
                    'calibrationId': 200,
                    'calibrationStepId': 300,
                },
                'timestamp': 1678892300456,
                'values': {
                    'mem': 35.6,
                }
            },
        ])

    def test_unknown_metrics(self):
        result = HappysocksMetricsConverter.to_metrics_batch([
            {'source': 'local', 'ts': 1678892271.3985019, 'unknown': 2485},
        ], 'r-v4-64102f1ab8795890049369', 100, 200, 300)
        self.assertEqual(result, [])

    def test_multiple_timestamps(self):
        result = HappysocksMetricsConverter.to_metrics_batch([
            {'source': 'local', 'ts': 1678892271.3985019, 'cpu': 9.4},
            {'source': 'local', 'ts': 1678892300.4565019, 'mem': 55.6},
        ], 'r-v4-64102f1ab8795890049369', 100, 200, 300)
        self.assertEqual(result, [
            {
                'metadata': {
                    'type': 'engine-health',
                    'source': 'local',
                    'entityId': 'r-v4-64102f1ab8795890049369',
                    'masterId': 100,
                    'calibrationId': 200,
                    'calibrationStepId': 300,
                },
                'timestamp': 1678892271398,
                'values': {
                    'cpu': 9.4,
                }
            },
            {
                'metadata': {
                    'type': 'engine-health',
                    'source': 'local',
                    'entityId': 'r-v4-64102f1ab8795890049369',
                    'masterId': 100,
                    'calibrationId': 200,
                    'calibrationStepId': 300,
                },
                'timestamp': 1678892300456,
                'values': {
                    'mem': 55.6,
                }
            },
        ])

    def test_no_calibration(self):
        result = HappysocksMetricsConverter.to_metrics_batch([
            {'source': 'local', 'ts': 1678892271.3985019, 'cpu': 9.4},
            {'source': 'local', 'ts': 1678892271.3985019, 'mem': 55.6},
        ], 'r-v4-64102f1ab8795890049369', 100)
        self.assertEqual(result, [
            {
                'metadata': {
                    'type': 'engine-health',
                    'source': 'local',
                    'entityId': 'r-v4-64102f1ab8795890049369',
                    'masterId': 100,
                },
                'timestamp': 1678892271398,
                'values': {
                    'cpu': 9.4,
                    'mem': 55.6,
                }
            },
        ])

    def test_no_master_id(self):
        result = HappysocksMetricsConverter.to_metrics_batch([
            {'source': 'local', 'ts': 1678892271.3985019, 'cpu': 9.4},
            {'source': 'local', 'ts': 1678892271.3985019, 'mem': 55.6},
        ], 'r-v4-64102f1ab8795890049369')
        self.assertEqual(result, [
            {
                'metadata': {
                    'type': 'engine-health',
                    'source': 'local',
                    'entityId': 'r-v4-64102f1ab8795890049369',
                },
                'timestamp': 1678892271398,
                'values': {
                    'cpu': 9.4,
                    'mem': 55.6,
                }
            },
        ])


class TestHappySocksConcurrencyConverter(BZTestCase):

    def setUp(self):
        super().setUp()
        sys.stdout = self.stdout_backup  # super().setUp() disables sys.stdout, restore it

    def tearDown(self):
        super().tearDown()

    def test_empty(self):
        result = HappySocksConcurrencyConverter.to_concurrency_batch([], 'r-v4-64102f1ab8795890049369', 100)
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 0)

    def test_concurrency_batch(self):
        result = HappySocksConcurrencyConverter.to_concurrency_batch([
            {'timestamp': 1678892271398, 'concurrency': 8},
            {'timestamp': 1678892274765, 'concurrency': 500}
        ], 'r-v4-64102f1ab8795890049369', 100)
        self.assertEqual([
            {
                'metadata': {
                    'sessionId': 'r-v4-64102f1ab8795890049369',
                    'masterId': 100,
                },
                'timestamp': 1678892271398,
                'concurrency': 8
            },
            {
                'metadata': {
                    'sessionId': 'r-v4-64102f1ab8795890049369',
                    'masterId': 100,
                },
                'timestamp': 1678892274765,
                'concurrency': 500
            }
        ], result)

    def test_extract_concurrency_data(self):
        ts = 1234
        data = random_datapoint(ts)
        res = HappySocksConcurrencyConverter.extract_concurrency_data(data)
        self.assertEqual([{'timestamp': ts * 1000, 'concurrency': data['current']['']['concurrency']}], res)

    def test_extract_concurrency_data_not_data_point(self):
        data = {'ts': 5, 'current': {'': {'concurrency': 5}}}
        res = HappySocksConcurrencyConverter.extract_concurrency_data(data)
        self.assertIsNone(res)

    def test_extract_concurrency_data_missing_ts_key(self):
        data = random_datapoint(12345)
        data.pop('ts')
        res = HappySocksConcurrencyConverter.extract_concurrency_data(data)
        self.assertIsNone(res)

    def test_extract_concurrency_data_missing_current_key(self):
        data = random_datapoint(12345)
        data.pop('current')
        res = HappySocksConcurrencyConverter.extract_concurrency_data(data)
        self.assertIsNone(res)

    def test_extract_concurrency_data_missing_all_key(self):
        data = random_datapoint(12345)
        data['current'].pop('')
        res = HappySocksConcurrencyConverter.extract_concurrency_data(data)
        self.assertIsNone(res)

    def test_extract_concurrency_data_all_not_kpi_set(self):
        data = random_datapoint(12345)
        data['current'].pop('')
        data['current'][''] = {'concurrency': 100}
        res = HappySocksConcurrencyConverter.extract_concurrency_data(data)
        self.assertIsNone(res)
