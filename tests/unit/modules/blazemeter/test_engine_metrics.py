import sys

from modules.blazemeter.engine_metrics import EngineMetricsBuffer, HappysocksMetricsConverter
from unit import BZTestCase


class TestEngineMetricsBuffer(BZTestCase):

    def test_empty(self):
        buffer = EngineMetricsBuffer()
        data = buffer.get_data()
        self.assertEqual(data, [])

    def test_push_get(self):
        buffer = EngineMetricsBuffer()
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
        buffer = EngineMetricsBuffer(2)
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
        buffer = EngineMetricsBuffer(2)
        buffer.record_data([
            {'source': 'local', 'ts': 1678892271.3985019, 'cpu': 9.4},
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
        ], 'r-v4-64102f1ab8795890049369', 100, 200, 300)
        self.assertEqual(result, [
            {
                'metadata': {
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
