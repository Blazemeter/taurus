import json
import time

from bzt.utils import to_json
from tests.unit import BZTestCase, ROOT_LOGGER

from bzt.modules.aggregator import ResultsReader, DataPoint, KPISet, RespTimesCounter
from tests.unit.mocks import r, rc, err, MockReader

from hdrh.histogram import HdrHistogram


class TestResultsReader(BZTestCase):
    def setUp(self):
        super(TestResultsReader, self).setUp()
        self.obj = ResultsReader()
        self.obj.track_percentiles = [25, 50, 75, 80, 90, 95, 99, 99.9, 100]
        self.obj.buffer_scale_idx = str(float(self.obj.track_percentiles[-1]))
        self.obj.buffer_len = 1

    def test_1(self):
        obj = self.obj

        mock = MockReader()
        mock.buffer_scale_idx = '100.0'
        mock.data.append((1, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((2, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((2, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((3, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((3, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((4, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((4, "", 1, r(), r(), r(), 200, None, '', 0))

        obj.add_listener(mock)

        for point in mock.datapoints():
            self.assertNotEquals(0, point[DataPoint.CUMULATIVE][''][KPISet.CONCURRENCY])

        mock.data.append((2, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((2, "", 1, r(), r(), r(), 200, None, '', 0))

        for point in mock.datapoints():
            pass

        for point in mock.datapoints(True):
            pass

        for point in mock.results:
            overall = point[DataPoint.CURRENT]['']
            self.assertTrue(len(overall[KPISet.PERCENTILES]) > 0)

    def test_new_reader(self):
        mock = MockReader()
        mock.buffer_scale_idx = '100.0'
        # data format: t_stamp, label, conc, r_time, con_time, latency, r_code, error, trname, byte_count
        mock.data.append((1, "a", 1, 1, 1, 1, 200, None, '', 0))
        mock.data.append((2, "b", 1, 2, 2, 2, 200, None, '', 0))
        mock.data.append((2, "b", 1, 3, 3, 3, 404, "Not Found", '', 0))
        mock.data.append((2, "c", 1, 4, 4, 4, 200, None, '', 0))
        mock.data.append((3, "d", 1, 5, 5, 5, 200, None, '', 0))
        mock.data.append((4, "b", 1, 6, 6, 6, 200, None, '', 0))

        list(mock.datapoints(True))

        failed = mock.results[1]
        self.assertEqual(2, failed['ts'])

        for kpis in (failed['current'], failed['cumulative']):
            self.assertEqual(1, kpis['b']['fail'])

    def test_max_concurrency(self):
        mock = MockReader()
        # data format: t_stamp, label, conc, r_time, con_time, latency, r_code, error, trname, byte_count
        mock.data.append((1, "a", 1, 1, 1, 1, 200, None, '', 0))
        mock.data.append((1, "b", 3, 2, 2, 2, 200, None, '', 0))
        mock.data.append((1, "c", 2, 4, 4, 4, 200, None, '', 0))

        data_point = list(mock.datapoints(True))[0]
        self.assertEqual(3, data_point[DataPoint.CURRENT][''][KPISet.CONCURRENCY])
        self.assertEqual(3, data_point[DataPoint.CUMULATIVE][''][KPISet.CONCURRENCY])

    def test_sample_ignores(self):
        mock = MockReader()
        mock.ignored_labels = ["ignore"]
        mock.buffer_scale_idx = '100.0'
        mock.data.append((1, "ignore", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((2, "ignore1", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((2, "ignore2", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((3, "not-ignore", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((3, "not-ignore", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((4, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((4, "", 1, r(), r(), r(), 200, None, '', 0))

        for point in mock.datapoints(True):
            self.assertNotIn("ignore", point[DataPoint.CUMULATIVE].keys())
            self.assertNotIn("ignore1", point[DataPoint.CUMULATIVE].keys())
            self.assertNotIn("ignore2", point[DataPoint.CUMULATIVE].keys())

    def test_speed(self):
        obj = self.obj

        mock = MockReader()
        mock.buffer_scale_idx = '100.0'
        obj.add_listener(mock)

        res = {}
        # current measurements shows ~25K samples/sec
        for cnt in (10000, 25000, 50000):
            for a in range(0, cnt):
                sample = (cnt, "", 1, r(1000), r(1000), r(1000), rc(), err(), '', 0)
                mock.data.append(sample)
            before = time.time()
            for point in mock.datapoints():
                pass
            after = time.time()
            res[cnt] = after - before
            ROOT_LOGGER.info("Times: %s", res)

            while mock.results:
                point = mock.results.pop(0)
                overall = point[DataPoint.CURRENT]['']
                self.assertTrue(len(overall[KPISet.PERCENTILES]) > 0)

        for point in mock.datapoints(True):
            pass

    def test_0buffer_scaling(self):
        obj = self.obj

        mock = MockReader()
        obj.add_listener(mock)

        # t_stamp, label, conc, r_time, con_time, latency, r_code, error, trname
        mock.min_buffer_len = 1
        mock.buffer_len = 1
        mock.buffer_multiplier = 1
        mock.buffer_scale_idx = '50.0'

        buffer_len = mock.buffer_len
        for i in range(5):
            mock.data.append((100 + i, "", 1, 2, 2, 2, 200, None, '', 0))
        points = list(mock.datapoints())
        points = list(mock.datapoints())
        self.assertTrue(mock.buffer_len > buffer_len)
        buffer_len = mock.buffer_len

        for i in range(10):
            mock.data.append((200 + i, "", 1, 3, 3, 3, 200, None, '', 0))
        points = list(mock.datapoints())
        points = list(mock.datapoints())
        self.assertTrue(mock.buffer_len > buffer_len)
        buffer_len = mock.buffer_len

        for i in range(20):
            mock.data.append((300 + i, "", 1, 4, 4, 4, 200, None, '', 0))
        points = list(mock.datapoints())
        points = list(mock.datapoints())
        self.assertTrue(mock.buffer_len > buffer_len)
        buffer_len = mock.buffer_len

        for i in range(15):
            mock.data.append((400 + i, "", 1, 1, 1, 1, 200, None, '', 0))
        points = list(mock.datapoints())
        points = list(mock.datapoints())
        self.assertTrue(mock.buffer_len < buffer_len)
        buffer_len = mock.buffer_len

        for i in range(30):
            mock.data.append((500 + i, "", 1, 1, 1, 1, 200, None, '', 0))
        points = list(mock.datapoints())
        points = list(mock.datapoints())
        self.assertTrue(mock.buffer_len < buffer_len)

    def test_json(self):
        obj = self.obj

        mock = MockReader()
        mock.buffer_scale_idx = '100.0'
        mock.data.append((1, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((2, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((2, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((3, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((3, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((4, "", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((4, "", 1, r(), r(), r(), 200, None, '', 0))

        obj.add_listener(mock)

        for point in mock.datapoints(True):
            pass

        for point in mock.results:
            serialized = json.loads(to_json(point))
            rt_keys = serialized["current"][""]["rt"].keys()
            for key in rt_keys:
                rt = float(key)
                self.assertGreaterEqual(rt, 1.0)
                self.assertLessEqual(rt, 2.0)


class TestRespTimesCounterEncode(BZTestCase):
    """
    The encoded response-time histogram (hstRt) is consumed by the BlazeMeter ClickHouse
    ingestion pipeline, which derives all percentiles by merging HdrHistograms. It must be
    a base64 HdrHistogram V2 (compressed) blob, in milliseconds, using the canonical fixed
    config (lowest=1, highest=3_600_000, 3 significant figures) so it merges with cloud
    records and decodes with hdrh (lambdas) / org.HdrHistogram (Dagger).
    """

    def _counter_from_seconds(self, samples_sec):
        counter = RespTimesCounter(1, 1000.0, 3)
        for rt in samples_sec:
            counter.add(rt, 1)
        return counter

    def test_encode_roundtrip_percentiles(self):
        # a spread of response times in seconds
        samples = [((i % 500) + 1) / 1000.0 for i in range(5000)]  # 1ms..500ms
        counter = self._counter_from_seconds(samples)

        encoded = counter.encode()
        self.assertIsInstance(encoded, str)
        # compressed HdrHistogram V2 base64 blobs start with the "HISTF" cookie
        self.assertTrue(encoded.startswith("HISTF"), encoded[:12])

        decoded = HdrHistogram.decode(encoded)
        self.assertEqual(RespTimesCounter.ENCODE_HIGHEST, decoded.highest_trackable_value)
        self.assertEqual(RespTimesCounter.ENCODE_SIGNIFICANT_FIGURES, decoded.significant_figures)
        self.assertEqual(len(samples), decoded.get_total_count())

        ms = sorted(int(round(s * 1000)) for s in samples)

        def raw_pct(p):
            return ms[min(len(ms) - 1, int(round(p / 100.0 * len(ms))) - 1)]

        for p in (50.0, 90.0, 95.0, 99.0):
            got = decoded.get_value_at_percentile(p)
            exp = raw_pct(p)
            # within HdrHistogram's 3-significant-figure (1%) tolerance
            self.assertLessEqual(abs(got - exp) / max(1, exp), 0.02,
                                 "p%s decoded=%s raw=%s" % (p, got, exp))

    def test_encode_clamps_above_highest(self):
        # values beyond one hour (in ms) must be clamped, not dropped
        counter = self._counter_from_seconds([7200.0] * 10)  # 2h
        decoded = HdrHistogram.decode(counter.encode())
        self.assertEqual(10, decoded.get_total_count())
        # recorded at the ceiling bucket (get_max_value is the bucket's highest-equivalent,
        # so it may exceed ENCODE_HIGHEST by one bucket width at 3 significant figures)
        self.assertLessEqual(
            abs(decoded.get_max_value() - RespTimesCounter.ENCODE_HIGHEST) / RespTimesCounter.ENCODE_HIGHEST,
            0.01)

    def test_encode_empty_is_valid(self):
        counter = RespTimesCounter(1, 1000.0, 3)
        decoded = HdrHistogram.decode(counter.encode())
        self.assertEqual(0, decoded.get_total_count())
