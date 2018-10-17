import math
from random import random

from apiritif import random_string
from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet, AggregatorListener
from bzt.utils import to_json
from tests import BZTestCase, r
from tests.mocks import MockReader


def get_success_reader(offset=0):
    mock = MockReader()
    mock.data.append((1 + offset, "", 1, r(), r(), r(), 200, None, '', 0))
    mock.data.append((2 + offset, "", 1, r(), r(), r(), 200, None, '', 0))
    mock.data.append((2 + offset, "", 1, r(), r(), r(), 200, None, '', 0))
    mock.data.append((3 + offset, "", 1, r(), r(), r(), 200, None, '', 0))
    mock.data.append((3 + offset, "", 1, r(), r(), r(), 200, None, '', 0))
    mock.data.append((4 + offset, "", 1, r(), r(), r(), 200, None, '', 0))
    mock.data.append((4 + offset, "", 1, r(), r(), r(), 200, None, '', 0))
    mock.data.append((6 + offset, "", 1, r(), r(), r(), 200, None, '', 0))
    mock.data.append((6 + offset, "", 1, r(), r(), r(), 200, None, '', 0))
    mock.data.append((6 + offset, "", 1, r(), r(), r(), 200, None, '', 0))
    mock.data.append((5 + offset, "", 1, r(), r(), r(), 200, None, '', 0))
    return mock


def get_fail_reader(offset=0):
    mock = MockReader()
    mock.data.append((1 + offset, "first", 1, r(), r(), r(), 200, 'FAILx3', '', 0))
    mock.data.append((2 + offset, "first", 1, r(), r(), r(), 200, 'FAILx1', '', 0))
    mock.data.append((5 + offset, "first", 1, r(), r(), r(), 200, None, '', 0))
    mock.data.append((7 + offset, "second", 1, r(), r(), r(), 200, 'FAILx3', '', 0))
    mock.data.append((3 + offset, "first", 1, r(), r(), r(), 200, 'FAILx3', '', 0))
    mock.data.append((6 + offset, "second", 1, r(), r(), r(), 200, 'unique FAIL', '', 0))
    return mock


def get_fail_reader_alot(offset=0):
    mock = MockReader()
    for x in range(2, 100):
        rnd = random() * math.pow(x, 2)
        mock.data.append((x + offset, "first", 1, r(), r(), r(), 200, (random_string(1 + int(rnd))), '', 0))
    return mock


class TestTools(BZTestCase):
    def test_mock(self):
        # check mock reader
        reader = get_success_reader()
        reader.buffer_scale_idx = '90.0'
        first = list(reader.datapoints())
        second = list(reader.datapoints(True))
        self.assertEquals([1, 2, 3, 4], [x[DataPoint.TIMESTAMP] for x in first])
        self.assertEquals([5, 6], [x[DataPoint.TIMESTAMP] for x in second])
        for point in first + second:
            self.assertIn("", point[DataPoint.CURRENT])

    def test_merging(self):
        dst = DataPoint(0)
        src = DataPoint(0)
        src[DataPoint.CUMULATIVE].setdefault('', KPISet())
        src[DataPoint.CUMULATIVE][''].sum_rt = 0.5

        src[DataPoint.CUMULATIVE][''][KPISet.SAMPLE_COUNT] = 1
        dst.merge_point(src)
        self.assertEquals(0.5, dst[DataPoint.CUMULATIVE][''].sum_rt)
        self.assertEquals(0.5, dst[DataPoint.CUMULATIVE][''][KPISet.AVG_RESP_TIME])

        src[DataPoint.CUMULATIVE][''][KPISet.SAMPLE_COUNT] = 3
        dst.merge_point(src)
        self.assertEquals(4, dst[DataPoint.CUMULATIVE][''][KPISet.SAMPLE_COUNT])
        self.assertEquals(1, dst[DataPoint.CUMULATIVE][''].sum_rt)
        self.assertEquals(0.25, dst[DataPoint.CUMULATIVE][''][KPISet.AVG_RESP_TIME])

        src[DataPoint.CUMULATIVE][''][KPISet.SAMPLE_COUNT] = 6
        dst.merge_point(src)
        self.assertEquals(10, dst[DataPoint.CUMULATIVE][''][KPISet.SAMPLE_COUNT])
        self.assertEquals(1.5, dst[DataPoint.CUMULATIVE][''].sum_rt)
        self.assertEquals(0.15, dst[DataPoint.CUMULATIVE][''][KPISet.AVG_RESP_TIME])


class TestConsolidatingAggregator(BZTestCase):
    def setUp(self):
        super(TestConsolidatingAggregator, self).setUp()
        self.obj = ConsolidatingAggregator()

    def test_two_executions(self):
        self.obj.track_percentiles = [0, 50, 100]
        self.obj.prepare()
        underling1 = get_success_reader()
        underling2 = get_success_reader()
        self.obj.add_underling(underling1)
        self.obj.add_underling(underling2)

        cnt = 0
        for _ in range(1, 10):
            for point in self.obj.datapoints():
                overall = point[DataPoint.CURRENT]['']
                self.assertEquals(2, overall[KPISet.CONCURRENCY])
                self.assertGreater(overall[KPISet.PERCENTILES]["100.0"], 0)
                self.assertGreater(overall[KPISet.AVG_RESP_TIME], 0)
                cnt += 1

        self.assertEquals(2, cnt)

    def test_errors_cumulative(self):
        self.obj.track_percentiles = [50]
        self.obj.prepare()
        reader = get_fail_reader()
        self.obj.add_underling(reader)
        self.obj.shutdown()
        self.obj.post_process()
        cum_dict = self.obj.underlings[0].cumulative
        first_err_ids = [id(err) for err in cum_dict['first']['errors']]
        second_err_ids = [id(err) for err in cum_dict['second']['errors']]
        total_err_ids = [id(err) for err in cum_dict['']['errors']]
        all_ids = first_err_ids + second_err_ids + total_err_ids
        self.assertEqual(len(all_ids), len(set(all_ids)))
        for label in cum_dict:
            data = cum_dict[label]
            total_errors_count = sum(err['cnt'] for err in data['errors'])
            self.assertEqual(data['fail'], total_errors_count)

    def test_errors_variety(self):
        self.obj.track_percentiles = [50]
        self.obj.prepare()
        reader1 = get_fail_reader()
        reader2 = get_fail_reader_alot()
        self.obj.max_error_count = 9
        self.obj.add_underling(reader1)
        self.obj.add_underling(reader2)
        self.obj.shutdown()
        self.obj.post_process()
        cum_dict = self.obj.cumulative
        self.assertLessEqual(len(cum_dict['']['errors']), self.obj.max_error_count)

    def test_uniq_errors(self):
        self.obj.track_percentiles = [50]
        self.obj.prepare()
        reader = get_fail_reader()
        self.obj.max_error_count = 9
        self.obj.add_underling(reader)
        self.obj.shutdown()
        self.obj.post_process()
        cum_dict = self.obj.cumulative
        self.assertEqual(len(cum_dict['']['errors']), 3)

    def test_set_rtimes_len(self):
        self.obj.settings['rtimes-len'] = 10000
        self.obj.prepare()
        reader = get_fail_reader()
        self.obj.add_underling(reader)
        listener = MockListener()
        self.obj.add_listener(listener)
        self.obj.check()
        for dp in listener.results:
            for kpiset in dp['cumulative'].values():
                self.assertEqual(10000, kpiset[KPISet.RESP_TIMES].high)
            for kpiset in dp['current'].values():
                self.assertEqual(10000, kpiset[KPISet.RESP_TIMES].high)

    def test_inf_values(self):
        self.obj.settings['max-buffer-len'] = "inf"
        self.obj.prepare()
        self.assertEqual(self.obj.max_buffer_len, float("inf"))

    def test_datapoint_to_json(self):
        self.obj.track_percentiles = [0.0, 50.0, 95.0, 99.0, 100.0]
        self.obj.prepare()
        self.obj.add_underling(get_success_reader())
        for point in self.obj.datapoints():
            self.obj.log.info(to_json(point))

    def test_negative_response_time_scaling_crash(self):
        self.obj.track_percentiles = [0.0, 50.0, 95.0, 99.0, 100.0]
        self.obj.prepare()

        self.sniff_log(self.obj.log)

        mock = MockReader()
        mock.data.append((1, "first", 1, -r(), r(), r(), 200, 'FAILx3', '', 0))
        mock.data.append((2, "first", 1, -r(), r(), r(), 200, 'FAILx1', '', 0))
        mock.data.append((5, "first", 1, -r(), r(), r(), 200, None, '', 0))
        mock.data.append((7, "second", 1, -r(), r(), r(), 200, 'FAILx3', '', 0))
        mock.data.append((3, "first", 1, -r(), r(), r(), 200, 'FAILx3', '', 0))
        mock.data.append((6, "second", 1, -r(), r(), r(), 200, 'unique FAIL', '', 0))

        self.obj.add_underling(mock)

        self.obj.check()
        for point in self.obj.datapoints():
            self.obj.log.info(to_json(point))

        self.assertIn("Negative response time reported", self.log_recorder.warn_buff.getvalue())


class MockListener(AggregatorListener):
    def __init__(self):
        super(MockListener, self).__init__()
        self.results = []

    def aggregated_second(self, data):
        self.results.append(data)
