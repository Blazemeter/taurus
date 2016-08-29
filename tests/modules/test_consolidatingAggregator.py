from collections import Counter

from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet, FuncKPISet

from tests import BZTestCase, r
from tests.mocks import MockReader


class TestConsolidatingAggregator(BZTestCase):
    def test_mock(self):
        # check mock reader
        reader = self.get_reader()
        reader.buffer_scale_idx = '90.0'
        first = list(reader.datapoints())
        second = list(reader.datapoints(True))
        self.assertEquals([1, 2, 3, 4], [x[DataPoint.TIMESTAMP] for x in first])
        self.assertEquals([5, 6], [x[DataPoint.TIMESTAMP] for x in second])
        for point in first + second:
            val = point[DataPoint.CURRENT]['']
            # self.assertGreater(val[KPISet.AVG_RESP_TIME], 0)

    def test_merging(self):
        dst = DataPoint(0)
        src = DataPoint(0)
        src[DataPoint.CUMULATIVE].get('', KPISet())
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

    def test_two_executions(self):
        # check consolidator
        obj = ConsolidatingAggregator()
        obj.track_percentiles = [0, 50, 100]
        obj.prepare()
        underling1 = self.get_reader()
        underling2 = self.get_reader()
        obj.add_underling(underling1)
        obj.add_underling(underling2)

        cnt = 0
        for _ in range(1, 10):
            for point in obj.datapoints():
                overall = point[DataPoint.CURRENT]['']
                self.assertEquals(2, overall[KPISet.CONCURRENCY])
                self.assertGreater(overall[KPISet.PERCENTILES]["100.0"], 0)
                self.assertGreater(overall[KPISet.AVG_RESP_TIME], 0)
                cnt += 1

        self.assertEquals(2, cnt)

    def get_reader(self, offset=0):
        mock = MockReader()
        mock.data.append((1 + offset, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((2 + offset, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((2 + offset, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((3 + offset, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((3 + offset, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((4 + offset, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((4 + offset, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((6 + offset, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((6 + offset, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((6 + offset, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((5 + offset, "", 1, r(), r(), r(), 200, None, ''))
        return mock


class TestFuncKPIAggregation(BZTestCase):
    def get_reader(self, offset=0):
        items = [
            {"start_time": 1 + offset, "label": "test1", "duration": r(), "status": "PASSED"},
            {"start_time": 2 + offset, "label": "test2", "duration": r(), "status": "FAILED"},
            {"start_time": 2 + offset, "label": "test3", "duration": r(), "status": "BROKEN"},
            {"start_time": 3 + offset, "label": "test1", "duration": r(), "status": "PASSED"},
            {"start_time": 3 + offset, "label": "test3", "duration": r(), "status": "SKIPPED"},
            {"start_time": 4 + offset, "label": "test2", "duration": r(), "status": "PASSED"},
            {"start_time": 4 + offset, "label": "test1", "duration": r(), "status": "BROKEN"},
            {"start_time": 6 + offset, "label": "test1", "duration": r(), "status": "SKIPPED"},
            {"start_time": 6 + offset, "label": "test3", "duration": r(), "status": "FAILED"},
            {"start_time": 6 + offset, "label": "test2", "duration": r(), "status": "PASSED"},
            {"start_time": 5 + offset, "label": "test1", "duration": r(), "status": "BROKEN"},
        ]
        mock = MockReader()
        mock.func_mode = True
        for item in items:
            mock.data.append((item["start_time"], item["label"], item))
        return mock

    def test_mock(self):
        # check mock reader
        reader = self.get_reader()
        first = list(reader.datapoints())
        second = list(reader.datapoints(True))
        self.assertEquals([1, 2, 3, 4], [x[DataPoint.TIMESTAMP] for x in first])
        self.assertEquals([5, 6], [x[DataPoint.TIMESTAMP] for x in second])
        for point in first + second:
            val = point[DataPoint.CURRENT]['']
            self.assertGreater(val[FuncKPISet.TESTS_COUNT], 0)

    def test_merging(self):
        dst = DataPoint(0, func_mode=True)
        src = DataPoint(0, func_mode=True)

        src[DataPoint.CUMULATIVE].get('', FuncKPISet())
        src[DataPoint.CUMULATIVE][''][FuncKPISet.TESTS_COUNT] = 10
        src[DataPoint.CUMULATIVE][''][FuncKPISet.TEST_STATUSES] = Counter({"BROKEN": 2, "PASSED": 8})
        src[DataPoint.CUMULATIVE][''][FuncKPISet.TESTS] = [3]

        dst[DataPoint.CUMULATIVE].get('', FuncKPISet())
        dst[DataPoint.CUMULATIVE][''][FuncKPISet.TESTS_COUNT] = 2
        dst[DataPoint.CUMULATIVE][''][FuncKPISet.TEST_STATUSES] = Counter({"BROKEN": 0, "PASSED": 2})
        dst[DataPoint.CUMULATIVE][''][FuncKPISet.TESTS] = [1, 2]

        dst.merge_point(src)

        cumul = dst[DataPoint.CUMULATIVE]['']

        self.assertEquals(12, cumul[FuncKPISet.TESTS_COUNT])
        self.assertEquals(2, cumul[FuncKPISet.TEST_STATUSES]["BROKEN"])
        self.assertEquals(10, cumul[FuncKPISet.TEST_STATUSES]["PASSED"])
        self.assertEquals([1, 2, 3], cumul[FuncKPISet.TESTS])
