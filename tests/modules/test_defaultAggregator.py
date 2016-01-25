""" test """
import time
import logging

from bzt.modules.aggregator import ResultsReader, DataPoint, KPISet
from tests import BZTestCase, r, rc, err
from tests.mocks import MockReader


class TestDefaultAggregator(BZTestCase):
    def setUp(self):
        super(TestDefaultAggregator, self).setUp()
        self.obj = ResultsReader()
        self.obj.track_percentiles = [25, 50, 75, 80, 90, 95, 99, 99.9, 100]
        self.obj.buffer_len = 1

    def test_1(self):
        obj = self.obj

        mock = MockReader()
        mock.data.append((1, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((2, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((2, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((3, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((3, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((4, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((4, "", 1, r(), r(), r(), 200, None, ''))

        obj.add_listener(mock)

        for point in mock.datapoints():
            self.assertNotEquals(0, point[DataPoint.CUMULATIVE][''][KPISet.CONCURRENCY])

        mock.data.append((2, "", 1, r(), r(), r(), 200, None, ''))
        mock.data.append((2, "", 1, r(), r(), r(), 200, None, ''))

        for point in mock.datapoints():
            pass

        for point in mock.datapoints(True):
            pass

        for point in mock.results:
            overall = point[DataPoint.CURRENT]['']
            self.assertTrue(len(overall[KPISet.PERCENTILES]) > 0)

    def test_speed(self):
        obj = self.obj

        mock = MockReader()

        obj.add_listener(mock)

        res = {}
        # current measurements shows ~25K samples/sec
        for cnt in (10, 100, 1000, 10000, 25000, 40000, 50000):
            for a in range(0, cnt):
                sample = (cnt, "", 1, r(1000), r(1000), r(1000), rc(), err(), '')
                mock.data.append(sample)
            before = time.time()
            for point in mock.datapoints():
                pass
            after = time.time()
            res[cnt] = after - before
            logging.info("Times: %s", res)

            while mock.results:
                point = mock.results.pop(0)
                overall = point[DataPoint.CURRENT]['']
                self.assertTrue(len(overall[KPISet.PERCENTILES]) > 0)

        for point in mock.datapoints(True):
            pass
