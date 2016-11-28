from bzt.modules.aggregator import ConsolidatingAggregator, DataPoint, KPISet

from tests import BZTestCase, r
from tests.mocks import MockReader


class TestConsolidatingAggregator(BZTestCase):
    def test_mock(self):
        # check mock reader
        reader = self.get_success_reader()
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
        underling1 = self.get_success_reader()
        underling2 = self.get_success_reader()
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

    @staticmethod
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

    @staticmethod
    def get_fail_reader(offset=0):
        mock = MockReader()
        mock.data.append((1 + offset, "first", 1, r(), r(), r(), 200, 'FAILx3', '', 0))
        mock.data.append((2 + offset, "first", 1, r(), r(), r(), 200, 'FAILx1', '', 0))
        mock.data.append((5 + offset, "first", 1, r(), r(), r(), 200, None, '', 0))
        mock.data.append((7 + offset, "second", 1, r(), r(), r(), 200, 'FAILx3', '', 0))
        mock.data.append((3 + offset, "first", 1, r(), r(), r(), 200, 'FAILx3', '', 0))
        mock.data.append((6 + offset, "second", 1, r(), r(), r(), 200, 'unique FAIL', '', 0))
        return mock

    def test_errors_cumulative(self):
        aggregator = ConsolidatingAggregator()
        aggregator.track_percentiles = [50]
        aggregator.prepare()
        reader = self.get_fail_reader()
        aggregator.add_underling(reader)
        aggregator.shutdown()
        aggregator.post_process()
        cum_dict = aggregator.underlings[0].cumulative
        first_err_ids = [id(err) for err in cum_dict['first']['errors']]
        second_err_ids = [id(err) for err in cum_dict['second']['errors']]
        total_err_ids = [id(err) for err in cum_dict['']['errors']]
        all_ids = first_err_ids + second_err_ids + total_err_ids
        self.assertEqual(len(all_ids), len(set(all_ids)))
        for label in cum_dict:
            data = cum_dict[label]
            total_errors_count = sum(err['cnt'] for err in data['errors'])
            self.assertEqual(data['fail'], total_errors_count)

    def test_kpiset_merge_many_rt(self):
        vals = {49.741: 2, 51.987: 2, 38.45: 1, 2.868: 1, 34.153: 1, 28.919: 1, 35.832: 1, 33.556: 1, 38.926: 1,
                19.667: 1, 36.764: 1, 10.528: 1, 47.602: 1, 37.4: 1, 13.485: 1, 29.013: 1, 22.468: 1, 59.543: 1,
                59.403: 1, 4.081: 1, 47.321: 1, 7.702: 1, 51.724: 1, 28.102: 1, 19.872: 1, 6.969: 1, 33.246: 1,
                4.535: 1, 31.815: 1, 14.709: 1, 13.648: 1, 16.654: 1, 10.23: 1, 6.667: 1, 29.819: 1, 41.737: 1,
                22.733: 1, 6.549: 1, 44.199: 1, 43.82: 1, 44.241: 1, 54.698: 1, 33.869: 1, 44.601: 1, 5.823: 1,
                52.643: 1, 5.435: 1, 50.51: 1, 17.696: 1, 54.348: 1, 20.669: 1, 10.095: 1, 54.186: 1, 28.933: 1,
                49.548: 1, 36.524: 1, 16.947: 1, 13.528: 1, 36.487: 1, 19.711: 1, 3.399: 1, 57.95: 1, 10.675: 1,
                35.669: 1, 53.353: 1, 43.086: 1, 10.495: 1, 53.751: 1, 29.003: 1, 47.155: 1, 42.534: 1, 32.639: 1,
                59.812: 1, 58.03: 1, 37.32: 1, 53.152: 1, 28.214: 1, 21.864: 1, 12.967: 1, 25.931: 1, 34.01: 1,
                21.283: 1, 23.62: 1, 30.573: 1, 13.249: 1, 13.601: 1, 54.766: 1, 8.606: 1, 34.867: 1, 49.813: 1,
                44.827: 1, 50.455: 1, 59.48: 1, 41.508: 1, 58.506: 1, 17.079: 1, 16.143: 1, 42.219: 1, 7.498: 1,
                13.391: 1, 14.648: 1, 59.182: 1, 16.421: 1, 12.653: 1, 52.52: 1, 0.616: 1, 56.37: 1, 31.574: 1,
                59.321: 1, 18.602: 1, 21.088: 1, 36.993: 1, 39.387: 1, 46.704: 1, 30.366: 1, 43.26: 1, 36.173: 1,
                29.144: 1, 50.256: 1, 27.666: 1, 46.816: 1, 11.181: 1, 22.797: 1, 2.457: 1, 24.294: 1, 45.747: 1,
                43.65: 1, 12.901: 1, 46.15: 1, 53.113: 1, 46.304: 1, 3.058: 1, 6.555: 1, 19.603: 1, 40.484: 1,
                42.181: 1, 55.761: 1, 22.756: 1, 38.252: 1, 16.52: 1, 51.509: 1, 43.141: 1, 56.232: 1, 8.274: 1,
                31.302: 1, 27.471: 1, 50.722: 1, 18.326: 1, 45.501: 1, 34.299: 1, 13.17: 1, 9.456: 1, 25.729: 1,
                12.499: 1, 29.197: 1, 30.917: 1, 23.555: 1, 5.774: 1, 54.406: 1, 8.066: 1, 42.169: 1, 58.632: 1,
                43.848: 1, 52.695: 1, 15.68: 1, 14.712: 1, 8.202: 1, 49.886: 1, 22.345: 1, 57.995: 1, 58.271: 1,
                44.904: 1, 33.234: 1, 54.261: 1, 24.216: 1, 42.697: 1, 35.411: 1, 27.98: 1, 5.378: 1, 54.856: 1,
                13.872: 1, 40.998: 1, 21.498: 1, 10.129: 1, 28.918: 1, 39.825: 1, 21.587: 1, 13.768: 1, 57.244: 1,
                13.644: 1, 18.399: 1, 49.725: 1, 58.818: 1, 31.064: 1, 5.394: 1, 3.023: 1, 24.263: 1, 16.394: 1,
                55.374: 1, 58.684: 1, 12.797: 1, 47.145: 1, 5.662: 1, 40.202: 1, 22.245: 1, 32.617: 1, 49.143: 1,
                46.995: 1, 44.841: 1, 49.934: 1, 44.181: 1, 15.831: 1, 19.321: 1, 30.926: 1, 42.222: 1, 23.782: 1,
                5.31: 1, 56.38: 1, 55.91: 1, 22.759: 1, 51.193: 1, 48.283: 1, 6.391: 1, 4.865: 1, 21.468: 1, 50.216: 1,
                45.994: 1, 3.404: 1, 42.17: 1, 44.457: 1, 54.617: 1, 57.42: 1, 13.005: 1, 47.322: 1, 30.077: 1,
                39.133: 1, 29.524: 1, 39.013: 1, 33.28: 1, 31.467: 1, 5.263: 1, 6.034: 1, 45.482: 1, 36.257: 1,
                41.235: 1, 29.45: 1, 11.04: 1, 52.073: 1, 53.063: 1, 51.411: 1, 19.108: 1, 30.389: 1, 53.999: 1,
                35.439: 1, 22.122: 1, 44.262: 1, 4.964: 1, 31.56: 1, 32.3: 1, 31.97: 1, 48.05: 1, 0.967: 1, 17.884: 1,
                0.069: 1, 57.976: 1, 2.616: 1, 58.939: 1, 38.939: 1, 17.692: 1, 44.36: 1, 16.706: 1, 41.613: 1, 4.53: 1,
                11.858: 1, 17.345: 1, 39.078: 1, 59.218: 1, 51.541: 1, 32.535: 1, 3.924: 1, 15.999: 1, 9.751: 1,
                9.314: 1, 48.656: 1, 33.899: 1, 1.206: 1, 20.639: 1, 33.052: 1, 3.118: 1, 9.105: 1, 17.665: 1, 7.471: 1,
                12.099: 1, 35.379: 1, 5.482: 1, 22.127: 1, 17.318: 1, 6.339: 1, 12.548: 1, 51.62: 1, 37.948: 1,
                48.884: 1, 29.772: 1, 40.744: 1, 36.421: 1, 26.866: 1, 47.026: 1, 52.112: 1, 58.472: 1, 32.586: 1,
                7.729: 1, 58.254: 1, 10.215: 1, 45.512: 1, 42.682: 1, 21.194: 1, 39.06: 1, 11.831: 1, 41.882: 1,
                49.537: 1, 47.012: 1, 43.472: 1, 23.824: 1, 25.535: 1}

        src = KPISet()
        src[KPISet.RESP_TIMES].update(vals)
        dst = KPISet()
        dst.rt_dist_maxlen = 100
        for _ in range(0, 100):
            dst.merge_kpis(src)
            dst.compact_times()
            self.assertEqual(100, len(dst[KPISet.RESP_TIMES]))
