import unittest
from bzt.modules.aggregator import ResultsReader, DataPoint, KPISet

class TestResultsReaderIgnoreLabelsPercentiles(unittest.TestCase):
    def setUp(self):
        self.reader = ResultsReader()
        self.reader.track_percentiles = [50, 90, 100]
        self.reader.buffer_scale_idx = '100.0'
        self.reader.buffer_len = 1
        self.reader.ignored_labels = ["ignore"]

    def test_percentiles_exclude_ignored_labels(self):
        # t_stamp, label, conc, r_time, con_time, latency, r_code, error, trname, byte_count
        samples = [
            (1, "ignore", 1, 10, 1, 1, 200, None, '', 0),
            (1, "not-ignore", 1, 100, 1, 1, 200, None, '', 0),
            (1, "not-ignore", 1, 200, 1, 1, 200, None, '', 0),
        ]
        datapoint = DataPoint(1, self.reader.track_percentiles)
        self.reader._ResultsReader__aggregate_current(datapoint, samples)
        overall = datapoint[DataPoint.CURRENT]['']
        # Only the two 'not-ignore' samples should be used for percentiles
        percentiles = overall[KPISet.PERCENTILES]
        self.assertIn('50.0', percentiles)
        self.assertIn('90.0', percentiles)
        self.assertIn('100.0', percentiles)
        # 50th percentile should be 150 (middle of 100 and 200)
        self.assertAlmostEqual(percentiles['50.0'], 150, delta=1)
        # 100th percentile should be 200
        self.assertAlmostEqual(percentiles['100.0'], 200, delta=1)
        # 10 should not affect the percentiles
        self.assertTrue(all(v >= 100 for v in percentiles.values()))

if __name__ == "__main__":
    unittest.main()
