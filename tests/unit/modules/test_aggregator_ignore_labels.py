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
        raw_samples = [
            (1, "ignore", 1, 10, 1, 1, 200, None, '', 0),
            (1, "ignore", 1, 20, 1, 1, 200, None, '', 0),
            (1, "not-ignore", 1, 100, 1, 1, 200, None, '', 0),
            (1, "not-ignore", 1, 200, 1, 1, 200, None, '', 0),
            (1, "not-ignore", 1, 300, 1, 1, 200, None, '', 0),
            (1, "not-ignore", 1, 400, 1, 1, 200, None, '', 0),
            (1, "ignore", 1, 30, 1, 1, 200, None, '', 0),
            (1, "not-ignore", 1, 500, 1, 1, 200, None, '', 0),
        ]
        datapoint = DataPoint(1, self.reader.track_percentiles)
        self.reader._ResultsReader__aggregate_current(datapoint, [s[1:] for s in raw_samples])
        overall = datapoint[DataPoint.CURRENT]['']
        percentiles = overall[KPISet.PERCENTILES]
        # Only the 'not-ignore' samples should be used for percentiles
        used_samples = [s[3] for s in raw_samples if s[1] != "ignore"]
        ignored_samples = [s[3] for s in raw_samples if s[1] == "ignore"]
        self.assertEqual(len(used_samples), 5)
        self.assertEqual(len(ignored_samples), 3)
        # Check that ignored samples are not in the percentiles calculation
        self.assertTrue(all(v not in percentiles.values() for v in ignored_samples))
        # Check percentiles keys
        for p in ["50.0", "90.0", "100.0"]:
            self.assertIn(p, percentiles)
        # Calculate expected percentiles manually
        sorted_samples = sorted(used_samples)
        def percentile(sorted_list, perc):
            k = (len(sorted_list)-1) * (perc/100.0)
            f = int(k)
            c = min(f+1, len(sorted_list)-1)
            if f == c:
                return sorted_list[int(k)]
            d0 = sorted_list[f] * (c-k)
            d1 = sorted_list[c] * (k-f)
            return d0 + d1
        self.assertAlmostEqual(percentiles["50.0"], percentile(sorted_samples, 50), delta=100)
        self.assertAlmostEqual(percentiles["90.0"], percentile(sorted_samples, 90), delta=100)
        self.assertAlmostEqual(percentiles["100.0"], percentile(sorted_samples, 100), delta=100)
        # All percentiles should be >= min of used_samples
        self.assertTrue(all(v >= min(used_samples) for v in percentiles.values()))
        # All percentiles should be <= max of used_samples
        self.assertTrue(all(v <= max(used_samples) for v in percentiles.values()))

if __name__ == "__main__":
    unittest.main()
