from bzt.hdr_histogram import HdrHistogram
from tests import BZTestCase


class TestHdrHistogram(BZTestCase):
    def test_empty(self):
        hdr = HdrHistogram(1, 1 * 60 * 60 * 1000, 3)
        self.assertEqual(hdr.get_min_value(), 0)
        self.assertEqual(hdr.get_max_value(), 0)
        self.assertEqual(hdr.get_mean_value(), 0)
        self.assertEqual(hdr.get_stddev(), 0)

    def test_large_numbers(self):
        histogram = HdrHistogram(20000000, 100000000, 5)
        histogram.record_value(100000000)
        histogram.record_value(20000000)
        histogram.record_value(30000000)
        self.assertTrue(histogram.values_are_equivalent(20000000, histogram.get_value_at_percentile(50.0)))
        self.assertTrue(histogram.values_are_equivalent(30000000, histogram.get_value_at_percentile(83.33)))
        self.assertTrue(histogram.values_are_equivalent(100000000, histogram.get_value_at_percentile(83.34)))
        self.assertTrue(histogram.values_are_equivalent(100000000, histogram.get_value_at_percentile(99.0)))
    
    def test_record_value(self):
        histogram = HdrHistogram(1, 3600 * 1000 * 1000, 3)
        histogram.record_value(4)
        self.assertEqual(histogram.get_count_at_value(4), 1)
        self.assertEqual(histogram.get_total_count(), 1)
    
    def test_highest_equavalent_value(self):
        histogram = HdrHistogram(1, 3600 * 1000 * 1000, 3)
        self.assertEqual(8183 * 1024 + 1023, histogram.get_highest_equivalent_value(8180 * 1024))
        self.assertEqual(8191 * 1024 + 1023, histogram.get_highest_equivalent_value(8191 * 1024))
        self.assertEqual(8199 * 1024 + 1023, histogram.get_highest_equivalent_value(8193 * 1024))
        self.assertEqual(9999 * 1024 + 1023, histogram.get_highest_equivalent_value(9995 * 1024))
        self.assertEqual(10007 * 1024 + 1023, histogram.get_highest_equivalent_value(10007 * 1024))
        self.assertEqual(10015 * 1024 + 1023, histogram.get_highest_equivalent_value(10008 * 1024))

    def test_scaled_highest_equiv_value(self):
        histogram = HdrHistogram(1, 3600 * 1000 * 1000, 3)
        self.assertEqual(histogram.get_highest_equivalent_value(8180), 8183)
        self.assertEqual(histogram.get_highest_equivalent_value(8191), 8191)
        self.assertEqual(histogram.get_highest_equivalent_value(8193), 8199)
        self.assertEqual(histogram.get_highest_equivalent_value(9995), 9999)
        self.assertEqual(histogram.get_highest_equivalent_value(10007), 10007)
        self.assertEqual(histogram.get_highest_equivalent_value(10008), 10015)

    def load_histogram(self):
        histogram = HdrHistogram(1, 3600 * 1000 * 1000, 3)
        # record this value with a count of 10,000
        histogram.record_value(1000, 10000)
        histogram.record_value(100000000)
        return histogram

    def load_corrected_histogram(self):
        histogram = HdrHistogram(1, 3600 * 1000 * 1000, 3)
        # record this value with a count of 10,000
        histogram.record_corrected_value(1000, 10000, 10000)
        histogram.record_corrected_value(100000000, 10000)
        return histogram

    def check_percentile(self, hist, percentile, value, variation):
        value_at = hist.get_value_at_percentile(percentile)
        self.assertLess(abs(value_at - value), value * variation)

    def check_hist_percentiles(self, hist, total_count, perc_value_list):
        for pair in perc_value_list:
            self.check_percentile(hist, pair[0], pair[1], 0.001)
        self.assertEqual(hist.get_total_count(), total_count)
        self.assertTrue(hist.values_are_equivalent(hist.get_min_value(), 1000.0))
        self.assertTrue(hist.values_are_equivalent(hist.get_max_value(), 100000000.0))

    def test_percentiles(self):
        self.check_hist_percentiles(self.load_histogram(),
                                    10001,
                                   ((30.0, 1000.0),
                                    (99.0, 1000.0),
                                    (99.99, 1000.0),
                                    (99.999, 100000000.0),
                                    (100.0, 100000000.0)))
        self.check_hist_percentiles(self.load_corrected_histogram(),
                                    20000,
                                   ((30.0, 1000.0),
                                    (50.0, 1000.0),
                                    (75.0, 50000000.0),
                                    (90.0, 80000000.0),
                                    (99.0, 98000000.0),
                                    (99.999, 100000000.0),
                                    (100.0, 100000000.0)))

    def test_recorded_iterator(self):
        hist = self.load_histogram()
        index = 0
        for item in hist.get_recorded_iterator():
            count_added_in_this_bucket = item.count_added_in_this_iter_step
            if index == 0:
                self.assertEqual(count_added_in_this_bucket, 10000)
            else:
                self.assertEqual(count_added_in_this_bucket, 1)
            index += 1
        self.assertEqual(index, 2)

        hist = self.load_corrected_histogram()
        index = 0
        total_added_count = 0
        for item in hist.get_recorded_iterator():
            count_added_in_this_bucket = item.count_added_in_this_iter_step
            if index == 0:
                self.assertEqual(count_added_in_this_bucket, 10000)

            self.assertNotEqual(item.count_at_value_iterated_to, 0)
            total_added_count += count_added_in_this_bucket
            index += 1
        self.assertEqual(total_added_count, 20000)
        self.assertEqual(total_added_count, hist.get_total_count())

    def test_reset(self):
        histogram = self.load_histogram()
        histogram.reset()
        self.assertEqual(histogram.get_total_count(), 0)
        self.assertEqual(histogram.get_value_at_percentile(99.99), 0)

    def test_invalid_significant_figures(self):
        self.assertRaises(ValueError, lambda: HdrHistogram(1, 100, -1))
        self.assertRaises(ValueError, lambda: HdrHistogram(1, 100, 6))

    def test_out_of_range_values(self):
        histogram = HdrHistogram(1, 1000, 4)
        self.assertTrue(histogram.record_value(32767))
        self.assertFalse(histogram.record_value(32768))

    def test_mean_stddev(self):
        # fill up a histogram with the values in the list
        histogram = HdrHistogram(1, 3600 * 1000 * 1000, 3)
        for value in [1000, 1000, 3000, 3000]:
            histogram.record_value(value)
        self.assertEqual(histogram.get_mean_value(), 2000.5)
        self.assertEqual(histogram.get_stddev(), 1000.5)

    def test_get_value_at_percentile(self):
        histogram = HdrHistogram(1, 3600 * 1000 * 1000, 3)
        histogram.record_value(1)
        histogram.record_value(2)
        self.assertEqual(histogram.get_value_at_percentile(50.0), 1)
        self.assertEqual(histogram.get_value_at_percentile(50.00000000000001), 1)
        histogram.record_value(2)
        histogram.record_value(2)
        histogram.record_value(2)
        self.assertEqual(histogram.get_value_at_percentile(30), 2)

    # TODO: check HdrHistogram.add
    # TODO: check HdrHistogram.get_percentile_to_value_dict

