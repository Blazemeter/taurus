"""
A pure python version of the HdrHistogram

Ported from
https://github.com/HdrHistogram/HdrHistogram_py by Alec Hothan

The important edits:
- encoding/decoding routines were thrown away, as they rely on C extensions, which make code hard to distribute
- some iterators and histogram methods were omitted to minimize the amount of code

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""
from __future__ import division

from abc import abstractmethod
import math
import sys

import numpy


def get_bucket_count(value, subb_count, unit_mag):
    smallest_untrackable_value = subb_count << unit_mag
    buckets_needed = 1
    while smallest_untrackable_value <= value:
        if smallest_untrackable_value > sys.maxsize // 2:
            return buckets_needed + 1
        smallest_untrackable_value <<= 1
        buckets_needed += 1
    return buckets_needed


class HdrConcurrentModificationException(Exception):
    pass


class HdrIterationValue(object):
    """
    Class of the values returned by each iterator
    """

    def __init__(self, hdr_iterator):
        self.hdr_iterator = hdr_iterator
        self.value_iterated_to = 0
        self.value_iterated_from = 0,
        self.count_at_value_iterated_to = 0
        self.count_added_in_this_iter_step = 0
        self.total_count_to_this_value = 0
        self.total_value_to_this_value = 0
        self.percentile = 0.0
        self.percentile_level_iterated_to = 0.0

    def set(self, value_iterated_to):
        hdr_it = self.hdr_iterator
        self.value_iterated_to = value_iterated_to
        self.value_iterated_from = hdr_it.prev_value_iterated_to
        self.count_at_value_iterated_to = hdr_it.count_at_this_value
        self.count_added_in_this_iter_step = \
            hdr_it.total_count_to_current_index - hdr_it.total_count_to_prev_index
        self.total_count_to_this_value = hdr_it.total_count_to_current_index
        self.total_value_to_this_value = hdr_it.value_to_index
        self.percentile = (100.0 * hdr_it.total_count_to_current_index) / hdr_it.total_count
        self.percentile_level_iterated_to = hdr_it.get_percentile_iterated_to()


class AbstractHdrIterator(object):
    """
    Provide a means of iterating through all histogram values using the finest
    granularity steps supported by the underlying representation.
    The iteration steps through all possible unit value levels, regardless of
    whether or not there were recorded values for that value level,
    and terminates when all recorded histogram values are exhausted.
    """

    def __init__(self, histogram):
        self.histogram = histogram
        self.current_index = 0
        self.count_at_this_value = 0
        self.total_count_to_current_index = 0
        self.total_count_to_prev_index = 0
        self.prev_value_iterated_to = 0
        self.value_at_index = 0
        self.value_to_index = 0
        self.value_at_next_index = 1 << histogram.unit_magnitude
        self.current_iteration_value = HdrIterationValue(self)
        # take a snapshot of the total count
        self.total_count = histogram.total_count
        self.fresh_sub_bucket = True

    def __iter__(self):
        self.reset_iterator(self.histogram)
        return self

    def reset_iterator(self, histogram):
        if not histogram:
            histogram = self.histogram
        self.histogram = histogram
        self.current_index = 0
        self.count_at_this_value = 0
        self.total_count_to_current_index = 0
        self.total_count_to_prev_index = 0
        self.prev_value_iterated_to = 0
        self.value_at_index = 0
        self.value_to_index = 0
        self.value_at_next_index = 1 << histogram.unit_magnitude
        self.current_iteration_value = HdrIterationValue(self)
        # take a snapshot of the total count
        self.total_count = histogram.total_count
        self.fresh_sub_bucket = True

    def has_next(self):
        return self.total_count_to_current_index < self.total_count

    def __next__(self):
        if self.total_count != self.histogram.total_count:
            raise HdrConcurrentModificationException()
        while self.has_next():
            self.count_at_this_value = self.histogram.get_count_at_index(self.current_index)
            if self.fresh_sub_bucket:
                self.total_count_to_current_index += self.count_at_this_value
                self.value_to_index += self.count_at_this_value * self.get_value_iterated_to()
                self.fresh_sub_bucket = False
            if self.reached_iteration_level():
                value_iterated_to = self.get_value_iterated_to()
                self.current_iteration_value.set(value_iterated_to)

                self.prev_value_iterated_to = value_iterated_to
                self.total_count_to_prev_index = self.total_count_to_current_index

                self.increment_iteration_level()

                if self.total_count != self.histogram.total_count:
                    raise HdrConcurrentModificationException()

                return self.current_iteration_value

            # get to the next sub bucket
            self.increment_sub_bucket()

        if self.total_count_to_current_index > self.total_count_to_prev_index:
            # We are at the end of the iteration but we still need to report
            # the last iteration value
            value_iterated_to = self.get_value_iterated_to()
            self.current_iteration_value.set(value_iterated_to)
            # we do this one time only
            self.total_count_to_prev_index = self.total_count_to_current_index
            return self.current_iteration_value

        raise StopIteration()
    next = __next__

    @abstractmethod
    def reached_iteration_level(self):
        pass

    @abstractmethod
    def increment_iteration_level(self):
        pass

    def increment_sub_bucket(self):
        self.fresh_sub_bucket = True
        self.current_index += 1
        self.value_at_index = self.histogram.get_value_from_index(self.current_index)
        self.value_at_next_index = \
            self.histogram.get_value_from_index(self.current_index + 1)

    def get_value_iterated_to(self):
        return self.histogram.get_highest_equivalent_value(self.value_at_index)

    def get_percentile_iterated_to(self):
        return (100.0 * self.total_count_to_current_index) / self.total_count

    def get_percentile_iterated_from(self):
        return (100.0 * self.total_count_to_prev_index) / self.total_count


class RecordedIterator(AbstractHdrIterator):
    """Provide a means of iterating through all recorded histogram values
    using the finest granularity steps supported by the underlying representation.
    The iteration steps through all non-zero recorded value counts,
    and terminates when all recorded histogram values are exhausted.
    """
    def __init__(self, histogram):
        AbstractHdrIterator.__init__(self, histogram)
        self.visited_index = -1

    def reset(self, histogram=None):
        AbstractHdrIterator.reset_iterator(self, histogram)
        self.visited_index = -1

    def reached_iteration_level(self):
        current_count = self.histogram.get_count_at_index(self.current_index)
        return current_count and self.visited_index != self.current_index

    def increment_iteration_level(self):
        self.visited_index = self.current_index

    def has_next(self):
        if self.total_count != self.histogram.total_count:
            raise HdrConcurrentModificationException()
        return self.current_index < self.histogram.counts_len - 1


class HdrHistogram(object):
    """
    This class supports the recording and analyzing of sampled data value
    counts across a configurable integer value range with configurable value
    precision within the range. Value precision is expressed as the number of
    significant digits in the value recording, and provides control over value
    quantization behavior across the value range and the subsequent value
    resolution at any given level.

    For example, a Histogram could be configured to track the counts of
    observed integer values between 0 and 3,600,000,000 while maintaining a
    value precision of 3 significant digits across that range. Value
    quantization within the range will thus be no larger than 1/1,000th
    (or 0.1%) of any value. This example Histogram could be used to track and
    analyze the counts of observed response times ranging between 1 microsecond
    and 1 hour in magnitude, while maintaining a value resolution of 1
    microsecond up to 1 millisecond, a resolution of 1 millisecond (or better)
    up to one second, and a resolution of 1 second (or better) up to 1,000
    seconds. At it's maximum tracked value (1 hour), it would still maintain a
    resolution of 3.6 seconds (or better).
    """

    def __init__(self, lowest_trackable_value, highest_trackable_value, significant_figures):
        """
        Create a new histogram with given arguments

        Params:
            lowest_trackable_value The lowest value that can be discerned
                (distinguished from 0) by the histogram.
                Must be a positive integer that is >= 1.
                May be internally rounded down to nearest power of 2.
            highest_trackable_value The highest value to be tracked by the
                histogram. Must be a positive integer that is >=
                (2 * lowest_trackable_value).
            significant_figures The number of significant decimal digits to
                which the histogram will maintain value resolution and
                separation. Must be a non-negative integer between 0 and 5.
        Exceptions:
            ValueError if the word_size value is unsupported
                if significant_figures is invalid
        """
        if significant_figures < 1 or significant_figures > 5:
            raise ValueError('Invalid significant_figures')
        self.lowest_trackable_value = lowest_trackable_value
        self.highest_trackable_value = highest_trackable_value
        self.significant_figures = significant_figures
        self.unit_magnitude = int(math.floor(math.log(lowest_trackable_value) / math.log(2)))
        largest_value_single_unit_res = 2 * math.pow(10, significant_figures)
        subb_count_mag = int(math.ceil(math.log(largest_value_single_unit_res) / math.log(2)))
        self.sub_bucket_half_count_magnitude = subb_count_mag - 1 if subb_count_mag > 1 else 0
        self.sub_bucket_count = int(math.pow(2, self.sub_bucket_half_count_magnitude + 1))
        self.sub_bucket_half_count = self.sub_bucket_count // 2
        self.sub_bucket_mask = (self.sub_bucket_count - 1) << self.unit_magnitude
        self.bucket_count = get_bucket_count(highest_trackable_value,
                                             self.sub_bucket_count,
                                             self.unit_magnitude)
        self.min_value = sys.maxsize
        self.max_value = 0
        self.total_count = 0
        self.counts_len = (self.bucket_count + 1) * (self.sub_bucket_count // 2)
        self.counts = numpy.zeros(self.counts_len, dtype=numpy.int64)

    @staticmethod
    def _clz(value):
        """calculate the leading zeros, equivalent to C __builtin_clzll()
        value in hex:
        value = 1 clz = 63
        value = 2 clz = 62
        value = 4 clz = 61
        value = 1000 clz = 51
        value = 1000000 clz = 39
        """
        return 63 - (len(bin(value)) - 3)

    def _get_bucket_index(self, value):
        # smallest power of 2 containing value
        pow2ceiling = 64 - self._clz(int(value) | self.sub_bucket_mask)
        return int(pow2ceiling - self.unit_magnitude -
                   (self.sub_bucket_half_count_magnitude + 1))

    def _get_sub_bucket_index(self, value, bucket_index):
        return int(value) >> (bucket_index + self.unit_magnitude)

    def _counts_index(self, bucket_index, sub_bucket_index):
        # Calculate the index for the first entry in the bucket:
        # (The following is the equivalent of ((bucket_index + 1) * subBucketHalfCount) ):
        bucket_base_index = (bucket_index + 1) << self.sub_bucket_half_count_magnitude
        # Calculate the offset in the bucket:
        offset_in_bucket = sub_bucket_index - self.sub_bucket_half_count
        # The following is the equivalent of
        # ((sub_bucket_index  - subBucketHalfCount) + bucketBaseIndex
        return bucket_base_index + offset_in_bucket

    def _counts_index_for(self, value):
        bucket_index = self._get_bucket_index(value)
        sub_bucket_index = self._get_sub_bucket_index(value, bucket_index)
        return self._counts_index(bucket_index, sub_bucket_index)

    def record_value(self, value, count=1):
        """Record a new value into the histogram

        Args:
            value: the value to record (must be in the valid range)
            count: incremental count (defaults to 1)
        """
        if value < 0:
            return False
        counts_index = self._counts_index_for(value)
        if (counts_index < 0) or (self.counts_len <= counts_index):
            return False
        self.counts[counts_index] += count
        self.total_count += count
        self.min_value = min(self.min_value, value)
        self.max_value = max(self.max_value, value)
        return True

    def record_corrected_value(self, value, expected_interval, count=1):
        """Record a new value into the histogram and correct for
        coordinated omission if needed

        Args:
            value: the value to record (must be in the valid range)
            expected_interval: the expected interval between 2 value samples
            count: incremental count (defaults to 1)
        """
        while True:
            if not self.record_value(value, count):
                return False
            if value <= expected_interval or expected_interval <= 0:
                return True
            value -= expected_interval

    def get_count_at_index(self, index):
        if index >= self.counts_len:
            raise IndexError()
        return self.counts[index]

    def get_value_from_sub_bucket(self, bucket_index, sub_bucket_index):
        return sub_bucket_index << (bucket_index + self.unit_magnitude)

    def get_value_from_index(self, index):
        bucket_index = (index >> self.sub_bucket_half_count_magnitude) - 1
        sub_bucket_index = (index & (self.sub_bucket_half_count - 1)) + self.sub_bucket_half_count
        if bucket_index < 0:
            sub_bucket_index -= self.sub_bucket_half_count
            bucket_index = 0
        return self.get_value_from_sub_bucket(bucket_index, sub_bucket_index)

    def get_lowest_equivalent_value(self, value):
        bucket_index = self._get_bucket_index(value)
        sub_bucket_index = self._get_sub_bucket_index(value, bucket_index)

        lowest_equivalent_value = self.get_value_from_sub_bucket(bucket_index,
                                                                 sub_bucket_index)
        return lowest_equivalent_value

    def get_highest_equivalent_value(self, value):
        bucket_index = self._get_bucket_index(value)
        sub_bucket_index = self._get_sub_bucket_index(value, bucket_index)

        lowest_equivalent_value = self.get_value_from_sub_bucket(bucket_index,
                                                                 sub_bucket_index)
        if sub_bucket_index >= self.sub_bucket_count:
            bucket_index += 1
        size_of_equivalent_value_range = 1 << (self.unit_magnitude + bucket_index)
        next_non_equivalent_value = lowest_equivalent_value + size_of_equivalent_value_range

        return next_non_equivalent_value - 1

    def get_target_count_at_percentile(self, percentile):
        requested_percentile = min(percentile, 100.0)
        count_at_percentile = int((requested_percentile * self.total_count / 100) + 0.5)
        return max(count_at_percentile, 1)

    def get_value_at_percentile(self, percentile):
        """
        Get the value for a given percentile

        Args:
            percentile: a float in [0.0..100.0]
        Returns:
            the value for the given percentile
        """
        count_at_percentile = self.get_target_count_at_percentile(percentile)
        total = 0
        for index in range(self.counts_len):
            total += self.get_count_at_index(index)
            if total >= count_at_percentile:
                value_at_index = self.get_value_from_index(index)
                if percentile:
                    return self.get_highest_equivalent_value(value_at_index)
                return self.get_lowest_equivalent_value(value_at_index)
        return 0

    def get_percentile_to_value_dict(self, percentile_list):
        """
        A faster alternative to query values for a list of percentiles.

        Args:
            percentile_list: a list of percentiles in any order, dups will be ignored
            each element in the list must be a float value in [0.0 .. 100.0]
        Returns:
            a dict of percentile values indexed by the percentile
        """
        result = {}
        total = 0
        percentile_list_index = 0
        count_at_percentile = 0
        # remove dups and sort
        percentile_list = list(set(percentile_list))
        percentile_list.sort()

        for index in range(self.counts_len):
            total += self.get_count_at_index(index)
            while True:
                # recalculate target based on next requested percentile
                if not count_at_percentile:
                    if percentile_list_index == len(percentile_list):
                        return result
                    percentile = percentile_list[percentile_list_index]
                    percentile_list_index += 1
                    if percentile > 100:
                        return result
                    count_at_percentile = self.get_target_count_at_percentile(percentile)

                if total >= count_at_percentile:
                    value_at_index = self.get_value_from_index(index)
                    if percentile:
                        result[percentile] = self.get_highest_equivalent_value(value_at_index)
                    else:
                        result[percentile] = self.get_lowest_equivalent_value(value_at_index)
                    count_at_percentile = 0
                else:
                    break
        return result

    def get_total_count(self):
        return self.total_count

    def get_count_at_value(self, value):
        counts_index = self._counts_index_for(value)
        return self.counts[counts_index]

    def values_are_equivalent(self, val1, val2):
        """Check whether 2 values are equivalent (meaning they
        are in the same bucket/range)

        Returns:
            true if the 2 values are equivalent
        """
        return self.get_lowest_equivalent_value(val1) == self.get_lowest_equivalent_value(val2)

    def get_max_value(self):
        if self.max_value == 0:
            return 0
        return self.get_highest_equivalent_value(self.max_value)

    def get_min_value(self):
        if self.counts[0] > 0 or self.total_count == 0:
            return 0
        if sys.maxsize == self.min_value:
            return sys.maxsize
        return self.get_lowest_equivalent_value(self.min_value)

    def _hdr_size_of_equiv_value_range(self, value):
        bucket_index = self._get_bucket_index(value)
        sub_bucket_index = self._get_sub_bucket_index(value, bucket_index)
        if sub_bucket_index >= self.sub_bucket_count:
            bucket_index += 1
        return 1 << (self.unit_magnitude + bucket_index)

    def _hdr_median_equiv_value(self, value):
        return self.get_lowest_equivalent_value(value) + \
               (self._hdr_size_of_equiv_value_range(value) >> 1)

    def get_mean_value(self):
        if not self.total_count:
            return 0.0
        total = 0
        itr = self.get_recorded_iterator()
        for item in itr:
            total += itr.count_at_this_value * self._hdr_median_equiv_value(item.value_iterated_to)
        return float(total) / self.total_count

    def get_stddev(self):
        if not self.total_count:
            return 0.0
        mean = self.get_mean_value()
        geometric_dev_total = 0.0
        for item in self.get_recorded_iterator():
            dev = (self._hdr_median_equiv_value(item.value_iterated_to) * 1.0) - mean
            geometric_dev_total += (dev * dev) * item.count_added_in_this_iter_step
        return math.sqrt(geometric_dev_total / self.total_count)

    def reset(self):
        """Reset the histogram to a pristine state
        """
        for index in range(self.counts_len):
            self.counts[index] = 0
        self.total_count = 0
        self.min_value = sys.maxsize
        self.max_value = 0

    def __iter__(self):
        """Returns the recorded iterator if iter(self) is called
        """
        return self.get_recorded_iterator()

    def get_recorded_iterator(self):
        return RecordedIterator(self)

    def add(self, other_hist):
        """

        :type other_hist: HdrHistogram
        """
        highest_recordable_value = \
            self.get_highest_equivalent_value(self.get_value_from_index(self.counts_len - 1))
        if highest_recordable_value < other_hist.get_max_value():
            raise IndexError("The other histogram includes values that do not fit %d < %d" %
                             (highest_recordable_value, other_hist.get_max_value()))

        if (self.bucket_count == other_hist.bucket_count) and \
                (self.sub_bucket_count == other_hist.sub_bucket_count) and \
                (self.unit_magnitude == other_hist.unit_magnitude):

            self.counts += other_hist.counts  # TODO: check other_hist.counts dtype?
            self.total_count += other_hist.get_total_count()
            self.max_value = max(self.max_value, other_hist.get_max_value())
            self.min_value = min(self.get_min_value(), other_hist.get_min_value())
        else:
            # Arrays are not a direct match, so we can't just stream through and add them.
            # Instead, go through the array and add each non-zero value found at it's proper value:
            for index in range(other_hist.counts_len):
                other_count = other_hist.get_count_at_index(index)
                if other_count > 0:
                    self.record_value(other_hist.get_value_from_index(index), other_count)
