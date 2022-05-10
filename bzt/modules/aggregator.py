"""
Aggregating results into DataPoints

Copyright 2015 BlazeMeter Inc.

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
import collections
import copy
import logging
import math
import time
from abc import abstractmethod
from collections import Counter

import fuzzyset
from hdrpy import HdrHistogram, RecordedIterator
from yaml import SafeDumper
from yaml.representer import SafeRepresenter

from bzt import TaurusInternalException, TaurusConfigError
from bzt.engine import Aggregator
from bzt.utils import iteritems, dehumanize_time, JSONConvertible, is_int

log = logging.getLogger('aggregator')
SAMPLE_STATES = 'success', 'jmeter_errors', 'http_errors'
AGGREGATED_STATES = (
    '_'.join((SAMPLE_STATES[0], SAMPLE_STATES[1])),     # success_jmeter_errors
    '_'.join((SAMPLE_STATES[0], SAMPLE_STATES[2])),     # success_http_errors
    '_'.join((SAMPLE_STATES[2], SAMPLE_STATES[1])),     # http_errors_jmeter_errors
)


class SinglePassIterator(RecordedIterator):
    """
    An iterator to do only one pass on data and calculate all we need inside it.
    Should do one pass and return:
    - stddev
    - percentiles
    - histogram
    """

    def __init__(self, histogram, percentiles, mean):
        super(SinglePassIterator, self).__init__(histogram)
        assert mean is not None, "Known mean is required"
        self.perc_levels = list(percentiles)
        self.perc_levels.sort()
        self._mean = mean
        self._init()

    def reset(self, histogram=None):
        super(SinglePassIterator, self).reset(histogram)
        self._init()

    def _init(self):
        self.percentiles = {}
        self.stdev = 0
        self.hist_values = {}
        self._geometric_dev_total = 0.0
        self._perc_indexes = copy.copy(self.perc_levels)

    def __next__(self):
        item = super(SinglePassIterator, self).__next__()

        # histogram
        self.hist_values[item.value_iterated_to] = item.count_at_value_iterated_to

        # stddev
        dev = (self.histogram._hdr_median_equiv_value(item.value_iterated_to) * 1.0) - self._mean * 1000
        self._geometric_dev_total += (dev * dev) * item.count_added_in_this_iter_step

        # percentiles
        self._fill_percentiles()

        return item

    def _fill_percentiles(self):
        while self._perc_indexes and self._perc_indexes[0] <= self.get_percentile_iterated_to():
            perc_level = self._perc_indexes.pop(0)
            self.percentiles[perc_level] = self.value_at_index

    next = __next__

    def has_next(self):
        has = super(SinglePassIterator, self).has_next()
        if not has:
            self.stdev = math.sqrt(self._geometric_dev_total / self.total_count)
            assert set(self.perc_levels) == set(self.percentiles.keys()), 'Not all percentiles are generated'
        return has


class RespTimesCounter(JSONConvertible):
    def __init__(self, low, high, sign_figures, perc_levels=()):
        super(RespTimesCounter, self).__init__()
        self.low = low
        self.high = high
        self.sign_figures = sign_figures
        self.histogram = HdrHistogram(low, high, sign_figures)
        self._ff_iterator = None
        self._perc_levels = perc_levels
        self.known_mean = None

    def __deepcopy__(self, memo):
        new = RespTimesCounter(self.low, self.high, self.sign_figures)
        new._ff_iterator = self._ff_iterator
        new._perc_levels = self._perc_levels

        new.histogram.counts = copy.deepcopy(self.histogram.counts, memo)
        new.histogram.total_count = self.histogram.total_count
        new.histogram.min_value = self.histogram.min_value
        new.histogram.max_value = self.histogram.max_value

        return new

    def __bool__(self):
        return len(self) > 0

    def __len__(self):
        return self.histogram.total_count

    def add(self, item, count=1):
        item = round(item * 1000.0, 3)
        if item > self.high:
            self.__grow(math.ceil(item / 1000.0) * 1000.0)
        self._ff_iterator = None
        self.histogram.record_value(item, count)

    def merge(self, other):
        self._ff_iterator = None
        if other.high > self.high:
            self.__grow(other.high)

        self.histogram.add(other.histogram)

    def _get_ff(self):
        if self._ff_iterator is None:
            self._ff_iterator = SinglePassIterator(self.histogram, self._perc_levels, self.known_mean)
            for _ in self._ff_iterator:
                pass  # consume it
        return self._ff_iterator

    def get_percentiles_dict(self):
        return self._get_ff().percentiles

    def get_counts(self):
        return self._get_ff().hist_values

    def get_stdev(self):
        return self._get_ff().stdev / 1000.0

    def __json__(self):
        return {
            rt / 1000.0: int(count)  # because hdrpy returns int64, which is unrecognized by json serializer
            for rt, count in iteritems(self.get_counts())
        }

    def __grow(self, newsize):
        log.debug("Growing HDR from %s to %s", self.high, newsize)
        old = self.histogram
        self.high = newsize
        self.histogram = HdrHistogram(self.low, self.high, self.sign_figures)
        self.histogram.add(old)


class KPISet(dict):
    """
    Main entity in results, contains all KPIs for single label,
    capable of merging other KPISet's into it to compose cumulative results
    """
    ERRORS = "errors"
    SAMPLE_COUNT = "throughput"
    CONCURRENCY = "concurrency"
    SUCCESSES = "succ"
    FAILURES = "fail"
    BYTE_COUNT = "bytes"
    RESP_TIMES = "rt"
    AVG_RESP_TIME = "avg_rt"
    STDEV_RESP_TIME = "stdev_rt"
    AVG_LATENCY = "avg_lt"
    AVG_CONN_TIME = "avg_ct"
    PERCENTILES = "perc"
    RESP_CODES = "rc"
    ERRTYPE_ERROR = 0
    ERRTYPE_ASSERT = 1
    ERRTYPE_SUBSAMPLE = 2

    def __init__(self, perc_levels=(), hist_max_rt=1000.0, ext_aggregation=False):
        super(KPISet, self).__init__()
        self.sum_rt = 0
        self.sum_lt = 0
        self.sum_cn = 0
        self.perc_levels = perc_levels
        if ext_aggregation:
            self.concurrencies = set()
        else:
            self.concurrencies = Counter()
        # scalars
        self[KPISet.SAMPLE_COUNT] = 0
        self[KPISet.CONCURRENCY] = 0
        self[KPISet.SUCCESSES] = 0
        self[KPISet.FAILURES] = 0
        self[KPISet.AVG_RESP_TIME] = 0
        self[KPISet.STDEV_RESP_TIME] = 0
        self[KPISet.AVG_LATENCY] = 0
        self[KPISet.AVG_CONN_TIME] = 0
        self[KPISet.BYTE_COUNT] = 0
        # vectors
        self[KPISet.ERRORS] = []
        self[KPISet.RESP_TIMES] = RespTimesCounter(1, hist_max_rt, 3, perc_levels)
        self[KPISet.RESP_CODES] = Counter()
        self[KPISet.PERCENTILES] = {}
        self.ext_aggregation = ext_aggregation

    def __deepcopy__(self, memo):
        mycopy = KPISet(self.perc_levels, self[KPISet.RESP_TIMES].high)
        mycopy.ext_aggregation = self.ext_aggregation
        mycopy.sum_rt = self.sum_rt
        mycopy.sum_lt = self.sum_lt
        mycopy.sum_cn = self.sum_cn
        mycopy.perc_levels = self.perc_levels
        mycopy.concurrencies = copy.deepcopy(self.concurrencies, memo)
        for key in self:
            mycopy[key] = copy.deepcopy(self.get(key, no_recalc=True), memo)
        return mycopy

    @staticmethod
    def error_item_skel(error, ret_c, cnt, errtype, urls, tag):
        """

        :type error: str
        :type ret_c: str
        :type tag: str
        :type cnt: int
        :type errtype: int
        :type urls: collections.Counter
        :rtype: dict
        """
        assert isinstance(urls, collections.Counter)
        return {
            "cnt": cnt,
            "msg": error,
            "tag": tag,  # just one more string qualifier
            "rc": ret_c,
            "type": errtype,
            "urls": urls,
        }

    def add_sample(self, sample):
        """
        Add sample, consisting of: cnc, rt, cn, lt, rc, error, trname, byte_count

        :type sample: tuple
        """
        cnc, r_time, con_time, latency, r_code, error, trname, byte_count = sample
        self[self.SAMPLE_COUNT] += 1
        if self.ext_aggregation:
            self.concurrencies.add(trname)
        elif cnc:
            self.add_concurrency(cnc, trname)

        if r_code is not None:
            self[self.RESP_CODES][r_code] += 1

            # count times only if we have RCs
            if con_time:
                self.sum_cn += con_time
            self.sum_lt += latency
            self.sum_rt += r_time

        if error is not None:
            self[self.FAILURES] += 1

            item = self.error_item_skel(error, r_code, 1, KPISet.ERRTYPE_ERROR, Counter(), None)
            self.inc_list(self[self.ERRORS], ("msg", error), item)
        else:
            self[self.SUCCESSES] += 1

        self[self.RESP_TIMES].add(r_time, 1)

        if byte_count is not None:
            self[self.BYTE_COUNT] += byte_count

    def add_concurrency(self, cnc, sid):
        # sid: source id, e.g. node id for jmeter distributed mode
        if self.ext_aggregation:
            if isinstance(sid, set):
                self.concurrencies.update(sid)
            else:
                self.concurrencies.add(sid)
        elif self.concurrencies.get(sid, 0) < cnc:    # take max value of concurrency during the second.
            self.concurrencies[sid] = cnc

    @staticmethod
    def inc_list(values, selector, value):
        """
        Increment list item, based on selector criteria

        :param values: list to update
        :param selector: tuple of 2 values, field name and value to match
        :param value: dict to put into list
        :type values: list[dict]
        :type selector: tuple
        :type value: dict
        """
        found = False
        for item in values:
            if item[selector[0]] == selector[1]:
                item['cnt'] += value['cnt']
                item['urls'] += value['urls']
                found = True
                break

        if not found:
            values.append(copy.deepcopy(value))

    def __getitem__(self, key):
        rtimes = self.get(self.RESP_TIMES, no_recalc=True)
        rtimes.known_mean = self.get(self.AVG_RESP_TIME, no_recalc=True)
        if key != self.RESP_TIMES and rtimes:
            if key == self.STDEV_RESP_TIME:
                self[self.STDEV_RESP_TIME] = rtimes.get_stdev()
            elif key == self.PERCENTILES:
                percs = {str(float(perc)): value / 1000.0 for perc, value in
                         iteritems(rtimes.get_percentiles_dict())}
                self[self.PERCENTILES] = percs

        val = super(KPISet, self).__getitem__(key)
        assert key != KPISet.ERRORS or isinstance(val, list)
        return val

    def get(self, k, no_recalc=False):
        if no_recalc:
            return super(KPISet, self).get(k)
        else:
            return self.__getitem__(k)

    def items(self):
        for item in super(KPISet, self).items():
            yield (item[0], self.__getitem__(item[0]))

    def viewvalues(self):
        raise TaurusInternalException("Invalid call")

    def values(self):
        raise TaurusInternalException("Invalid call")

    def recalculate(self):
        """
        Recalculate averages, stdev and percentiles

        :return:
        """
        if self[self.SAMPLE_COUNT]:
            self[self.AVG_CONN_TIME] = self.sum_cn / self[self.SAMPLE_COUNT]
            self[self.AVG_LATENCY] = self.sum_lt / self[self.SAMPLE_COUNT]
            self[self.AVG_RESP_TIME] = self.sum_rt / self[self.SAMPLE_COUNT]

        if len(self.concurrencies):
            if self.ext_aggregation:
                self[self.CONCURRENCY] = len(self.concurrencies)
            else:
                self[self.CONCURRENCY] = sum(self.concurrencies.values())

        return self

    def merge_kpis(self, src, sid=None):
        """
        Merge other instance into self

        :param sid: source ID to use when suming up concurrency
        :type src: KPISet
        :return:
        """
        src.recalculate()

        self.sum_cn += src.sum_cn
        self.sum_lt += src.sum_lt
        self.sum_rt += src.sum_rt

        self[self.SAMPLE_COUNT] += src[self.SAMPLE_COUNT]
        self[self.SUCCESSES] += src[self.SUCCESSES]
        self[self.FAILURES] += src[self.FAILURES]
        self[self.BYTE_COUNT] += src[self.BYTE_COUNT]
        # NOTE: should it be average? mind the timestamp gaps
        if self.ext_aggregation:
            sid = src.concurrencies
        if src[self.CONCURRENCY]:
            self.add_concurrency(src[self.CONCURRENCY], sid)

        if src[self.RESP_TIMES]:
            self[self.RESP_TIMES].merge(src[self.RESP_TIMES])
        elif not self[self.PERCENTILES]:
            # using existing percentiles, in case we have no source data to recalculate them
            self[self.PERCENTILES] = copy.deepcopy(src[self.PERCENTILES])

        self[self.RESP_CODES].update(src[self.RESP_CODES])

        for src_item in src[self.ERRORS]:
            self.inc_list(self[self.ERRORS], ('msg', src_item['msg']), src_item)

    @staticmethod
    def from_dict(obj, ext_aggregation=False):
        """
        :type obj: dict
        :type ext_aggregation: bool
        :rtype: KPISet
        """
        prc_levels = [float(x) for x in obj[KPISet.PERCENTILES].keys()]
        inst = KPISet(perc_levels=prc_levels, ext_aggregation=ext_aggregation)

        assert inst.PERCENTILES in obj

        for key, val in iteritems(obj):
            if key == inst.RESP_TIMES:
                if isinstance(val, dict):
                    for value, count in iteritems(val):
                        inst[inst.RESP_TIMES].add(float(value), count)
            else:
                inst[key] = val

        inst.sum_cn = obj[inst.AVG_CONN_TIME] * obj[inst.SAMPLE_COUNT]
        inst.sum_lt = obj[inst.AVG_LATENCY] * obj[inst.SAMPLE_COUNT]
        inst.sum_rt = obj[inst.AVG_RESP_TIME] * obj[inst.SAMPLE_COUNT]
        for error in inst[KPISet.ERRORS]:
            error['urls'] = Counter(error['urls'])
        return inst


class DataPoint(dict):
    """
    Represents an aggregate data point

    :param ts: timestamp of this point
    """

    SOURCE_ID = 'id'
    TIMESTAMP = "ts"
    CURRENT = "current"
    CUMULATIVE = "cumulative"
    SUBRESULTS = "subresults"

    def __init__(self, ts, perc_levels=()):
        """

        :type ts: int
        :type perc_levels: list[float]
        """
        super(DataPoint, self).__init__()
        self.perc_levels = perc_levels
        self[self.SOURCE_ID] = None
        self[self.TIMESTAMP] = ts
        self[self.CUMULATIVE] = {}
        self[self.CURRENT] = {}
        self[self.SUBRESULTS] = []

    def __deepcopy__(self, memo):
        new = DataPoint(self[self.TIMESTAMP], self.perc_levels)
        for key in self.keys():
            new[key] = copy.deepcopy(self[key], memo)
        return new

    def __merge_kpis(self, src, dst, sid):
        """
        :param src: dict[str,KPISet]
        :param dst: dict[str,KPISet]
        :param sid: int
        :return:
        """
        for label, val in iteritems(src):
            dest = dst.setdefault(label, KPISet(self.perc_levels, val[KPISet.RESP_TIMES].high))
            if not isinstance(val, KPISet):
                val = KPISet.from_dict(val)
                val.perc_levels = self.perc_levels
            dest.merge_kpis(val, sid)

    def recalculate(self):
        """
        Recalculate all KPISet's
        """
        for val in self[self.CURRENT].values():
            val.recalculate()

        for val in self[self.CUMULATIVE].values():
            val.recalculate()

    def merge_point(self, src, do_recalculate=True):
        """

        :type src: DataPoint
        """
        if self[self.TIMESTAMP] != src[self.TIMESTAMP]:
            msg = "Cannot merge different timestamps (%s and %s)"
            raise TaurusInternalException(msg % (self[self.TIMESTAMP], src[self.TIMESTAMP]))

        self[DataPoint.SUBRESULTS].append(src)

        self.__merge_kpis(src[self.CURRENT], self[self.CURRENT], src[DataPoint.SOURCE_ID])
        self.__merge_kpis(src[self.CUMULATIVE], self[self.CUMULATIVE], src[DataPoint.SOURCE_ID])

        if do_recalculate:
            self.recalculate()


SafeDumper.add_representer(KPISet, SafeRepresenter.represent_dict)
SafeDumper.add_representer(DataPoint, SafeRepresenter.represent_dict)


class ResultsProvider(object):
    """
    :type listeners: list[AggregatorListener]
    """

    def __init__(self):
        super(ResultsProvider, self).__init__()
        self.cumulative = {}
        self.track_percentiles = [0.0, 50.0, 90.0, 95.0, 99.0, 99.9, 100.0]
        self.listeners = []

        self.buffer_scale_idx = None    # string value of the most interesting percentile (e.q. '90.0')
        self.buffer_multiplier = 2      # how many sequential samples we want to get before aggregation (approximately)

        self.buffer_len = 2                     # how many data points we want to collect before aggregation
        self.min_buffer_len = 2                 # small buffer is more responsive but tends to loose data
        self.max_buffer_len = float('inf')      # buffer_len value can be changed during runtime.

        self.histogram_max = 1.0
        self.known_errors = fuzzyset.FuzzySet(use_levenshtein=True)
        self.max_error_count = 100
        self.known_labels = fuzzyset.FuzzySet(use_levenshtein=True)
        self.generalize_labels = 100
        self._redundant_aggregation = False

    def set_aggregation(self, aggregation):
        self._redundant_aggregation = aggregation

    @staticmethod
    def _fuzzy_fold(key, dataset, limit):
        """
        :type key: str
        :type dataset: fuzzyset.FuzzySet
        :type limit: int
        :rtype: str
        """
        if not key or limit <= 0:
            return key

        if not isinstance(key, str):
            key = key.decode('utf-8')

        if key.lower() in dataset.exact_set:
            return key

        size = len(dataset)
        if size >= limit / 4:
            tolerance = (float(size) / float(limit)) ** 2
            threshold = 1 - tolerance
            matches = dataset.get(key)
            if matches:
                for score, result in matches:
                    if score >= threshold:
                        return result
            elif tolerance >= 1.0:
                return next(iter(dataset.exact_set.values()))  # last resort for capping

        dataset.add(key)
        return key

    def _generalize_label(self, label):
        return self._fuzzy_fold(label, self.known_labels, self.generalize_labels)

    def _fold_error(self, error):
        return self._fuzzy_fold(error, self.known_errors, self.max_error_count)

    def add_listener(self, listener):
        """
        Add aggregate results listener

        :type listener: AggregatorListener
        """
        self.listeners.append(listener)

    def __merge_to_cumulative(self, current):
        """
        Merge current KPISet to cumulative
        :param current: KPISet
        """
        for label, data in iteritems(current):
            default = KPISet(
                perc_levels=self.track_percentiles,
                hist_max_rt=data[KPISet.RESP_TIMES].high,
                ext_aggregation=self._redundant_aggregation)
            cumul = self.cumulative.setdefault(label, default)
            cumul.merge_kpis(data)
            cumul.recalculate()

    def datapoints(self, final_pass=False):
        """
        Generator object that returns datapoints from the reader

        :type final_pass: bool
        """
        for datapoint in self._calculate_datapoints(final_pass):
            current = datapoint[DataPoint.CURRENT]
            if datapoint[DataPoint.CUMULATIVE] or not self._ramp_up_exclude():
                self.__merge_to_cumulative(current)
                datapoint[DataPoint.CUMULATIVE] = copy.deepcopy(self.cumulative)
                datapoint.recalculate()

            for listener in self.listeners:
                listener.aggregated_second(datapoint)
            yield datapoint

    @abstractmethod
    def _calculate_datapoints(self, final_pass=False):
        """
        :rtype : list[DataPoint]
        """
        yield

    @abstractmethod
    def _ramp_up_exclude(self):
        """
        :rtype : bool
        """
        return False


class ResultsReader(ResultsProvider):
    """
    Aggregator that reads samples one by one,
    supposed to be attached to every executor
    """
    def __init__(self, perc_levels=None):
        super(ResultsReader, self).__init__()
        self.ignored_labels = []
        self.log = logging.getLogger(self.__class__.__name__)
        self.buffer = {}
        self.min_timestamp = 0      # last aggregated timestamp, older data is obsolete and must be fixed
        if perc_levels is not None:
            self.track_percentiles = perc_levels

    @staticmethod
    def get_mixed_label(label, rc=None, msg=None):
        # it is used for generation of extended label.
        # each label data is split according to sample state (success/error/assert)

        def http_mark():
            http_prefs = ("Embedded resource download error", "Non HTTP response message")
            return any(msg.startswith(http_pref) for http_pref in http_prefs)

        rc = int(rc) if is_int(rc) else 0  # it's jmeter_error by default
        if msg:
            if rc > 299 or http_mark():
                group = SAMPLE_STATES[2]  # http error
            else:
                group = SAMPLE_STATES[1]  # jmeter error
        else:
            group = SAMPLE_STATES[0]  # succeeded sample

        return '-'.join((label, str(group)))

    def __process_readers(self, final_pass=False):
        """

        :param final_pass: True if in post-process stage
        :return:
        """
        for result in self._read(final_pass):
            if result is None:
                self.log.debug("No data from reader")
                break
            elif isinstance(result, list) or isinstance(result, tuple):
                t_stamp, label, conc, r_time, con_time, latency, r_code, error, trname, byte_count = result

                if any([label.startswith(ignore) for ignore in self.ignored_labels]):
                    continue

                if t_stamp < self.min_timestamp:
                    self.log.debug("Putting sample %s into %s", t_stamp, self.min_timestamp)
                    t_stamp = self.min_timestamp

                if r_time < 0:
                    self.log.warning("Negative response time reported by tool, resetting it to zero")
                    r_time = 0

                if t_stamp not in self.buffer:
                    self.buffer[t_stamp] = []

                error = self._fold_error(error)
                self.buffer[t_stamp].append((label, conc, r_time, con_time, latency, r_code, error, trname, byte_count))
            else:
                raise TaurusInternalException("Unsupported results from %s reader: %s" % (self, result))

    def __aggregate_current(self, datapoint, samples):
        """
        :param datapoint: DataPoint
        :param samples: list of samples
        :return:
        """
        current = datapoint[DataPoint.CURRENT]
        for sample in samples:
            # sample format: label, conc, r_time, con_time, latency, r_code, error, trname, byte_count
            base_label = sample[0]

            # empty means overall
            if base_label == '':
                base_label = '[empty]'

            if self.generalize_labels:
                base_label = self._generalize_label(base_label)

            self.__add_sample(current, base_label, sample[1:])

        overall = KPISet(self.track_percentiles, self.__get_rtimes_max(''), ext_aggregation=self._redundant_aggregation)

        for label in current.values():
            overall.merge_kpis(label, datapoint[DataPoint.SOURCE_ID])
        current[''] = overall

        return current

    def _get_suffix(self, label):
        # to collect kpisets to overall sets according to rules result we need to split base label and suffix
        # the suffixes replace '' label in meaning of 'summary result'
        if self._redundant_aggregation:
            return label[label.rfind('-'):]
        return ''

    def __add_sample(self, current, label, kpis):
        if self._redundant_aggregation:
            # kpis format: conc, r_time, con_time, latency, r_code, error_msg, trname, byte_count
            label = self.get_mixed_label(label=label, rc=kpis[4], msg=kpis[5])

        if label not in current:
            current[label] = KPISet(
                perc_levels=self.track_percentiles,
                hist_max_rt=self.__get_rtimes_max(label),
                ext_aggregation=self._redundant_aggregation)

        current[label].add_sample(kpis)

    def __get_rtimes_max(self, label):
        if label in self.cumulative:
            rtimes_max = self.cumulative[label][KPISet.RESP_TIMES].high
        else:
            rtimes_max = self.histogram_max * 1000.0
        return rtimes_max

    def _calculate_datapoints(self, final_pass=False):
        """
        A generator to read available datapoints

        :type final_pass: bool
        :rtype: DataPoint
        """
        if final_pass or len(self.buffer) < self.buffer_len * 10:  # safety valve to preserve RAM
            self.__process_readers(final_pass)
        else:
            self.log.debug("Skipped reading new data, we have enough in the buffer")

        self.log.debug("Buffer len: %s; Known errors count: %s", len(self.buffer), len(self.known_errors))
        if not self.buffer:
            return

        if self.cumulative and self.track_percentiles and self.buffer_scale_idx is not None:
            old_len = self.buffer_len

            # choose average timing of the most interesting percentile
            chosen_timing = self.cumulative[''][KPISet.PERCENTILES][self.buffer_scale_idx]

            # and calculate new buffer_len based on current speed of data getting
            self.buffer_len = round(chosen_timing * self.buffer_multiplier)
            self.buffer_len = max(self.min_buffer_len, self.buffer_len)
            self.buffer_len = min(self.max_buffer_len, self.buffer_len)
            if self.buffer_len != old_len:
                self.log.info("Changed data analysis delay to %ds", self.buffer_len)

        timestamps = sorted(self.buffer.keys())
        while final_pass or (timestamps[-1] >= (timestamps[0] + self.buffer_len)):
            timestamp = timestamps.pop(0)
            self.min_timestamp = timestamp + 1
            self.log.debug("Aggregating: %s, %s in buffer", timestamp, len(self.buffer))
            samples = self.buffer.pop(timestamp)
            datapoint = self.__get_new_datapoint(timestamp)
            self.__aggregate_current(datapoint, samples)
            yield datapoint

            if not timestamps:
                break

    def __get_new_datapoint(self, timestamp):
        """
        :rtype: DataPoint
        """
        point = DataPoint(timestamp, self.track_percentiles)
        point[DataPoint.SOURCE_ID] = id(self)
        return point

    @abstractmethod
    def _read(self, final_pass=False):
        """

        :param final_pass: True if called from post-process stage, when reader
            should report possible rests of results
        :rtype: list
        :return: timestamp, label, concurrency, rt, latency, rc, error
        """
        yield


class ConsolidatingAggregator(Aggregator, ResultsProvider):
    """

    :type underlings: list[bzt.modules.aggregator.ResultsProvider]
    """
    OVERALL_STATE = "all_transactions_aggregated"

    def __init__(self):
        Aggregator.__init__(self, is_functional=False)
        ResultsProvider.__init__(self)
        self.generalize_labels = 500
        self.ignored_labels = ["ignore"]
        self.underlings = []
        self.buffer = {}
        self.histogram_max = 5.0
        self._sticky_concurrencies = {}
        self.min_timestamp = None

    def converter(self, data):
        if data and self._redundant_aggregation:
            self.__extend_reported_data(data)

        return data

    def __extend_reported_data(self, kpi_sets):
        def add_kpi_set_to_state(destination, _state=self.OVERALL_STATE):
            # add kpi_set to data[<label>][<_state>] value
            if _state not in destination:
                destination[_state] = copy.deepcopy(kpi_set)  # avoid merging kpis for first sample
            else:
                destination[_state].merge_kpis(kpi_set)  # deepcopy inside
                destination[_state].recalculate()

        data = kpi_sets['current']
        overall_label = ''
        mixed_labels = set(data.keys()) - {overall_label}
        data[overall_label] = dict()
        for key in mixed_labels:
            sep = key.rindex('-')
            original_label, state = key[:sep], key[sep + 1:]
            kpi_set = data.pop(key)
            if original_label not in data:
                data[original_label] = dict()

            add_kpi_set_to_state(data[overall_label])
            add_kpi_set_to_state(data[original_label])
            add_kpi_set_to_state(data[overall_label], state)
            add_kpi_set_to_state(data[original_label], state)

            for agg_state in AGGREGATED_STATES:
                if state in agg_state:
                    add_kpi_set_to_state(data[overall_label], agg_state)
                    add_kpi_set_to_state(data[original_label], agg_state)
        pass

    def prepare(self):
        """
        Read aggregation options
        """
        super(ConsolidatingAggregator, self).prepare()

        # make unique & sort
        self.track_percentiles = self.settings.get("percentiles", self.track_percentiles)
        self.track_percentiles = list(set(self.track_percentiles))
        self.track_percentiles.sort()
        self.settings["percentiles"] = self.track_percentiles
        
        self.set_aggregation(self.settings.get('extend-aggregation'))

        self.ignored_labels = self.settings.get("ignore-labels", self.ignored_labels)
        self.generalize_labels = self.settings.get("generalize-labels", self.generalize_labels)

        self.min_buffer_len = dehumanize_time(self.settings.get("min-buffer-len", self.min_buffer_len))

        max_buffer_len = self.settings.get("max-buffer-len", self.max_buffer_len)
        try:
            self.max_buffer_len = dehumanize_time(max_buffer_len)
        except TaurusInternalException as exc:
            self.log.debug("Exception in dehumanize_time(%s): %s", max_buffer_len, exc)
            raise TaurusConfigError("Wrong 'max-buffer-len' value: %s" % max_buffer_len)

        self.buffer_multiplier = self.settings.get("buffer-multiplier", self.buffer_multiplier)

        count = len(self.track_percentiles)
        if count == 1:
            self.buffer_scale_idx = str(float(self.track_percentiles[0]))
        if count > 1:
            percentile = self.settings.get("buffer-scale-choice", 0.5)
            percentiles = [i / (count - 1.0) for i in range(count)]
            distances = [abs(percentile - percentiles[i]) for i in range(count)]
            index_position = distances.index(min(distances))
            self.buffer_scale_idx = str(float(self.track_percentiles[index_position]))

        debug_str = 'Buffer scaling setup: percentile %s from %s selected'
        self.log.debug(debug_str, self.buffer_scale_idx, self.track_percentiles)
        self.histogram_max = dehumanize_time(self.settings.get("histogram-initial", self.histogram_max))
        self.max_error_count = self.settings.get("max-error-variety", self.max_error_count)

    def startup(self):
        super(Aggregator, self).startup()

        # send rules to underlings
        for underling in self.underlings:
            underling.set_aggregation(self._redundant_aggregation)

    def add_underling(self, underling):
        """
        Add source for aggregating

        :type underling: ResultsProvider
        """
        underling.track_percentiles = self.track_percentiles
        underling.ignored_labels = self.ignored_labels
        underling.min_buffer_len = self.min_buffer_len
        underling.max_buffer_len = self.max_buffer_len
        underling.buffer_multiplier = self.buffer_multiplier
        underling.buffer_scale_idx = self.buffer_scale_idx
        underling.histogram_max = self.histogram_max

        underling.max_error_count = self.max_error_count
        underling.generalize_labels = self.generalize_labels

        # share error set and label set between underlings
        underling.known_errors = self.known_errors
        underling.known_labels = self.known_labels

        self.underlings.append(underling)

    def check(self):
        """
        Check if there is next aggregate data present

        :rtype: bool
        """
        for point in self.datapoints():
            self.log.debug("Processed datapoint: %s/%s with %d labels",
                           point[DataPoint.TIMESTAMP], point[DataPoint.SOURCE_ID], len(point[DataPoint.CUMULATIVE]))
        return super(ConsolidatingAggregator, self).check()

    def post_process(self):
        """
        Process all remaining aggregate data
        """
        super(ConsolidatingAggregator, self).post_process()
        for point in self.datapoints(True):
            self.log.debug("Processed datapoint: %s/%s", point[DataPoint.TIMESTAMP], point[DataPoint.SOURCE_ID])

    def _process_underlings(self, final_pass):
        time_start = time.time()
        has_some_time = lambda x: time.time() - x < self.engine.check_interval
        while final_pass or has_some_time(time_start):
            had_data = False
            for underling in self.underlings:
                for point in underling.datapoints(final_pass):
                    had_data = True
                    self._put_into_buffer(point)

            if not had_data:
                break

    def _put_into_buffer(self, point):
        tstamp = point[DataPoint.TIMESTAMP]
        if self.buffer:
            mints = min(self.buffer.keys())
            if tstamp < mints:
                self.log.debug("Putting datapoint %s into %s", tstamp, mints)
                point[DataPoint.TIMESTAMP] = mints
                tstamp = mints
        self.buffer.setdefault(tstamp, []).append(point)

    def _get_max_ramp_up(self):
        ramp_ups = [0]
        for execution in self.engine.config['execution']:
            if 'ramp-up' in execution:
                ramp_ups.append(dehumanize_time(execution['ramp-up']))
        return max(ramp_ups)

    def _ramp_up_exclude(self):
        return self.engine.config.get('settings').get('ramp-up-exclude')

    def _calculate_datapoints(self, final_pass=False):
        """
        Override ResultsProvider._calculate_datapoints
        """
        self._process_underlings(final_pass)

        self.log.debug("Consolidator buffer[%s]: %s", len(self.buffer), self.buffer.keys())
        if not self.buffer:
            return

        timestamps = sorted(self.buffer.keys())
        while timestamps and (final_pass or (timestamps[-1] >= timestamps[0] + self.buffer_len)):
            tstamp = timestamps.pop(0)
            self.log.debug("Merging into %s", tstamp)
            points_to_consolidate = self.buffer.pop(tstamp)

            for subresult in points_to_consolidate:
                if self._ramp_up_exclude():
                    if not self.min_timestamp:
                        self.min_timestamp = subresult['ts']

                    if subresult['ts'] < self.min_timestamp + self._get_max_ramp_up():
                        subresult[DataPoint.CUMULATIVE] = dict()

                if not subresult[DataPoint.SOURCE_ID]:
                    raise ValueError("Reader must provide source ID for datapoint")
                self._sticky_concurrencies[subresult[DataPoint.SOURCE_ID]] = {
                    label: kpiset[KPISet.CONCURRENCY] for label, kpiset in iteritems(subresult[DataPoint.CURRENT])
                }

            if len(points_to_consolidate) == 1:
                self.log.debug("Bypassing consolidation because of single result")
                point = points_to_consolidate[0]
                point[DataPoint.SUBRESULTS] = [points_to_consolidate[0]]
            else:
                point = DataPoint(tstamp, self.track_percentiles)
                for subresult in points_to_consolidate:
                    self.log.debug("Merging %s", subresult[DataPoint.TIMESTAMP])
                    point.merge_point(subresult, do_recalculate=False)
                point.recalculate()

            current_sids = [x[DataPoint.SOURCE_ID] for x in point[DataPoint.SUBRESULTS]]
            for sid in self._sticky_concurrencies:
                if sid not in current_sids:
                    self.log.debug("Adding sticky concurrency for %s", sid)
                    self._add_sticky_concurrency(point, sid)

            point[DataPoint.SOURCE_ID] = self.__class__.__name__ + '@' + str(id(self))
            yield point

    def _add_sticky_concurrency(self, point, sid):
        concur = self._sticky_concurrencies[sid]
        for label, kpiset in iteritems(point[DataPoint.CURRENT]):  # type: (str, KPISet)
            if label in concur:
                kpiset.add_concurrency(concur[label], sid)


class NoneAggregator(Aggregator, ResultsProvider):
    """
    Dummy aggregator
    """

    def __init__(self):
        Aggregator.__init__(self, is_functional=False)
        ResultsProvider.__init__(self)

    def _calculate_datapoints(self, final_pass=False):
        pass


class AggregatorListener(object):
    """
    Mixin for listeners of aggregator data
    """

    @abstractmethod
    def aggregated_second(self, data):
        """
        Notification about new data point

        :param data: bzt.modules.reporting.DataPoint
        """
        pass

    def finalize(self):
        """
        This method is called at the end of run
        to close open file descriptors etc.
        """
        pass
