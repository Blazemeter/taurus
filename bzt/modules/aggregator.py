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
import copy
import logging
import math
import operator
import re
from abc import abstractmethod
from collections import Counter

from bzt import TaurusInternalException, TaurusConfigError
from bzt.engine import Aggregator
from bzt.six import iteritems
from bzt.utils import BetterDict, dehumanize_time


class KPISet(BetterDict):
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

    def __init__(self, perc_levels=(), rt_dist_maxlen=None):
        super(KPISet, self).__init__()
        self.sum_rt = 0
        self.sum_lt = 0
        self.sum_cn = 0
        self.perc_levels = perc_levels
        self.rtimes_len = rt_dist_maxlen
        # scalars
        self.get(self.SAMPLE_COUNT, 0)
        self.get(self.CONCURRENCY, 0)
        self.get(self.SUCCESSES, 0)
        self.get(self.FAILURES, 0)
        self.get(self.AVG_RESP_TIME, 0)
        self.get(self.STDEV_RESP_TIME, 0)
        self.get(self.AVG_LATENCY, 0)
        self.get(self.AVG_CONN_TIME, 0)
        self.get(self.BYTE_COUNT, 0)
        # vectors
        self.get(self.ERRORS, [])
        self.get(self.RESP_TIMES, Counter())
        self.get(self.RESP_CODES, Counter())
        self.get(self.PERCENTILES)
        self._concurrencies = BetterDict()  # NOTE: shouldn't it be Counter?

    def __deepcopy__(self, memo):
        mycopy = KPISet(self.perc_levels)
        mycopy.sum_rt = self.sum_rt
        mycopy.sum_lt = self.sum_lt
        mycopy.sum_cn = self.sum_cn
        mycopy.rtimes_len = self.rtimes_len
        for key, val in iteritems(self):
            mycopy[key] = copy.deepcopy(val, memo)
        return mycopy

    @staticmethod
    def error_item_skel(error, ret_c, cnt, errtype, urls):
        """

        :type error: str
        :type ret_c: str
        :type cnt: int
        :type errtype: int
        :type urls: collections.Counter
        :rtype: dict
        """
        return {
            "cnt": cnt,
            "msg": error,
            "rc": ret_c,
            "type": errtype,
            "urls": urls
        }

    def add_sample(self, sample):
        """
        Add sample, consisting of: cnc, rt, cn, lt, rc, error, trname, byte_count

        :type sample: tuple
        """
        # TODO: introduce a flag to not count failed in resp times? or offer it always?
        cnc, r_time, con_time, latency, r_code, error, trname, byte_count = sample
        self[self.SAMPLE_COUNT] += 1
        if cnc:
            self._concurrencies[trname] = cnc

        if r_code is not None:
            self[self.RESP_CODES][r_code] += 1

            # count times only if we have RCs
            if con_time:
                self.sum_cn += con_time
            self.sum_lt += latency
            self.sum_rt += r_time

        if error is not None:
            self[self.FAILURES] += 1

            item = self.error_item_skel(error, r_code, 1, KPISet.ERRTYPE_ERROR, Counter())
            self.inc_list(self[self.ERRORS], ("msg", error), item)
        else:
            self[self.SUCCESSES] += 1

        self[self.RESP_TIMES][r_time] += 1

        if byte_count is not None:
            self[self.BYTE_COUNT] += byte_count
            # TODO: max/min rt? there is percentiles...
            # TODO: throughput if interval is not 1s

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

    def recalculate(self):
        """
        Recalculate averages, stdev and percentiles

        :return:
        """
        if self[self.SAMPLE_COUNT]:
            self[self.AVG_CONN_TIME] = self.sum_cn / self[self.SAMPLE_COUNT]
            self[self.AVG_LATENCY] = self.sum_lt / self[self.SAMPLE_COUNT]
            self[self.AVG_RESP_TIME] = self.sum_rt / self[self.SAMPLE_COUNT]

        if len(self._concurrencies):
            self[self.CONCURRENCY] = sum(self._concurrencies.values())

        perc, stdev = self.__perc_and_stdev(self[self.RESP_TIMES], self.perc_levels, self[self.AVG_RESP_TIME])
        for level, val in perc:
            self[self.PERCENTILES][str(float(level))] = val

        self[self.STDEV_RESP_TIME] = stdev

        return self

    def compact_times(self):
        if not self.rtimes_len:
            return

        times = self[KPISet.RESP_TIMES]
        redundant_cnt = len(times) - self.rtimes_len
        if redundant_cnt > 0:
            logging.debug("Compacting %s response timing into %s", len(times), self.rtimes_len)

        while redundant_cnt > 0:
            keys = sorted(times.keys())
            distances = [(lidx, keys[lidx + 1] - keys[lidx]) for lidx in range(len(keys) - 1)]
            distances.sort(key=operator.itemgetter(1))  # sort by distance

            # cast candidates for consolidation
            lkeys_indexes = [lidx for lidx, _ in distances[:redundant_cnt]]

            while lkeys_indexes:
                lidx = lkeys_indexes.pop(0)
                lkey = keys[lidx]
                rkey = keys[lidx + 1]
                if lkey in times and rkey in times:  # neighbours aren't changed
                    lval = times.pop(lkey)
                    rval = times.pop(rkey)

                    # shift key proportionally to values
                    idx_new = lkey + (rkey - lkey) * float(rval) / (lval + rval)

                    # keep precision the same
                    lprec = len(str(math.modf(lkey)[0])) - 2
                    rprec = len(str(math.modf(rkey)[0])) - 2
                    idx_new = round(idx_new, max(lprec, rprec))

                    times[idx_new] = lval + rval
                    redundant_cnt -= 1

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
        if src[self.CONCURRENCY]:
            self._concurrencies[sid] = src[self.CONCURRENCY]

        if src[self.RESP_TIMES]:
            # using raw times to calculate percentiles
            self[self.RESP_TIMES].update(src[self.RESP_TIMES])
            self.compact_times()
        elif not self[self.PERCENTILES]:
            # using existing percentiles
            # FIXME: it's not valid to overwrite, better take average
            self[self.PERCENTILES] = copy.deepcopy(src[self.PERCENTILES])

        self[self.RESP_CODES].update(src[self.RESP_CODES])

        for src_item in src[self.ERRORS]:
            self.inc_list(self[self.ERRORS], ('msg', src_item['msg']), src_item)

    @staticmethod
    def from_dict(obj):
        """

        :type obj: dict
        :rtype: KPISet
        """
        inst = KPISet()
        for key, val in iteritems(obj):
            inst[key] = val
        inst.sum_cn = obj[inst.AVG_CONN_TIME] * obj[inst.SAMPLE_COUNT]
        inst.sum_lt = obj[inst.AVG_LATENCY] * obj[inst.SAMPLE_COUNT]
        inst.sum_rt = obj[inst.AVG_RESP_TIME] * obj[inst.SAMPLE_COUNT]
        inst.perc_levels = [float(x) for x in inst[inst.PERCENTILES].keys()]
        inst[inst.RESP_TIMES] = {float(level): inst[inst.RESP_TIMES][level] for level in inst[inst.RESP_TIMES].keys()}
        for error in inst[KPISet.ERRORS]:
            error['urls'] = Counter(error['urls'])
        return inst

    @staticmethod
    def __perc_and_stdev(cnts_dict, percentiles_to_calc=(), avg=0):
        """
        from http://stackoverflow.com/questions/25070086/percentiles-from-counts-of-values
        Returns [(percentile, value)] with nearest rank percentiles.
        Percentile 0: <min_value>, 100: <max_value>.
        cnts_dict: { <value>: <count> }
        percentiles_to_calc: iterable for percentiles to calculate; 0 <= ~ <= 100

        upd: added stdev calc to have it in single-pass for mans of efficiency

        :type percentiles_to_calc: list(float)
        :type cnts_dict: collections.Counter
        """
        assert all(0 <= percentile <= 100 for percentile in percentiles_to_calc)
        percentiles = []
        if not cnts_dict:
            return percentiles, 0

        num = sum(cnts_dict.values())
        cnts = sorted(cnts_dict.items())
        curr_cnts_pos = 0  # current position in cnts
        curr_pos = cnts[0][1]  # sum of freqs up to current_cnts_pos

        sqr_diffs = 0
        for percentile in sorted(percentiles_to_calc):
            if percentile < 100:
                percentile_pos = percentile / 100.0 * num
                while curr_pos <= percentile_pos and curr_cnts_pos < len(cnts):
                    sqr_diffs += cnts[curr_cnts_pos][1] * math.pow(cnts[curr_cnts_pos][0] - avg, 2)

                    curr_cnts_pos += 1
                    curr_pos += cnts[curr_cnts_pos][1]

                percentiles.append((percentile, cnts[curr_cnts_pos][0]))
            else:
                percentiles.append((percentile, cnts[-1][0]))  # we could add a small value

        while curr_cnts_pos < len(cnts):
            sqr_diffs += cnts[curr_cnts_pos][1] * math.pow(cnts[curr_cnts_pos][0] - avg, 2)
            curr_cnts_pos += 1

        stdev = math.sqrt(sqr_diffs / len(cnts))
        return percentiles, stdev


class DataPoint(BetterDict):
    """
    Represents an aggregate data poing

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
        self[self.CUMULATIVE] = BetterDict()
        self[self.CURRENT] = BetterDict()
        self[self.SUBRESULTS] = []

    def __deepcopy__(self, memo):
        new = DataPoint(self[self.TIMESTAMP], self.perc_levels)
        for key in self.keys():
            new[key] = copy.deepcopy(self[key], memo)
        return new

    def __merge_kpis(self, src, dst, sid):
        """
        :param src: KPISet
        :param dst: KPISet
        :param sid: int
        :return:
        """
        for label, val in iteritems(src):
            dest = dst.get(label, KPISet(self.perc_levels))
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

    def merge_point(self, src):
        """

        :type src: DataPoint
        """
        if self[self.TIMESTAMP] != src[self.TIMESTAMP]:
            msg = "Cannot merge different timestamps (%s and %s)"
            raise TaurusInternalException(msg % (self[self.TIMESTAMP], src[self.TIMESTAMP]))

        self[DataPoint.SUBRESULTS].append(src)

        self.__merge_kpis(src[self.CURRENT], self[self.CURRENT], src[DataPoint.SOURCE_ID])
        self.__merge_kpis(src[self.CUMULATIVE], self[self.CUMULATIVE], src[DataPoint.SOURCE_ID])

        self.recalculate()


class ResultsProvider(object):
    """
    :type listeners: list[AggregatorListener]
    """

    def __init__(self):
        super(ResultsProvider, self).__init__()
        self.cumulative = BetterDict()
        self.track_percentiles = []
        self.listeners = []
        self.buffer_len = 2
        self.min_buffer_len = 2
        self.max_buffer_len = float('inf')
        self.buffer_multiplier = 2
        self.buffer_scale_idx = None
        self.rtimes_len = None

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
            cumul = self.cumulative.get(label, KPISet(self.track_percentiles, self.rtimes_len))
            cumul.merge_kpis(data)
            cumul.compact_times()
            cumul.recalculate()

    def datapoints(self, final_pass=False):
        """
        Generator object that returns datapoints from the reader

        :type final_pass: bool
        """
        for datapoint in self._calculate_datapoints(final_pass):
            current = datapoint[DataPoint.CURRENT]
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


class ResultsReader(ResultsProvider):
    """
    Aggregator that reads samples one by one,
    supposed to be attached to every executor
    """
    label_generalize_regexps = [
        (re.compile(r"\b[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\b"), "U"),
        (re.compile(r"\b[0-9a-fA-F]{2,}\b"), "U"),
        # (re.compile(r"\b[0-9a-fA-F]{32}\b"), "U"), # implied by previous, maybe prev is too wide
        (re.compile(r"\b\d{2,}\b"), "N")
    ]

    def __init__(self, perc_levels=()):
        super(ResultsReader, self).__init__()
        self.generalize_labels = False
        self.ignored_labels = []
        self.log = logging.getLogger(self.__class__.__name__)
        self.buffer = {}
        self.min_timestamp = 0
        self.track_percentiles = perc_levels

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

                if label in self.ignored_labels:
                    continue
                if t_stamp < self.min_timestamp:
                    self.log.debug("Putting sample %s into %s", t_stamp, self.min_timestamp)
                    t_stamp = self.min_timestamp

                if t_stamp not in self.buffer:
                    self.buffer[t_stamp] = []
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
            label, r_time, concur, con_time, latency, r_code, error, trname, byte_count = sample
            if label == '':
                label = '[empty]'

            if self.generalize_labels:
                label = self.__generalize_label(label)

            if label in current:
                label = current[label]
            else:
                label = current.get(label, KPISet(self.track_percentiles))

            # empty means overall
            label.add_sample((r_time, concur, con_time, latency, r_code, error, trname, byte_count))
        overall = KPISet(self.track_percentiles)
        for label in current.values():
            overall.merge_kpis(label, datapoint[DataPoint.SOURCE_ID])
        current[''] = overall
        return current

    def _calculate_datapoints(self, final_pass=False):
        """
        A generator to read available datapoints

        :type final_pass: bool
        :rtype: DataPoint
        """
        self.__process_readers(final_pass)

        self.log.debug("Buffer len: %s", len(self.buffer))
        if not self.buffer:
            return

        if self.cumulative and self.track_percentiles:
            old_len = self.buffer_len
            chosen_timing = self.cumulative[''][KPISet.PERCENTILES][self.buffer_scale_idx]
            self.buffer_len = round(chosen_timing * self.buffer_multiplier)

            self.buffer_len = max(self.min_buffer_len, self.buffer_len)
            self.buffer_len = min(self.max_buffer_len, self.buffer_len)
            if self.buffer_len != old_len:
                self.log.info("Changed data analysis delay to %ds", self.buffer_len)

        timestamps = sorted(self.buffer.keys())
        while final_pass or (timestamps[-1] >= (timestamps[0] + self.buffer_len)):
            timestamp = timestamps.pop(0)
            self.min_timestamp = timestamp + 1
            self.log.debug("Aggregating: %s", timestamp)
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

    def __generalize_label(self, label):
        for regexp, replacement in self.label_generalize_regexps:
            label = regexp.sub(replacement, label)

        return label


class ConsolidatingAggregator(Aggregator, ResultsProvider):
    """

    :type underlings: list[bzt.modules.aggregator.ResultsProvider]
    """

    # TODO: switch to underling-count-based completeness criteria
    def __init__(self):
        Aggregator.__init__(self, is_functional=False)
        ResultsProvider.__init__(self)
        self.generalize_labels = False
        self.ignored_labels = []
        self.underlings = []
        self.buffer = BetterDict()
        self.rtimes_len = 1000

    def prepare(self):
        """
        Read aggregation options
        """
        super(ConsolidatingAggregator, self).prepare()

        # make unique & sort
        percentiles = self.settings.get("percentiles", self.track_percentiles)
        percentiles = list(set(percentiles))
        percentiles.sort()
        self.track_percentiles = percentiles
        self.settings['percentiles'] = percentiles

        self.ignored_labels = self.settings.get("ignore-labels", self.ignored_labels)
        self.generalize_labels = self.settings.get("generalize-labels", self.generalize_labels)

        self.min_buffer_len = dehumanize_time(self.settings.get("min-buffer-len", self.min_buffer_len))

        max_buffer_len = self.settings.get("max-buffer-len", self.max_buffer_len)
        try:  # for max_buffer_len == float('inf')
            self.max_buffer_len = dehumanize_time(max_buffer_len)
        except TaurusInternalException as exc:
            self.log.debug("Exception in dehumanize_time(%s)" % max_buffer_len)
            if str(exc).find('inf') != -1:
                self.max_buffer_len = max_buffer_len
            else:
                raise TaurusConfigError("Wrong 'max-buffer-len' value: %s" % max_buffer_len)

        self.buffer_multiplier = self.settings.get("buffer-multiplier", self.buffer_multiplier)

        percentile = self.settings.get("buffer-scale-choice", 0.5)
        count = len(self.track_percentiles)
        if count == 1:
            self.buffer_scale_idx = str(float(self.track_percentiles[0]))
        if count > 1:
            percentiles = [i / (count - 1.0) for i in range(count)]
            distances = [abs(percentile - percentiles[i]) for i in range(count)]
            index_position = distances.index(min(distances))
            self.buffer_scale_idx = str(float(self.track_percentiles[index_position]))

        debug_str = 'Buffer scaling setup: percentile %s from %s selected'
        self.log.debug(debug_str, self.buffer_scale_idx, self.track_percentiles)
        self.rtimes_len = self.settings.get("rtimes-len", self.rtimes_len)

    def add_underling(self, underling):
        """
        Add source for aggregating

        :type underling: ResultsProvider
        """
        underling.track_percentiles = self.track_percentiles
        if isinstance(underling, ResultsReader):
            underling.ignored_labels = self.ignored_labels
            underling.generalize_labels = self.generalize_labels
            underling.min_buffer_len = self.min_buffer_len
            underling.max_buffer_len = self.max_buffer_len
            underling.buffer_multiplier = self.buffer_multiplier
            underling.buffer_scale_idx = self.buffer_scale_idx
            underling.rtimes_len = self.rtimes_len

        self.underlings.append(underling)

    def check(self):
        """
        Check if there is next aggregate data present

        :rtype: bool
        """
        for point in self.datapoints():
            self.log.debug("Processed datapoint: %s/%s", point[DataPoint.TIMESTAMP], point[DataPoint.SOURCE_ID])
        return super(ConsolidatingAggregator, self).check()

    def post_process(self):
        """
        Process all remaining aggregate data
        """
        super(ConsolidatingAggregator, self).post_process()
        for point in self.datapoints(True):
            self.log.debug("Processed datapoint: %s/%s", point[DataPoint.TIMESTAMP], point[DataPoint.SOURCE_ID])

    def _process_underlings(self, final_pass):
        for underling in self.underlings:
            for data in underling.datapoints(final_pass):
                tstamp = data[DataPoint.TIMESTAMP]
                if self.buffer:
                    mints = min(self.buffer.keys())
                    if tstamp < mints:
                        self.log.debug("Putting datapoint %s into %s", tstamp, mints)
                        data[DataPoint.TIMESTAMP] = mints
                        tstamp = mints
                self.buffer.get(tstamp, []).append(data)

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
            point = DataPoint(tstamp, self.track_percentiles)
            for subresult in points_to_consolidate:
                self.log.debug("Merging %s", subresult[DataPoint.TIMESTAMP])
                point.merge_point(subresult)
            point.recalculate()
            yield point


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
