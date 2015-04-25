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
from collections import Counter
import copy
import logging
import math
import six

from bzt.utils import BetterDict
from bzt.engine import EngineModule


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
    RESP_TIMES = "rt"
    AVG_RESP_TIME = "avg_rt"
    STDEV_RESP_TIME = "stdev_rt"
    AVG_LATENCY = "avg_lt"
    AVG_CONN_TIME = "avg_ct"
    PERCENTILES = "perc"
    RESP_CODES = "rc"
    ERRTYPE_ERROR = 0
    ERRTYPE_ASSERT = 1

    def __init__(self, perc_levels=()):
        super(KPISet, self).__init__()
        self.sum_rt = 0
        self.sum_lt = 0
        self.sum_cn = 0
        self.perc_levels = perc_levels
        # scalars
        self.get(self.SAMPLE_COUNT, 0)
        self.get(self.CONCURRENCY, 0)
        self.get(self.SUCCESSES, 0)
        self.get(self.FAILURES, 0)
        self.get(self.AVG_RESP_TIME, 0)
        self.get(self.STDEV_RESP_TIME, 0)
        self.get(self.AVG_LATENCY, 0)
        self.get(self.AVG_CONN_TIME, 0)
        # vectors
        self.get(self.ERRORS, [])
        self.get(self.RESP_TIMES, Counter())
        self.get(self.RESP_CODES, Counter())
        self.get(self.PERCENTILES)
        self._concurrencies = BetterDict()

    def __deepcopy__(self, memo):
        mycopy = KPISet(self.perc_levels)
        mycopy.sum_rt = self.sum_rt
        mycopy.sum_lt = self.sum_lt
        mycopy.sum_cn = self.sum_cn
        for key, val in six.iteritems(self):
            mycopy[key] = copy.deepcopy(val, memo)
        return mycopy

    @staticmethod
    def error_item_skel(error, rc, cnt, errtype, urls):
        """

        :type error: str
        :type rc: str
        :type cnt: int
        :type errtype: int
        :type urls: Counter
        :rtype: dict
        """
        return {
            "cnt": cnt,
            "msg": error,
            "rc": rc,
            "type": errtype,
            "urls": urls
        }

    def add_sample(self, sample):
        """
        Add sample, consisting of: cnc, rt, cn, lt, rc, error

        :type sample: tuple
        """
        # TODO: introduce a flag to not count failed in resp times? or offer it always?
        cnc, rt, cn, lt, rc, error = sample
        if cn:
            self.sum_cn += cn
        self.sum_lt += lt
        self.sum_rt += rt
        self[self.SAMPLE_COUNT] = self.get(self.SAMPLE_COUNT, 0) + 1
        if cnc:  # NOTE: maybe should find a way to handle it more gracefully
            self[self.CONCURRENCY] = cnc
        resp_codes = self.get(self.RESP_CODES)
        resp_codes[rc] = resp_codes.get(rc, 0) + 1
        if error is not None:
            self[self.FAILURES] = self.get(self.FAILURES, 0) + 1

            item = self.error_item_skel(error, rc, 1, KPISet.ERRTYPE_ERROR, Counter())
            self.inc_list(self.get(self.ERRORS), ("msg", error), item)
        else:
            self[self.SUCCESSES] = self.get(self.SUCCESSES, 0) + 1

        self.get(self.RESP_TIMES)[rt] += 1
        # TODO: max/min rt? there is percentiles...
        # TODO: throughput if interval is not 1s

    @staticmethod
    def inc_list(values, selector, value):
        """
        Increment list item, based on selector criteria

        :param values: list to update
        :type values: list
        :param selector: tuple of 2 values, field name and value to match
        :type selector: tuple
        :param value: dict to put into list
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
            values.append(value)

    def recalculate(self):
        """
        Recalculate averages, stdev and percentiles

        :return:
        """
        if self[self.SAMPLE_COUNT]:
            self[self.AVG_CONN_TIME] = self.sum_cn / self[self.SAMPLE_COUNT]
            self[self.AVG_LATENCY] = self.sum_lt / self[self.SAMPLE_COUNT]
            self[self.AVG_RESP_TIME] = self.sum_rt / self[self.SAMPLE_COUNT]

        if self._concurrencies:
            self[self.CONCURRENCY] = sum(self._concurrencies.values())

        perc, stdev = self.__perc_and_stdev(self[self.RESP_TIMES], self.perc_levels, self[self.AVG_RESP_TIME])
        for level, val in perc:
            self[self.PERCENTILES][str(float(level))] = val

        self[self.STDEV_RESP_TIME] = stdev

        return self

    def merge_kpis(self, src, sid=None):
        """
        Merge other instance into self

        :param sid: source ID to use when suming up concurrency
        :type src: KPISet
        :return:
        """
        self.sum_cn += src.sum_cn
        self.sum_lt += src.sum_lt
        self.sum_rt += src.sum_rt

        self[self.SAMPLE_COUNT] += src[self.SAMPLE_COUNT]
        self[self.SUCCESSES] += src[self.SUCCESSES]
        self[self.FAILURES] += src[self.FAILURES]
        # NOTE: should it be average? mind the timestamp gaps
        self._concurrencies[sid] = src[self.CONCURRENCY]

        self[self.RESP_TIMES].update(src[self.RESP_TIMES])
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
        for key, val in six.iteritems(obj):
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
        assert all(0 <= p <= 100 for p in percentiles_to_calc)
        percentiles = []
        if not cnts_dict:
            return percentiles, 0

        num = sum(cnts_dict.values())
        cnts = sorted(cnts_dict.items())
        curr_cnts_pos = 0  # current position in cnts
        curr_pos = cnts[0][1]  # sum of freqs up to current_cnts_pos

        sqr_diffs = 0
        for p in sorted(percentiles_to_calc):
            if p < 100:
                percentile_pos = p / 100.0 * num
                while curr_pos <= percentile_pos and curr_cnts_pos < len(cnts):
                    sqr_diffs += cnts[curr_cnts_pos][1] * math.pow(cnts[curr_cnts_pos][0] - avg, 2)

                    curr_cnts_pos += 1
                    curr_pos += cnts[curr_cnts_pos][1]

                percentiles.append((p, cnts[curr_cnts_pos][0]))
            else:
                percentiles.append((p, cnts[-1][0]))  # we could add a small value

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
        super(DataPoint, self).__init__()
        self.perc_levels = perc_levels
        self[self.SOURCE_ID] = None
        self[self.TIMESTAMP] = ts
        self[self.CUMULATIVE] = BetterDict()
        self[self.CURRENT] = BetterDict()
        self[self.SUBRESULTS] = []

    def __merge_kpis(self, src, dst, sid):
        for label, val in six.iteritems(src):
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
            self.log.warning("Tried to merge data for %s and %s", self[self.TIMESTAMP], src[self.TIMESTAMP])
            raise ValueError("Cannot merge different timestamps")

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

    def add_listener(self, listener):
        """
        Add aggregate results listener

        :type listener: AggregatorListener
        """
        self.listeners.append(listener)

    def __merge_to_cumulative(self, current):
        for label, data in six.iteritems(current):
            cumul = self.cumulative.get(label, KPISet(self.track_percentiles))
            cumul.merge_kpis(data)

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

    def _calculate_datapoints(self, final_pass=False):
        raise NotImplementedError()


class ResultsReader(ResultsProvider):
    """
    Aggregator that reads samples one by one,
    supposed to be attached to every executor
    """

    def __init__(self, perc_levels=()):
        super(ResultsReader, self).__init__()
        self.ignored_labels = []
        self.log = logging.getLogger(self.__class__.__name__)
        self.buffer = {}
        self.buffer_len = 2
        self.min_timestamp = 0
        self.track_percentiles = perc_levels

    def __process_readers(self, final_pass=False):
        for result in self._read(final_pass):
            if result is None:
                self.log.debug("No data from reader")
                break
            elif isinstance(result, list) or isinstance(result, tuple):
                ts, label, conc, rt, cn, lt, rc, error = result
                if label in self.ignored_labels:
                    continue
                if ts < self.min_timestamp:
                    self.log.warning("Putting %s into %s", ts, self.min_timestamp)
                    ts = self.min_timestamp
                if ts not in self.buffer:
                    self.buffer[ts] = []
                self.buffer[ts].append((label, conc, rt, cn, lt, rc, error))
            else:
                raise ValueError("Unsupported results from reader: %s" % result)

    def __aggreagate_current(self, datapoint, samples):
        current = datapoint[DataPoint.CURRENT]
        for sample in samples:
            label, rt, concur, cn, lt, rc, error = sample
            # TODO: replace digit/uuid sequences in sample names, like in LS, have option for it
            if label == '':
                label = '[empty]'
            label = current.get(label, KPISet(self.track_percentiles))
            # empty means overall
            label.add_sample((rt, concur, cn, lt, rc, error))
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
        timestamps = sorted(self.buffer.keys())
        while final_pass or (timestamps[-1] >= timestamps[0] + self.buffer_len):
            timestamp = timestamps.pop(0)
            self.min_timestamp = timestamp + 1  # NOTE: why +1?
            self.log.debug("Aggregating: %s", timestamp)
            samples = self.buffer.pop(timestamp)
            datapoint = self.__get_new_datapoint(timestamp)
            self.__aggreagate_current(datapoint, samples)
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

    def _read(self, final_pass=False):
        """

        :param final_pass: True if called from post-process stage, when reader
            should report possible rests of results
        :rtype: iterable
        :return: timestamp, label, concurrency, rt, latency, rc, error
        """
        raise NotImplementedError()


class ConsolidatingAggregator(EngineModule, ResultsProvider):
    """

    :type underlings: list[bzt.modules.aggregator.ResultsProvider]
    """
    # FIXME: it was oscillating with remote test of 100 servers
    def __init__(self):
        EngineModule.__init__(self)
        ResultsProvider.__init__(self)
        self.ignored_labels = []
        self.underlings = []
        self.buffer = BetterDict()
        self.buffer_len = 2

    def prepare(self):
        """
        Read aggregation options
        """
        super(ConsolidatingAggregator, self).prepare()
        self.track_percentiles = self.settings.get("percentiles", self.track_percentiles)
        self.buffer_len = self.settings.get("buffer-seconds", self.buffer_len)
        self.ignored_labels = self.settings.get("ignore-labels", ["ignore"])

    def add_underling(self, underling):
        """
        Add source for aggregating

        :type underling: ResultsProvider
        """
        underling.track_percentiles = self.track_percentiles
        if isinstance(underling, ResultsReader):
            underling.ignored_labels = self.ignored_labels
        # TODO: how to pass buffer len to underling?
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
            for data in [x for x in underling.datapoints(final_pass)]:
                tstamp = data[DataPoint.TIMESTAMP]
                if self.buffer:
                    mints = min(self.buffer.keys())
                    if tstamp < mints:
                        self.log.warning("Putting %s into %s", tstamp, mints)
                        data[DataPoint.TIMESTAMP] = mints
                        tstamp = mints
                self.buffer.get(tstamp, []).append(data)

    def _calculate_datapoints(self, final_pass=False):
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


class NoneAggregator(EngineModule, ResultsProvider):
    """
    Dummy aggregator
    """

    def __init__(self):
        EngineModule.__init__(self)
        ResultsProvider.__init__(self)

    def _calculate_datapoints(self, final_pass=False):
        pass