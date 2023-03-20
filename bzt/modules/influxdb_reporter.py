"""
Influxdb reporter for Taurus stats

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

import sys

import bzt
import time

from influxdb import InfluxDBClient

from bzt.engine import Reporter, Singletone
from bzt.modules.aggregator import DataPoint, KPISet, AggregatorListener, ResultsProvider
from bzt.utils import iteritems
from influxdb.exceptions import InfluxDBServerError, InfluxDBClientError


class InfluxdbStatusReporter(Reporter, AggregatorListener, Singletone):

    METRIC_COUNT = "count"
    METRIC_COUNT_ERROR = "countError"
    METRIC_HIT = "hit"
    METRIC_AVG = "avg"
    METRIC_RECEIVED_BYTES = "rb"
    METRIC_PCT_PREFIX = "pct"
    METRIC_MEAN_ACTIVE_THREADS = "meanAT"
    TAG_TRANSACTION = "transaction"
    TAG_STATUS = "status"
    TAG_APPLICATION = "application"
    TAG_RESPONSE_CODE = "responseCode"
    TAG_RESPONSE_MESSAGE = "responseMessage"
    TAG_OK = "ok"
    TAG_KO = "ko"
    TAG_ALL = "all"  # used for cumulated metrics

    """
    A reporter that send statistic status on test end to an influxdb datasource
    """

    def __init__(self):
        super(InfluxdbStatusReporter, self).__init__()
        self._common_tags = None
        self._client = None
        self.application = None
        self.measurement = None
        self._timestamp_serie = int(time.time())
        self.timeout = 30
        self.cumulative_results = None
        self.last_sec = None
        self.start_time = time.time()
        self.end_time = time.time()
        self.first_ts = sys.maxsize
        self.last_ts = 0
        self.kpi_buffer = []
        self.send_interval = 30
        self.send_data = True
        self._last_status_check = time.time()
        self._last_dispatch = 0

    def startup(self):
        self.start_time = time.time()

    def shutdown(self):
        self.end_time = time.time()

    def prepare(self):
        super(InfluxdbStatusReporter, self).prepare()

        self.send_data = self.parameters.get("send-data", self.send_data)
        self.application = self.parameters.get("application", self.settings.get("application", self.application))
        self.measurement = self.settings.get("measurement", None)
        host = self.settings.get("host", 'localhost')
        port = self.settings.get("port", 8086)
        db = self.settings.get("database", None)
        username = self.settings.get("username", None)
        password = self.settings.get("password", None)
        self._client = InfluxDBClient(host, port, username, password, db)

        # Build common tags
        self._common_tags = self.__compute_common_tags()

        # test connection
        if self.send_data:
            try:
                self._client.ping()  # check connectivity and auth
            except (InfluxDBServerError, InfluxDBClientError):
                self.log.error("Cannot reach influxdb, please check your settings.")
                raise

        if isinstance(self.engine.aggregator, ResultsProvider):
            self.engine.aggregator.add_listener(self)

    def post_process(self):
        super(InfluxdbStatusReporter, self).post_process()

        self.log.debug("Proceeding to %s KPI remaining report ...", len(self.kpi_buffer))

        self.log.info("Sending remaining KPI data to server...")
        self.__send_data(self.kpi_buffer)
        self.kpi_buffer = []

    # from AggregatorListener
    def aggregated_second(self, data):
        """
        Just store the latest info
        :type data: bzt.modules.aggregator.DataPoint
        """
        self.kpi_buffer.append(data)

    def check(self):
        """
        Send data if any in buffer at fixed interval
        """
        self.log.debug("KPI bulk buffer len: %s", len(self.kpi_buffer))
        if self._last_dispatch < (time.time() - self.send_interval):
            self._last_dispatch = time.time()
            if self.send_data and len(self.kpi_buffer):
                self.__send_data(self.kpi_buffer)
                self.kpi_buffer = []

        return super(InfluxdbStatusReporter, self).check()

    def __send_data(self, data):
        # depends on aggregator
        self.engine.aggregator.converter(data)
        series = self.__compute_data_points(data)

        if self.send_data:
            self.__send_series(series)

    def __compute_data_points(self, data):
        dps = []
        if data:
            # following data is received in the cumulative way
            # processing from last to first
            agg = data[-1]
            time_stamp = agg[DataPoint.TIMESTAMP]
            for label, kpi_set in iteritems(agg[DataPoint.CUMULATIVE]):
                if label == "":  # process total cumulated label
                    dps.extend(self.__compute_cumulative(kpi_set, time_stamp))

            # processing detailed metric for each label
            for dpoint in data:
                time_stamp = dpoint[DataPoint.TIMESTAMP]
                for label, kpi_set in iteritems(dpoint[DataPoint.CURRENT]):
                    self.log.debug('Computing timestamped metric for \'%s\' at %s', label, time_stamp)
                    dps.extend(self.__compute_current(label, kpi_set, time_stamp))

        return dps

    def __compute_cumulative(self, data, ts):
        series = []
        series.extend(self.__report_internal_metrics(data, '', ts))
        series.extend(self.__report_bytes(data, '', ts))
        # merge tags & common tags
        self.__merge_tags(series, self._common_tags)
        return series

    def __compute_current(self, label, data, ts):
        series = []

        series.extend(self.__report_detailed_labels(data, label, ts))

        # merge tags & common tags
        self.__merge_tags(series, self._common_tags)
        return series

    def __report_samples_count(self, kpi_set, sample_label=None):
        """
        reports samples count
        :kpi_set
        :returns metrics [dict, Any]
        """
        metrics = []
        transaction = self.TAG_ALL if sample_label == '' else sample_label
        # total count for the sample
        if kpi_set[KPISet.SAMPLE_COUNT]:
            metrics.append({
                "time": self._timestamp_serie,
                "measurement": self.measurement,
                "fields": {
                    self.METRIC_COUNT: kpi_set[KPISet.SAMPLE_COUNT],
                },
                "tags": {
                    self.TAG_TRANSACTION: transaction
                }
            })
            metrics.append({
                "time": self._timestamp_serie,
                "measurement": self.measurement,
                "fields": {
                    self.METRIC_HIT: kpi_set[KPISet.SAMPLE_COUNT],
                },
                "tags": {
                    self.TAG_TRANSACTION: transaction
                }
            })

        if kpi_set[KPISet.FAILURES]:
            metrics.append({
                "time": self._timestamp_serie,
                "measurement": self.measurement,
                "fields": {
                    self.METRIC_COUNT_ERROR: kpi_set[KPISet.FAILURES],
                },
                "tags": {
                    self.TAG_TRANSACTION: transaction
                }
            })

        return metrics

    def __report_detailed_labels(self, kpi_set, sample_label=None, ts=None):
        """
        reports detailed transactions
        kpi_set: DataPoint
        """
        metrics = []
        if sample_label != "":  # ignore cumulated dp
            """
            reports all transactions
            """
            metrics.append({
                "time": ts,
                "measurement": self.measurement,
                "fields": {
                    self.METRIC_COUNT: kpi_set['throughput'],
                    self.METRIC_AVG: kpi_set[KPISet.AVG_RESP_TIME]
                },
                "tags": {
                    self.TAG_TRANSACTION: sample_label,
                }
            })

            """
            reports successful transactions
            """
            successful_samples_count = kpi_set['succ']
            if successful_samples_count:
                metrics.append({
                    "time": ts,
                    "measurement": self.measurement,
                    "fields": {
                        self.METRIC_COUNT: successful_samples_count,
                    },
                    "tags": {
                        self.TAG_TRANSACTION: sample_label,
                        self.TAG_STATUS: self.TAG_OK
                    }
                })

            """
            reports failed transactions
            """
            failed_samples_count = kpi_set['fail']
            if failed_samples_count:
                for error in kpi_set['errors']:
                    metrics.append({
                        "time": ts,
                        "measurement": self.measurement,
                        "fields": {
                            self.METRIC_COUNT: error['cnt'],
                        },
                        "tags": {
                            self.TAG_TRANSACTION: sample_label,
                            self.TAG_STATUS: self.TAG_KO,
                            self.TAG_RESPONSE_CODE: error['rc'],
                            self.TAG_RESPONSE_MESSAGE: error['msg'] if 'msg' in error else None
                        }
                    })
            """
            reports percentiles
            """
            if self.parameters.get("percentiles", True):
                for key in sorted(kpi_set[KPISet.PERCENTILES].keys(), key=float):
                    metrics.append({
                        "time": ts,
                        "measurement": self.measurement,
                        "fields": {
                            self.METRIC_PCT_PREFIX + str(float(key)): kpi_set[KPISet.PERCENTILES][key],
                        },
                        "tags": {
                            self.TAG_TRANSACTION: sample_label
                        }
                    })
        return metrics

    def __report_internal_metrics(self, kpi_set, sample_label=None, ts=None):
        """
        reports internals (active users..)
        """
        metrics = []
        transaction = self.TAG_ALL if sample_label == '' else sample_label
        # average number of Virtual Users
        if kpi_set[KPISet.CONCURRENCY]:
            metrics.append({
                "time": ts,
                "measurement": self.measurement,
                "fields": {
                    self.METRIC_MEAN_ACTIVE_THREADS: kpi_set[KPISet.CONCURRENCY],
                },
                "tags": {
                    self.TAG_TRANSACTION: transaction
                }
            })
        return metrics

    def __report_bytes(self, kpi_set, sample_label=None, ts=None):
        """
        reports bytes received
        :returns: the metric formatted
        :rtype: [dict, Any]
        """
        metrics = []
        transaction = self.TAG_ALL if sample_label == '' else sample_label
        # total response size (header+body)
        if kpi_set[KPISet.BYTE_COUNT]:
            metrics.append({
                "time": ts,
                "measurement": self.measurement,
                "fields": {
                    self.METRIC_RECEIVED_BYTES: kpi_set[KPISet.BYTE_COUNT],
                },
                "tags": {
                    self.TAG_TRANSACTION: transaction
                }
            })
        return metrics

    def __compute_common_tags(self):
        common_tags = {
            self.TAG_TRANSACTION: self.TAG_ALL,
            self.TAG_STATUS: self.TAG_ALL,
        }
        if self.application is not None:
            common_tags[self.TAG_APPLICATION] = self.application
        return common_tags

    @staticmethod
    def __merge_tags(series, common_tags=None):
        """
        :param common_tags:
        :type series: Union[dict,str]
        """
        common_tags = common_tags or {}
        if series:
            for pv in series:
                if "tags" in pv:
                    merged_tags = {**common_tags, **pv["tags"]}
                    pv["tags"] = merged_tags
                else:
                    pv["tags"] = common_tags

    def __send_series(self, series=[]):
        """
        :type series: Union[dict,str]
        :return: dict
        """
        # by default epoch is set to 'ns'. time being computed as timestamp we override epoch to 's'.
        self._client.write_points(series, time_precision='s')
