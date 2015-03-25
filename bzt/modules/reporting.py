"""
Basics of reporting capabilities

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

from bzt.modules.aggregator import DataPoint, KPISet

from bzt.engine import Reporter, AggregatorListener


class FinalStatus(Reporter, AggregatorListener):
    """
    A reporter that prints short statistics on test end
    """

    def __init__(self):
        super(FinalStatus, self).__init__()
        self.last_sec = None

    def aggregated_second(self, data):
        """
        Just store the latest info

        :type data: bzt.modules.aggregator.DataPoint
        """
        self.last_sec = data

    def post_process(self):
        """
        Log basic stats
        """
        super(FinalStatus, self).post_process()
        if self.last_sec:
            cumul = self.last_sec[DataPoint.CUMULATIVE]
            overall = cumul['']
            err_rate = 100 * overall[KPISet.FAILURES] / float(overall[KPISet.SAMPLE_COUNT])
            self.log.info("Samples count: %s, %.2f%% failures", overall[KPISet.SAMPLE_COUNT], err_rate)
            fmt = "Average times: total %.3f, latency %.3f, connect %.3f"
            self.log.info(fmt, overall[KPISet.AVG_RESP_TIME], overall[KPISet.AVG_LATENCY],
                          overall[KPISet.AVG_CONN_TIME])
            for key in sorted(overall[KPISet.PERCENTILES].keys(), key=float):
                self.log.info("Percentile %.1f%%: %.3f", float(key), overall[KPISet.PERCENTILES][key])

                # todo: add optional errors summary
