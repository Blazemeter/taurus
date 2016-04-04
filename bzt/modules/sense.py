"""
Basics of reporting capabilities

Copyright 2016 BlazeMeter Inc.

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
import json

from bzt.engine import Reporter
from bzt.modules.monitoring import Monitoring, MonitoringListener
from bzt.modules.aggregator import DataPoint, KPISet, AggregatorListener, ResultsProvider


class BlazemeterSenseReporter(Reporter, AggregatorListener, MonitoringListener):
    """
    A reporter that prints short statistics on test end
    """

    def __init__(self):
        super(BlazemeterSenseReporter, self).__init__()
        self.results_fds = None
        self.monitoring_fds = None

    def prepare(self):
        super(BlazemeterSenseReporter, self).prepare()
        if isinstance(self.engine.aggregator, ResultsProvider):
            self.engine.aggregator.add_listener(self)
        for service in self.engine.services:
            if isinstance(service, Monitoring):
                service.add_listener(self)
        self.results_fds = open(self.engine.create_artifact('results', '.ldjson'), 'w')
        self.monitoring_fds = open(self.engine.create_artifact('monitoring', '.ldjson'), 'w')

    def startup(self):
        # sense.start_online()
        pass

    def aggregated_second(self, data):
        self.results_fds.write("%s\n" % json.dumps(data, sort_keys=True))
        # - aggregate 5 or more seconds
        # - send them to Sense

    def monitoring_data(self, data):
        self.monitoring_fds.write("%s\n" % json.dumps(data, sort_keys=True))
        # - aggregate 5 or more seconds
        # - send them to Sense

    def shutdown(self):
        # sense.end_online()
        pass

    def post_process(self):
        super(BlazemeterSenseReporter, self).post_process()
        # send results/monitoring data

    def finalize(self):
        """"
        Close opened file
        """
        if self.results_fds:
            self.results_fds.flush()
            self.results_fds.close()
        if self.monitoring_fds:
            self.monitoring_fds.flush()
            self.monitoring_fds.close()

