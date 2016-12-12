import logging
import new
from operator import itemgetter
from getsize import getsize
from random import random
from time import time, sleep

from bzt.engine import ScenarioExecutor, Service
from bzt.modules.aggregator import ConsolidatingAggregator, ResultsReader
from bzt.modules.console import WidgetProvider, ExecutorWidget
from bzt.modules.blazemeter import BlazeMeterUploader, BlazeMeterClientEmul


class Empty(ScenarioExecutor, WidgetProvider):
    def __init__(self):
        super(Empty, self).__init__()
        self.log = logging.getLogger('')
        self.process = None
        self.tool_path = None
        self.scenario = None

    def prepare(self):
        self.reader = FakeReader(self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def check(self):
        self.log.info('Engine   : %6.3fs', self.engine.engine_loop_utilization)

    def get_widget(self):
        if not self.widget:
            self.widget = ExecutorWidget(self, "%s" % self)
        return self.widget


class FakeReader(ResultsReader):
    def __init__(self, parent_logger):
        super(FakeReader, self).__init__()
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.ts = time()
        self.time_range = 5
        self.val_range = 10

    def _read(self, last_pass=False):
        interval = time() - self.ts
        self.ts += interval

        for _ in range(int(interval*1000)):
            yield int(self.ts - random()*self.time_range), \
                  'urllll', \
                  1, \
                  random() * self.val_range, \
                  random() * self.val_range, \
                  random() * self.val_range, \
                  '200', \
                  None, \
                  '', \
                  1


class TimerAndReplacer(Service):
    def __init__(self):
        super(TimerAndReplacer, self).__init__()
        self.old_checks = {}
        self.timings = {}

    def measured_check(self, holder):
        mod_name = "%s" % holder
        start_time = time()
        check_val = self.old_checks[mod_name]()
        self.timings[mod_name] = time() - start_time
        return check_val

    def main_measured_check(self):
        mod_name = "%s" % self.engine
        start_time = time()
        check_val = self.old_checks[mod_name]()
        total_time = time() - start_time
        results = []
        for timing in self.timings:
            name = timing.split('.')[-1].split(' ')[0]
            results.append((name, self.timings[timing]/total_time*100))
        results.sort(key=itemgetter(1), reverse=True)
        out_str = 'Total    :%7.3fs\n\n' % total_time
        for res in results:
            out_str += '%-23s :  %5.2f%%\n' % (res[0], res[1])
        self.log.info(out_str)
        return check_val

    def startup(self):
        eng_name = "%s" % self.engine
        self.old_checks[eng_name] = self.engine._check_modules_list
        self.engine._check_modules_list = self.main_measured_check
        modules = [self.engine.provisioning, self.engine.aggregator] + self.engine.services + self.engine.reporters
        for module in modules:
            mod_name = "%s" % module
            self.old_checks[mod_name] = module.check
            module.check = new.instancemethod(self.measured_check, module, None)


class SlowUploader(BlazeMeterUploader):
    def __init__(self):
        super(SlowUploader, self).__init__()
        self.client = SlowClient(self.log)


class SlowClient(BlazeMeterClientEmul):
    def send_kpi_data(self, data_buffer, is_check_response=True, is_final=False):
        size = getsize(data_buffer)
        #if size > 2 ** 30:
        #    size = "%.2f" % (1.0 * size/(2 ** 30))
        #elif size > 2 ** 20:
        #    size = "%.2f" % (1.0 * size / (2 ** 20))
        #elif size > 2 ** 10:
        #    size = "%.2f" % (1.0 * size / (2 ** 10))
        #else:
        #    size = "%sb" % size
        size = size / 1000

        self.log.info('Buffer: %s (%sK)', len(data_buffer), size)
        super(SlowClient, self).send_kpi_data(data_buffer, is_check_response, is_final)

    def _request(self, url, data=None, headers=None, checker=None, method=None):
        sleep(0.2)
        # return super(SlowClient, self)._request(url, data, headers, checker, method)
        return {
            'result': {
                'session': {'id': 'id', 'userId': 'userId', 'testId': 'testId'},
                'master': {'id': 'id'},
                'signature': 'sig',
                'id': 'id'}}

""""msg = ','.join(["%.3f" % (timings[i+1] - timings[i]) for i in range(len(timings)-1)])"""
