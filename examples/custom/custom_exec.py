import os

from bzt.modules import SubprocessedExecutor, ConsolidatingAggregator
from bzt.modules.jmeter import JTLReader


class MyCustomExecutor(SubprocessedExecutor):

    def __init__(self):
        super(MyCustomExecutor, self).__init__()
        self.register_reader = False

    def prepare(self):
        super(MyCustomExecutor, self).prepare()
        self.reporting_setup(suffix='.csv')
        self.reader = JTLReader(self.report_file, self.log)
        if isinstance(self.engine.aggregator, ConsolidatingAggregator):
            self.engine.aggregator.add_underling(self.reader)

    def startup(self):
        # here you shoild only start subprocess
        # all longer preparations should be done in prepare()
        load = self.get_load()
        cmdline = [os.path.abspath(os.path.join(os.path.dirname(__file__), "custom_cmd.sh")),
                   self.report_file, str(load.concurrency), str(load.iterations)]

        self.process = self.execute(cmdline)
