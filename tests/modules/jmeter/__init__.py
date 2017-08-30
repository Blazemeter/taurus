import logging

from bzt.modules.jmeter import JMeter, JMeterExecutor
from bzt.utils import get_full_path


class MockJMeter(JMeter):
    def __init__(self, reaction=None):
        jmeter_version = JMeterExecutor.JMETER_VER
        jmeter_path = "~/.bzt/jmeter-taurus/{version}/"
        jmeter_path = get_full_path(jmeter_path)

        super(MockJMeter, self).__init__(tool_path=jmeter_path, parent_logger=logging.getLogger(''),
                                         jmeter_version=jmeter_version, jmeter_download_link=None, plugins=[], proxy={})
        self.reaction = reaction if reaction else []

    def _pmgr_call(self, params):
        # replaces real pmgr call
        reaction = self.reaction.pop(0)
        if 'raise' in reaction:
            raise reaction['raise']

        return reaction['output']


class MockJMeterExecutor(JMeterExecutor):
    def __init__(self):
        super(MockJMeterExecutor, self).__init__()
        self.mock_install = True
        self.version = None

    def install_required_tools(self):
        if self.mock_install:
            self.version = self.settings.get('version')
            self.tool = MockJMeter()
        else:
            super(MockJMeterExecutor, self).install_required_tools()