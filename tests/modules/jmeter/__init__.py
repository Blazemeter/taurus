import logging

from tests.mocks import EngineEmul
from bzt.modules.jmeter import JMeter, JMeterExecutor
from bzt.utils import get_full_path


class MockJMeter(JMeter):
    def __init__(self, has_ctg=None, reaction=None):
        jmeter_version = JMeterExecutor.JMETER_VER
        jmeter_path = "~/.bzt/jmeter-taurus/{version}/"
        jmeter_path = get_full_path(jmeter_path)

        super(MockJMeter, self).__init__(tool_path=jmeter_path, parent_logger=logging.getLogger(''),
                                         jmeter_version=jmeter_version, jmeter_download_link=None, plugins=[], proxy={})
        self.has_ctg = has_ctg
        self.reaction = reaction if reaction else []

    def ctg_plugin_installed(self):
        return self.has_ctg

    def _pmgr_call(self, params):
        # replaces real pmgr call
        reaction = self.reaction.pop(0)
        if 'raise' in reaction:
            raise reaction['raise']

        return reaction['output']


class MockJMeterExecutor(JMeterExecutor):
    def __init__(self, load=None, settings=None, has_ctg=None):
        super(MockJMeterExecutor, self).__init__()
        self.mock_install = True
        self.version = None

        if load is None: load = {}
        if settings is None: settings = {}
        if has_ctg is None: has_ctg = True

        self.engine = EngineEmul()
        self.env = self.engine.env
        self.execution.merge(load)
        self.settings.merge({"detect-plugins": False})
        self.settings.merge(settings)
        self.tool = MockJMeter(has_ctg)

    def install_required_tools(self):
        if self.mock_install:
            self.version = self.settings.get('version')
            self.tool = MockJMeter()
        else:
            super(MockJMeterExecutor, self).install_required_tools()
