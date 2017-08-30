import logging

from bzt.modules.jmeter import JMeterExecutor, JMeter

from bzt.utils import get_full_path
from tests import BZTestCase


class MockJMeter(JMeter):
    def __init__(self, tool_path, parent_logger, jmeter_version, jmeter_download_link, plugins, proxy, reaction=None):
        super(MockJMeter, self).__init__(tool_path, parent_logger, jmeter_version, jmeter_download_link, plugins, proxy)
        self.reaction = reaction

    def _pmgr_call(self, params):
        # todo: handle reaction
        pass


class TestJMeterTool(BZTestCase):
    def setUp(self):
        super(TestJMeterTool, self).setUp()

        jmeter_version = JMeterExecutor.JMETER_VER
        jmeter_path = "~/.bzt/jmeter-taurus/{version}/"
        jmeter_path = get_full_path(jmeter_path)
        self.obj = MockJMeter(
            tool_path=jmeter_path,
            parent_logger = logging.getLogger(''),
            jmeter_version = jmeter_version,
            jmeter_download_link=None,
            plugins=[],
            proxy={})

    def test_old_plugin(self):
        pass