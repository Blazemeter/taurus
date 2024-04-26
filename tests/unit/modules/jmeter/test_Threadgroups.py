from bzt.jmx import JMX
from bzt.jmx.threadgroups import ThreadGroupHandler
from tests.unit import BZTestCase


class TestThreadGroups(BZTestCase):
    def test_property_resolving(self):
        jmx = JMX(original="tests/resources/jmeter/torero/duration-with-env.jmx")
        tgh = ThreadGroupHandler(self.log)
        thread_groups = list(tgh.groups(jmx))
        self.assertEqual(5, thread_groups[0].get_ramp_up())
        self.assertEqual('M', thread_groups[0]._get_time_unit())
        self.assertEqual(12, thread_groups[0].get_concurrency())
        self.assertEqual(1, thread_groups[0].get_rate())
        self.assertEqual(None, thread_groups[0].get_iterations())
        self.assertEqual(360, thread_groups[0].get_duration())

