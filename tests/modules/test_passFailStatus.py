import json
import logging

from bzt import AutomatedShutdown
from bzt.modules.aggregator import DataPoint, KPISet

from bzt.modules.passfail import PassFailStatus
from tests import BZTestCase, __dir__, random_datapoint


class TestPassFailStatus(BZTestCase):
    def test_prepare(self):
        obj = PassFailStatus()
        config = json.loads(open(__dir__() + "/../json/passfail.json").read())
        obj.parameters = config['reporting'][0]
        obj.prepare()
        self.assertGreater(len(obj.criterias), 0)

        for n in range(0, 10):
            point = random_datapoint(n)
            logging.info("%s: %s", n, point)
            obj.aggregated_second(point)
            try:
                obj.check()
            except AutomatedShutdown:
                pass

        try:
            obj.post_process()
        except AutomatedShutdown:
            pass

    def test_prepare2(self):
        obj = PassFailStatus()
        obj.parameters = {"criterias": ["avg-rt>10ms, continue as non-failed"]}
        obj.prepare()
        self.assertGreater(len(obj.criterias), 0)

        for n in range(0, 10):
            point = random_datapoint(n)
            obj.aggregated_second(point)
            obj.check()

        obj.post_process()

    def test_prepare3(self):
        obj = PassFailStatus()
        obj.parameters = {"criterias": ["avg-rt>10ms for 3s, continue as failed"]}
        obj.prepare()
        self.assertGreater(len(obj.criterias), 0)

        for n in range(0, 10):
            point = random_datapoint(n)
            point[DataPoint.CURRENT][''][KPISet.AVG_RESP_TIME] = 1
            obj.aggregated_second(point)
            obj.check()

        try:
            obj.post_process()
            self.fail()
        except AutomatedShutdown:
            pass
