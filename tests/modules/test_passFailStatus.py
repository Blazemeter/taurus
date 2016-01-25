import json
import logging
import time

from tests import BZTestCase, __dir__, random_datapoint
from bzt import AutomatedShutdown
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.passfail import PassFailStatus
from tests.mocks import EngineEmul


class TestPassFailStatus(BZTestCase):
    def test_prepare(self):
        obj = PassFailStatus()
        obj.engine=EngineEmul()
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
        obj.engine=EngineEmul()
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
        obj.engine=EngineEmul()
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

    def test_widget(self):
        obj = PassFailStatus()
        obj.engine=EngineEmul()
        obj.parameters = {"criterias": ["avg-rt>10ms for 2s, continue as failed"]}
        obj.prepare()
        obj.get_widget()
        start_time = time.time()

        for _n in range(0, 10):
            point = random_datapoint(start_time)
            point[DataPoint.CURRENT]['']["avg_rt"] = 1.0
            obj.aggregated_second(point)
            obj.check()
            start_time += 1

        self.assertEqual(obj.widget.text_widget.text, "Failed: avg-rt>10ms for 10 sec\n")

    def test_within(self):
        obj = PassFailStatus()
        obj.engine=EngineEmul()
        obj.parameters = {"criterias": [
            "fail>10% within 5s",
            "fail>1000 within 5s",
            "avg-rt>100ms within 10s",
        ]}
        obj.prepare()

        start_time = time.time()
        for _n in range(0, 20):
            point = random_datapoint(start_time)
            obj.aggregated_second(point)
            if _n % 2 == 0:
                try:
                    obj.check()
                except KeyboardInterrupt:
                    pass

            try:
                obj.check()
            except KeyboardInterrupt:
                pass
            start_time += 1

    def test_prepare_label_issue(self):
        # https://groups.google.com/forum/?utm_medium=email&utm_source=footer#!msg/codename-taurus/PWjU7xVucZ0/WkjUAbE1EwAJ
        obj = PassFailStatus()
        obj.engine=EngineEmul()
        obj.parameters = {"criterias": ["avg-rt of spaced label>10ms"]}
        obj.prepare()
        self.assertGreater(len(obj.criterias), 0)

        for n in range(0, 10):
            point = random_datapoint(n)
            point[DataPoint.CUMULATIVE]['spaced label'] = point[DataPoint.CUMULATIVE]['']
            point[DataPoint.CURRENT]['spaced label'] = point[DataPoint.CURRENT]['']
            obj.aggregated_second(point)
            obj.check()

        obj.shutdown()

        try:
            obj.post_process()
            self.fail()
        except AutomatedShutdown:
            pass

    def test_named_criteria(self):
        obj = PassFailStatus()
        obj.engine = EngineEmul()
        obj.parameters = {"criterias": {"named criteria": "avg-rt of spaced label>10ms"}}
        obj.prepare()
        self.assertGreater(len(obj.criterias), 0)
        self.assertEquals(obj.criterias[0].message, "named criteria")

    def test_stop_counting_criteria(self):
        obj = PassFailStatus()
        obj.engine=EngineEmul()
        obj.parameters = {"criterias": ["avg-rt>10ms for 2s, continue as failed"]}
        obj.prepare()
        obj.get_widget()
        start_time = time.time()

        for _n in range(0, 10):
            point = random_datapoint(start_time)
            point[DataPoint.CURRENT]['']["avg_rt"] = 1.0
            obj.aggregated_second(point)
            obj.check()
            start_time += 1

        self.assertEqual(obj.widget.text_widget.text, "Failed: avg-rt>10ms for 10 sec\n")

        for _n in range(0, 10):
            point = random_datapoint(start_time)
            point[DataPoint.CURRENT]['']["avg_rt"] = 0.01
            obj.aggregated_second(point)
            obj.check()
            start_time += 1

        self.assertEqual(obj.widget.text_widget.text, "")
