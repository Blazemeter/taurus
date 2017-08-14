import json
import logging
import time

from bzt import AutomatedShutdown
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.passfail import PassFailStatus, DataCriterion
from bzt.utils import BetterDict
from tests import BZTestCase, random_datapoint, RESOURCES_DIR
from tests.mocks import EngineEmul


class TestPassFailStatus(BZTestCase):
    def test_prepare(self):
        obj = PassFailStatus()
        obj.engine = EngineEmul()
        config = json.loads(open(RESOURCES_DIR + "json/passfail.json").read())
        obj.parameters = config['reporting'][0]
        obj.prepare()
        self.assertGreater(len(obj.criteria), 0)

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
        obj.engine = EngineEmul()
        obj.parameters = {"criteria": ["avg-rt>10ms, continue as non-failed"]}
        obj.prepare()
        self.assertGreater(len(obj.criteria), 0)

        for n in range(0, 10):
            point = random_datapoint(n)
            obj.aggregated_second(point)
            obj.check()

        obj.post_process()

    def test_prepare3(self):
        obj = PassFailStatus()
        obj.engine = EngineEmul()
        obj.parameters = {"criteria": ["avg-rt>10ms for 3s, continue as failed"]}
        obj.prepare()
        self.assertGreater(len(obj.criteria), 0)

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
        obj.engine = EngineEmul()
        obj.parameters = {"criteria": ["avg-rt>10ms for 2s, continue as failed"]}
        obj.prepare()
        obj.get_widget()
        start_time = time.time()

        for _n in range(0, 10):
            point = random_datapoint(start_time + _n)
            point[DataPoint.CURRENT]['']["avg_rt"] = 1.0
            obj.aggregated_second(point)
            obj.check()

        self.assertEqual(obj.widget.text_widget.text, "Failed: avg-rt>10ms for 10 sec\n")

    def test_within(self):
        obj = PassFailStatus()
        obj.engine = EngineEmul()
        obj.parameters = {"criteria": [
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
        obj.engine = EngineEmul()
        obj.parameters = {"criteria": ["avg-rt of spaced label>10ms"]}
        obj.prepare()
        self.assertGreater(len(obj.criteria), 0)

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
        obj.parameters = {"criteria": {"named criterion": "avg-rt of spaced label>10ms"}}
        obj.prepare()
        self.assertGreater(len(obj.criteria), 0)
        self.assertEquals(obj.criteria[0].message, "named criterion")

    def test_stop_counting_criteria(self):
        obj = PassFailStatus()
        obj.engine = EngineEmul()
        obj.parameters = {"criteria": ["avg-rt>10ms for 2s, continue as failed"]}
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

    def test_short_data(self):
        obj = PassFailStatus()
        obj.engine = EngineEmul()

        crit_cfg = DataCriterion.string_to_config("failures>0%, stop as failed")
        obj.criteria.append(DataCriterion(crit_cfg, obj))

        point = DataPoint(0)
        point[DataPoint.CUMULATIVE] = {}
        point[DataPoint.CUMULATIVE][''] = {}
        point[DataPoint.CUMULATIVE][''][KPISet.FAILURES] = 100 * 16
        point[DataPoint.CUMULATIVE][''][KPISet.SAMPLE_COUNT] = 100 * 16

        obj.check()
        obj.shutdown()
        obj.aggregated_second(point)
        self.assertRaises(AutomatedShutdown, obj.post_process)

    def test_passfail_crash(self):
        passfail = BetterDict()
        passfail.merge({
            "module": "passfail",
            "criteria": [
                "fail>10% within 5s",
            ]
        })
        obj = PassFailStatus()
        obj.engine = EngineEmul()
        obj.parameters = passfail
        obj.engine.config.merge({
            "services": [passfail],
        })
        obj.prepare()
        self.assertTrue(all(isinstance(obj, dict) for obj in passfail["criteria"]))

    def test_percentiles_track(self):
        obj = PassFailStatus()
        obj.engine = EngineEmul()
        obj.parameters = {"criteria": ["p90>0ms"]}
        obj.prepare()
        self.assertGreater(len(obj.criteria), 0)

        for n in range(0, 10):
            point = random_datapoint(n)
            obj.aggregated_second(point)
            obj.check()

        obj.shutdown()
        try:
            obj.post_process()
            self.fail()
        except AutomatedShutdown:
            pass

    def test_cumulative_criteria_post_process(self):
        obj = PassFailStatus()
        obj.engine = EngineEmul()
        obj.parameters = {"criteria": [
            "p90>0ms, continue as failed",
            "avg-rt>0ms, continue as failed",
        ]}
        obj.prepare()
        self.assertEquals(len(obj.criteria), 2)

        for n in range(0, 10):
            point = random_datapoint(n)
            obj.aggregated_second(point)
            obj.check()

        obj.shutdown()
        self.assertRaises(AutomatedShutdown, obj.post_process)
        for crit in obj.criteria:
            self.assertTrue(crit.is_triggered)

    def test_rc_within(self):
        obj = PassFailStatus()
        obj.engine = EngineEmul()
        obj.parameters = {"criteria": [
            "rc413>10 within 3s, stop as successful",  # this one to cover branch that caused bug
            "rc413>10 within 10s, stop as failed",
        ]}
        obj.prepare()
        self.assertEquals(len(obj.criteria), 2)

        for n in range(0, 10):
            point = random_datapoint(n)
            rcs = point[DataPoint.CURRENT][''][KPISet.RESP_CODES]
            rcs['413'] = 3
            obj.aggregated_second(point)
            try:
                obj.check()
            except AutomatedShutdown:
                break

            self.assertLess(n, 3)

        obj.shutdown()
        obj.post_process()
        self.assertFalse(obj.criteria[0].is_triggered)
        self.assertTrue(obj.criteria[1].is_triggered)

    def test_rc_over1(self):
        obj = PassFailStatus()
        obj.engine = EngineEmul()
        obj.parameters = {"criteria": [
            "rc200<8 over 5s",
        ]}
        obj.prepare()

        for n in range(0, 10):
            point = random_datapoint(n)
            rcs = point[DataPoint.CURRENT][''][KPISet.RESP_CODES]
            rcs['200'] = 3
            obj.aggregated_second(point)
            obj.check()
            self.assertFalse(obj.criteria[0].is_triggered)

        obj.shutdown()
        obj.post_process()

    def test_rc_over2(self):
        obj = PassFailStatus()
        obj.engine = EngineEmul()
        obj.parameters = {"criteria": [
            "rc200>8 over 3s",
        ]}
        obj.prepare()

        for n in range(0, 10):
            point = random_datapoint(n)
            rcs = point[DataPoint.CURRENT][''][KPISet.RESP_CODES]
            rcs['200'] = 5
            # logging.debug("Datapoint %s: %s", n, point)
            obj.aggregated_second(point)
            try:
                obj.check()
            except AutomatedShutdown:
                break

            self.assertLess(n, 3)

        self.assertTrue(obj.criteria[0].is_triggered)
        obj.shutdown()
        obj.post_process()
