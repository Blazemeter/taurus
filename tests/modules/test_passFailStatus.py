import json
import time

from bzt import AutomatedShutdown
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.passfail import PassFailStatus, DataCriterion
from tests import BZTestCase, random_datapoint, RESOURCES_DIR, ROOT_LOGGER
from tests.mocks import EngineEmul


class TestPassFailStatus(BZTestCase):
    def setUp(self):
        super(TestPassFailStatus, self).setUp()
        self.obj = PassFailStatus()
        self.obj.engine = EngineEmul()

    def configure(self, params):
        self.obj.parameters.merge(params)

    def test_prepare(self):
        config = json.loads(open(RESOURCES_DIR + "json/passfail.json").read())
        self.configure(config['reporting'][0])
        self.obj.prepare()
        self.assertGreater(len(self.obj.criteria), 0)

        for n in range(0, 10):
            point = random_datapoint(n)
            ROOT_LOGGER.info("%s: %s", n, point)
            self.obj.aggregated_second(point)
            try:
                self.obj.check()
            except AutomatedShutdown:
                pass

        try:
            self.obj.post_process()
        except AutomatedShutdown:
            pass

    def test_prepare2(self):
        self.configure({"criteria": ["avg-rt>10ms, continue as non-failed"]})
        self.obj.prepare()
        self.assertGreater(len(self.obj.criteria), 0)

        for n in range(0, 10):
            point = random_datapoint(n)
            self.obj.aggregated_second(point)
            self.obj.check()

        self.obj.post_process()

    def test_prepare3(self):
        self.configure({"criteria": ["avg-rt>10ms for 3s, continue as failed"]})
        self.obj.prepare()
        self.assertGreater(len(self.obj.criteria), 0)

        for n in range(0, 10):
            point = random_datapoint(n)
            point[DataPoint.CURRENT][''][KPISet.AVG_RESP_TIME] = 1
            self.obj.aggregated_second(point)
            self.obj.check()

        self.assertRaises(AutomatedShutdown, self.obj.post_process)

    def test_widget(self):
        self.configure({"criteria": ["avg-rt>10ms for 2s, continue as failed"]})
        self.obj.prepare()
        self.obj.get_widget()
        start_time = time.time()

        for _n in range(0, 10):
            point = random_datapoint(start_time + _n)
            point[DataPoint.CURRENT]['']["avg_rt"] = 1.0
            self.obj.aggregated_second(point)
            self.obj.check()

        self.assertEqual(self.obj.widget.text_widget.text, "Failed: avg-rt>10ms for 10 sec\n")

    def test_within(self):
        self.configure({
            "criteria": [
                "fail>10% within 5s",
                "fail>1000 within 5s",
                "avg-rt>100ms within 10s",
            ]})
        self.obj.prepare()

        start_time = time.time()
        for _n in range(0, 20):
            point = random_datapoint(start_time)
            self.obj.aggregated_second(point)
            if _n % 2 == 0:
                try:
                    self.obj.check()
                except KeyboardInterrupt:
                    pass

            try:
                self.obj.check()
            except KeyboardInterrupt:
                pass
            start_time += 1

    def test_prepare_label_issue(self):
        # https://groups.google.com/forum/?utm_medium=email&utm_source=footer#!msg/codename-taurus/PWjU7xVucZ0/WkjUAbE1EwAJ
        self.configure({"criteria": ["avg-rt of spaced label>10ms"]})
        self.obj.prepare()
        self.assertGreater(len(self.obj.criteria), 0)

        for n in range(0, 10):
            point = random_datapoint(n)
            point[DataPoint.CUMULATIVE]['spaced label'] = point[DataPoint.CUMULATIVE]['']
            point[DataPoint.CURRENT]['spaced label'] = point[DataPoint.CURRENT]['']
            self.obj.aggregated_second(point)
            self.obj.check()

        self.obj.shutdown()

        try:
            self.obj.post_process()
            self.fail()
        except AutomatedShutdown:
            pass

    def test_named_criteria(self):
        self.configure({"criteria": {"named criterion": "avg-rt of spaced label>10ms"}})
        self.obj.prepare()
        self.assertGreater(len(self.obj.criteria), 0)
        self.assertEquals(self.obj.criteria[0].message, "named criterion")

    def test_stop_counting_criteria(self):
        self.configure({"criteria": ["avg-rt>10ms for 2s, continue as failed"]})
        self.obj.prepare()
        self.obj.get_widget()
        start_time = time.time()

        for _n in range(0, 10):
            point = random_datapoint(start_time)
            point[DataPoint.CURRENT]['']["avg_rt"] = 1.0
            self.obj.aggregated_second(point)
            self.obj.check()
            start_time += 1

        self.assertEqual(self.obj.widget.text_widget.text, "Failed: avg-rt>10ms for 10 sec\n")

        for _n in range(0, 10):
            point = random_datapoint(start_time)
            point[DataPoint.CURRENT]['']["avg_rt"] = 0.01
            self.obj.aggregated_second(point)
            self.obj.check()
            start_time += 1

        self.assertEqual(self.obj.widget.text_widget.text, "")

    def test_short_data(self):
        crit_cfg = DataCriterion.string_to_config("failures>0%, stop as failed")
        self.obj.criteria.append(DataCriterion(crit_cfg, self.obj))

        point = DataPoint(0)
        point[DataPoint.CUMULATIVE] = {}
        point[DataPoint.CUMULATIVE][''] = {}
        point[DataPoint.CUMULATIVE][''][KPISet.FAILURES] = 100 * 16
        point[DataPoint.CUMULATIVE][''][KPISet.SAMPLE_COUNT] = 100 * 16

        self.obj.check()
        self.obj.shutdown()
        self.obj.aggregated_second(point)
        self.assertRaises(AutomatedShutdown, self.obj.post_process)

    def test_passfail_crash(self):
        self.configure({
            "module": "passfail",
            "criteria": [
                "fail>10% within 5s"]
        })

        self.obj.engine.config.merge({
            "services": [self.obj.parameters],
        })

        self.obj.prepare()
        self.assertTrue(all(isinstance(obj, dict) for obj in self.obj.parameters["criteria"]))

    def test_percentiles_track(self):
        self.configure({"criteria": ["p90>0ms"]})
        self.obj.prepare()
        self.assertGreater(len(self.obj.criteria), 0)

        for n in range(0, 10):
            point = random_datapoint(n)
            self.obj.aggregated_second(point)
            self.obj.check()

        self.obj.shutdown()
        try:
            self.obj.post_process()
            self.fail()
        except AutomatedShutdown:
            pass

    def test_cumulative_criteria_post_process(self):
        self.configure({"criteria": [
            "p90>0ms, continue as failed",
            "avg-rt>0ms, continue as failed",
        ]})
        self.obj.prepare()
        self.assertEquals(len(self.obj.criteria), 2)

        for n in range(0, 10):
            point = random_datapoint(n)
            self.obj.aggregated_second(point)
            self.obj.check()

        self.obj.shutdown()
        self.assertRaises(AutomatedShutdown, self.obj.post_process)
        for crit in self.obj.criteria:
            self.assertTrue(crit.is_triggered)

    def test_rc_within(self):
        self.configure({"criteria": [
            "rc413>10 within 3s, stop as successful",  # this one to cover branch that caused bug
            "rc413>10 within 10s, stop as failed",
        ]})
        self.obj.prepare()
        self.assertEquals(len(self.obj.criteria), 2)

        for n in range(0, 10):
            point = random_datapoint(n)
            rcs = point[DataPoint.CURRENT][''][KPISet.RESP_CODES]
            rcs['413'] = 3
            self.obj.aggregated_second(point)
            try:
                self.obj.check()
            except AutomatedShutdown:
                break

            self.assertLess(n, 3)

        self.obj.shutdown()
        self.obj.post_process()
        self.assertFalse(self.obj.criteria[0].is_triggered)
        self.assertTrue(self.obj.criteria[1].is_triggered)

    def test_rc_over1(self):
        self.configure({"criteria": [
            "rc200<8 over 5s",
        ]})

        self.obj.prepare()

        for n in range(0, 10):
            point = random_datapoint(n)
            rcs = point[DataPoint.CURRENT][''][KPISet.RESP_CODES]
            rcs['200'] = 3
            self.obj.aggregated_second(point)
            self.obj.check()
            self.assertFalse(self.obj.criteria[0].is_triggered)

        self.obj.shutdown()
        self.obj.post_process()

    def test_rc_over2(self):
        self.configure({"criteria": [
            "rc200>8 over 3s",
        ]})
        self.obj.prepare()

        for n in range(0, 10):
            point = random_datapoint(n)
            rcs = point[DataPoint.CURRENT][''][KPISet.RESP_CODES]
            rcs['200'] = 5
            self.obj.aggregated_second(point)
            try:
                self.obj.check()
            except AutomatedShutdown:
                break

            self.assertLess(n, 3)

        self.assertTrue(self.obj.criteria[0].is_triggered)
        self.obj.shutdown()
        self.obj.post_process()
