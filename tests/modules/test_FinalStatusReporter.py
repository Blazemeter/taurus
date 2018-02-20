import os
from collections import Counter
import time

from tests import BZTestCase, random_datapoint
from tests.mocks import EngineEmul
from bzt.modules.blazemeter import BlazeMeterUploader, CloudProvisioning
from bzt.modules.reporting import FinalStatus
from bzt.utils import BetterDict
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.functional import ResultsTree, FunctionalSample


class TestFinalStatusReporter(BZTestCase):
    def test_log_messages_failed_labels(self):
        obj = FinalStatus()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        self.sniff_log(obj.log)
        obj.parameters.merge({"failed-labels": True, "percentiles": False, "summary": False, "test-duration": False})

        obj.startup()
        obj.shutdown()
        obj.aggregated_second(self.__get_datapoint())
        obj.post_process()
        self.assertIn("29656 failed samples: http://192.168.1.1/anotherquery\n", self.log_recorder.info_buff.getvalue())

    def test_log_messages_percentiles(self):
        obj = FinalStatus()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        self.sniff_log(obj.log)
        obj.parameters.merge({"failed-labels": False, "percentiles": True, "summary": False, "test-duration": False})

        obj.startup()
        obj.shutdown()
        obj.aggregated_second(self.__get_datapoint())
        obj.post_process()
        target_output = ("Average times: total 0.001, latency 0.000, connect 0.000\n"
                         "Percentile 0.0%: 0.000\n"
                         "Percentile 50.0%: 0.000\n"
                         "Percentile 90.0%: 0.001\n"
                         "Percentile 95.0%: 0.001\n"
                         "Percentile 99.0%: 0.003\n"
                         "Percentile 99.9%: 0.008\n"
                         "Percentile 100.0%: 0.081\n"
                         )
        self.assertEqual(target_output, self.log_recorder.info_buff.getvalue())

    def test_log_messages_samples_count(self):
        obj = FinalStatus()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        self.sniff_log(obj.log)
        obj.parameters.merge({"failed-labels": False, "percentiles": False, "summary": True, "test-duration": False})
        obj.aggregated_second(self.__get_datapoint())
        obj.startup()
        obj.shutdown()
        obj.post_process()

        self.assertEqual("Samples count: 59314, 50.00% failures\n", self.log_recorder.info_buff.getvalue())

    def test_log_messages_duration(self):
        """
        Test duration report
        :return:
        """
        obj = FinalStatus()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        self.sniff_log(obj.log)
        obj.prepare()
        obj.startup()
        obj.shutdown()
        obj.start_time -= 120005
        obj.post_process()
        self.assertEqual("Test duration: 1 day, 9:20:05\n", self.log_recorder.info_buff.getvalue())

    def test_dump(self):
        obj = FinalStatus()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        self.sniff_log(obj.log)
        obj.parameters.merge({
            "dump-xml": obj.engine.create_artifact("status", ".xml"),
            "dump-csv": obj.engine.create_artifact("status", ".csv")
        })

        obj.aggregated_second(random_datapoint(time.time()))
        obj.startup()
        obj.shutdown()
        obj.post_process()
        self.assertIn("XML", self.log_recorder.info_buff.getvalue())

    def test_func_report(self):
        obj = FinalStatus()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        self.sniff_log(obj.log)
        obj.prepare()
        obj.startup()
        obj.shutdown()
        obj.aggregated_results(*self.__get_func_tree())
        obj.post_process()
        info_log = self.log_recorder.info_buff.getvalue()
        warn_log = self.log_recorder.warn_buff.getvalue()
        self.assertIn("Total: 3 tests", info_log)
        self.assertIn("Test TestClass.case2 failed: something broke", warn_log)
        self.assertIn("stacktrace2", warn_log)
        self.assertIn("Test TestClass.case3 failed: something is badly broken", warn_log)
        self.assertIn("stacktrace3", warn_log)

    def test_func_report_all_no_stacktrace(self):
        obj = FinalStatus()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        self.sniff_log(obj.log)
        obj.parameters.merge({"report-tests": "all", "print-stacktrace": False})
        obj.prepare()
        obj.startup()
        obj.shutdown()
        obj.aggregated_results(*self.__get_func_tree())
        obj.post_process()
        info_log = self.log_recorder.info_buff.getvalue()
        self.assertIn("Total: 3 tests", info_log)
        self.assertIn("Test TestClass.case1 - PASSED", info_log)
        self.assertIn("Test TestClass.case2 - FAILED", info_log)
        self.assertIn("Test TestClass.case3 - BROKEN", info_log)
        self.assertNotIn("stacktrace2", info_log)
        self.assertNotIn("stacktrace3", info_log)

    def __get_datapoint(self, ts=0):
        datapoint = DataPoint(ts, None)
        cumul_data = datapoint[DataPoint.CUMULATIVE]
        cumul_data[""] = KPISet.from_dict(
            {KPISet.AVG_CONN_TIME: 7.890211417203362e-06,
             KPISet.RESP_TIMES: Counter(
                 {0.0: 32160, 0.001: 24919, 0.002: 1049, 0.003: 630, 0.004: 224, 0.005: 125,
                  0.006: 73, 0.007: 46, 0.008: 32, 0.009: 20, 0.011: 8, 0.01: 8, 0.017: 3,
                  0.016: 3, 0.014: 3, 0.013: 3, 0.04: 2, 0.012: 2, 0.079: 1, 0.081: 1,
                  0.019: 1, 0.015: 1}),
             KPISet.ERRORS: [{'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                              'urls': Counter({'http://192.168.1.1/anotherquery': 7373}), KPISet.RESP_CODES: '403'}],
             KPISet.STDEV_RESP_TIME: 0.04947974228872108,
             KPISet.AVG_LATENCY: 0.0002825639815220692,
             KPISet.RESP_CODES: Counter({'304': 29656, '403': 29656, '200': 2}),
             KPISet.PERCENTILES: {'95.0': 0.001, '0.0': 0.0, '99.9': 0.008, '90.0': 0.001,
                                  '100.0': 0.081, '99.0': 0.003, '50.0': 0.0},
             KPISet.SUCCESSES: 29658,
             KPISet.SAMPLE_COUNT: 59314,
             KPISet.CONCURRENCY: 0,
             KPISet.AVG_RESP_TIME: 0.0005440536804127192,
             KPISet.FAILURES: 29656})
        cumul_data["http://192.168.1.1/somequery"] = KPISet.from_dict(
            {KPISet.AVG_CONN_TIME: 9.609548856969457e-06,
             KPISet.RESP_TIMES: Counter(
                 {0.0: 17219, 0.001: 11246, 0.002: 543, 0.003: 341,
                  0.004: 121,
                  0.005: 66, 0.006: 36, 0.007: 33, 0.008: 18,
                  0.009: 12, 0.011: 6,
                  0.01: 5, 0.013: 2, 0.017: 2, 0.012: 2, 0.079: 1,
                  0.016: 1,
                  0.014: 1, 0.019: 1, 0.04: 1, 0.081: 1}),
             KPISet.ERRORS: [],
             KPISet.STDEV_RESP_TIME: 0.04073402130687656,
             KPISet.AVG_LATENCY: 1.7196034796682178e-06,
             KPISet.RESP_CODES: Counter({'304': 29656, '200': 2}),
             KPISet.PERCENTILES: {'95.0': 0.001, '0.0': 0.0,
                                  '99.9': 0.009,
                                  '90.0': 0.001,
                                  '100.0': 0.081,
                                  '99.0': 0.004,
                                  '50.0': 0.0},
             KPISet.SUCCESSES: 29658,
             KPISet.SAMPLE_COUNT: 29658,
             KPISet.CONCURRENCY: 0,
             KPISet.AVG_RESP_TIME: 0.0005164542450603551, KPISet.FAILURES: 0})
        cumul_data["http://192.168.1.1/anotherquery"] = KPISet.from_dict(
            {KPISet.AVG_CONN_TIME: 6.1707580253574335e-06,
             KPISet.RESP_TIMES: Counter({0.0: 14941, 0.001: 13673, 0.002: 506,
                                         0.003: 289, 0.004: 103,
                                         0.005: 59, 0.006: 37, 0.008: 14,
                                         0.007: 13, 0.009: 8, 0.01: 3,
                                         0.011: 2, 0.016: 2, 0.014: 2,
                                         0.017: 1, 0.013: 1, 0.015: 1,
                                         0.04: 1}),
             KPISet.ERRORS: [
                 {'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                  'urls': Counter(
                      {'http://192.168.1.1/anotherquery': 7373}),
                  KPISet.RESP_CODES: '403'}],
             KPISet.STDEV_RESP_TIME: 0.032465137860758844,
             KPISet.AVG_LATENCY: 0.0005634272997032645,
             KPISet.RESP_CODES: Counter({'403': 29656}),
             KPISet.PERCENTILES: {'95.0': 0.001, '0.0': 0.0,
                                  '99.9': 0.008, '90.0': 0.001,
                                  '100.0': 0.04, '99.0': 0.003,
                                  '50.0': 0.0},
             KPISet.SUCCESSES: 0,
             KPISet.SAMPLE_COUNT: 29656,
             KPISet.CONCURRENCY: 0,
             KPISet.AVG_RESP_TIME: 0.0005716549770704078,
             KPISet.FAILURES: 29656})
        cumul_data["http://192.168.100.100/somequery"] = KPISet.from_dict(
            {KPISet.AVG_CONN_TIME: 9.609548856969457e-06,
             KPISet.RESP_TIMES: Counter(
                 {0.0: 17219, 0.001: 11246, 0.002: 543,
                  0.003: 341, 0.004: 121,
                  0.005: 66, 0.006: 36, 0.007: 33, 0.008: 18,
                  0.009: 12, 0.011: 6,
                  0.01: 5, 0.013: 2, 0.017: 2, 0.012: 2,
                  0.079: 1, 0.016: 1,
                  0.014: 1, 0.019: 1, 0.04: 1, 0.081: 1}),
             KPISet.ERRORS: [],
             KPISet.STDEV_RESP_TIME: 0.04073402130687656,
             KPISet.AVG_LATENCY: 1.7196034796682178e-06,
             KPISet.RESP_CODES: Counter({'304': 29656, '200': 2}),
             KPISet.PERCENTILES: {'95.0': 0.001, '0.0': 0.0,
                                  '99.9': 0.009, '90.0': 0.001,
                                  '100.0': 0.081, '99.0': 0.004,
                                  '50.0': 0.0},
             KPISet.SUCCESSES: 29658,
             KPISet.SAMPLE_COUNT: 29658,
             KPISet.CONCURRENCY: 0,
             KPISet.AVG_RESP_TIME: 0.0005164542450603551,
             KPISet.FAILURES: 0})
        return datapoint

    def __get_func_tree(self):
        tree = ResultsTree()
        tree.add_sample(FunctionalSample(test_case="case1", test_suite="TestClass", status="PASSED",
                                         start_time=time.time(), duration=0.12,
                                         error_msg=None, error_trace=None, extras=None, subsamples=[]))
        tree.add_sample(FunctionalSample(test_case="case2", test_suite="TestClass", status="FAILED",
                                         start_time=time.time(), duration=0.33,
                                         error_msg="something broke", error_trace="stacktrace2", extras=None,
                                         subsamples=[]))
        tree.add_sample(FunctionalSample(test_case="case3", test_suite="TestClass", status="BROKEN",
                                         start_time=time.time(), duration=0.33,
                                         error_msg="something is badly broken", error_trace="stacktrace3", extras=None,
                                         subsamples=[]))
        return tree, tree

    def test_blazemeter_report_link(self):
        obj = FinalStatus()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        xml_report = obj.engine.create_artifact("status", ".xml")
        obj.parameters.merge({
            "dump-xml": xml_report,
        })

        rep = BlazeMeterUploader()
        rep.results_url = "http://report/link"
        obj.engine.reporters.append(rep)

        obj.startup()
        obj.shutdown()

        obj.aggregated_second(self.__get_datapoint())
        obj.post_process()

        self.assertTrue(os.path.exists(xml_report))
        with open(xml_report) as fds:
            report_content = fds.read()
        self.assertIn('<ReportURL>http://report/link</ReportURL>', report_content)

    def test_blazemeter_cloud_report_link(self):
        obj = FinalStatus()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        xml_report = obj.engine.create_artifact("status", ".xml")
        obj.parameters.merge({
            "dump-xml": xml_report,
        })

        prov = CloudProvisioning()
        prov.results_url = "http://report/link"
        obj.engine.provisioning = prov

        obj.startup()
        obj.shutdown()

        obj.aggregated_second(self.__get_datapoint())
        obj.post_process()

        self.assertTrue(os.path.exists(xml_report))
        with open(xml_report) as fds:
            report_content = fds.read()
        self.assertIn('<ReportURL>http://report/link</ReportURL>', report_content)

    def test_xml_report_test_duration(self):
        obj = FinalStatus()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        xml_report = obj.engine.create_artifact("status", ".xml")
        obj.parameters.merge({
            "dump-xml": xml_report,
        })

        obj.startup()
        obj.aggregated_second(self.__get_datapoint(ts=90))
        obj.aggregated_second(self.__get_datapoint(ts=100))
        obj.shutdown()
        obj.post_process()

        self.assertTrue(os.path.exists(xml_report))
        with open(xml_report) as fds:
            report_content = fds.read()
        self.assertIn('<TestDuration>10.0</TestDuration>', report_content)

    def test_xml_report_test_duration_failed_prepare(self):
        obj = FinalStatus()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        obj.aggregated_second(self.__get_datapoint(ts=100))
        obj.post_process()  # shouldn't raise ValueError because obj.start_time is None

    def test_csv_report_fieldname_order(self):
        obj = FinalStatus()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        csv_report = obj.engine.create_artifact("report", ".csv")
        obj.parameters.merge({
            "dump-csv": csv_report,
        })

        obj.startup()
        obj.aggregated_second(self.__get_datapoint(ts=90))
        obj.aggregated_second(self.__get_datapoint(ts=100))
        obj.shutdown()
        obj.post_process()

        self.assertTrue(os.path.exists(csv_report))
        with open(csv_report) as fds:
            fieldnames = fds.readline().strip().split(",")

        perc_fields = [float(name[5:]) for name in fieldnames if name.startswith('perc_')]
        self.assertTrue(sorted(perc_fields) == perc_fields)

        rc_fields = [float(name[3:]) for name in fieldnames if name.startswith('rc_')]
        self.assertTrue(sorted(rc_fields) == rc_fields)
