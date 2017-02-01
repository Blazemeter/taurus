import logging
import os
import tempfile
from collections import Counter

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.blazemeter import BlazeMeterUploader, CloudProvisioning
from bzt.modules.passfail import PassFailStatus, DataCriterion
from bzt.modules.reporting import JUnitXMLReporter
from bzt.six import etree
from bzt.utils import BetterDict
from tests import BZTestCase
from tests.mocks import EngineEmul


class TestJUnitXML(BZTestCase):
    def test_prepare_filename_in_settings(self):
        # test path parameter from config
        obj = JUnitXMLReporter()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()

        path_from_config = tempfile.mktemp(suffix='.xml', prefix='junit-xml-path-in-settings',
                                           dir=obj.engine.artifacts_dir)

        obj.parameters.merge({"filename": path_from_config, "data-source": "sample-labels"})

        obj.prepare()

        datapoint = DataPoint(0, [])

        cumul_data = KPISet.from_dict({
            KPISet.AVG_CONN_TIME: 7.890211417203362e-06,
            KPISet.RESP_TIMES: Counter({
                0.0: 32160, 0.001: 24919, 0.002: 1049, 0.003: 630, 0.004: 224, 0.005: 125,
                0.006: 73, 0.007: 46, 0.008: 32, 0.009: 20, 0.011: 8, 0.01: 8, 0.017: 3,
                0.016: 3, 0.014: 3, 0.013: 3, 0.04: 2, 0.012: 2, 0.079: 1, 0.081: 1,
                0.019: 1, 0.015: 1
            }),
            KPISet.ERRORS: [{'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                             'urls': Counter({'http://192.168.25.8/': 7373}), KPISet.RESP_CODES: '403'}],
            KPISet.STDEV_RESP_TIME: 0.04947974228872108,
            KPISet.AVG_LATENCY: 0.0002825639815220692,
            KPISet.RESP_CODES: Counter({'304': 29656, '403': 29656, '200': 2}),
            KPISet.PERCENTILES: {'95.0': 0.001, '0.0': 0.0, '99.9': 0.008, '90.0': 0.001, '100.0': 0.081,
                                 '99.0': 0.003, '50.0': 0.0},
            KPISet.SUCCESSES: 29658,
            KPISet.SAMPLE_COUNT: 59314,
            KPISet.CONCURRENCY: 0,
            KPISet.AVG_RESP_TIME: 0.0005440536804127192,
            KPISet.FAILURES: 29656})

        datapoint[DataPoint.CUMULATIVE][""] = cumul_data
        obj.aggregated_second(datapoint)
        obj.post_process()

        self.assertTrue(os.path.exists(obj.report_file_path))

    def test_prepare_no_filename_in_settings(self):
        obj = JUnitXMLReporter()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()

        obj.parameters.merge({"data-source": "sample-labels"})

        obj.prepare()
        datapoint = DataPoint(0, [])

        cumul_data = KPISet.from_dict({
            KPISet.AVG_CONN_TIME: 7.890211417203362e-06,
            KPISet.RESP_TIMES: Counter({
                0.0: 32160, 0.001: 24919, 0.002: 1049, 0.003: 630, 0.004: 224, 0.005: 125,
                0.006: 73, 0.007: 46, 0.008: 32, 0.009: 20, 0.011: 8, 0.01: 8, 0.017: 3,
                0.016: 3, 0.014: 3, 0.013: 3, 0.04: 2, 0.012: 2, 0.079: 1, 0.081: 1,
                0.019: 1, 0.015: 1
            }),
            KPISet.ERRORS: [{'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                             'urls': Counter({'http://192.168.25.8/': 7373}), KPISet.RESP_CODES: '403'}],
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

        datapoint[DataPoint.CUMULATIVE][""] = cumul_data

        obj.aggregated_second(datapoint)
        obj.post_process()

        self.assertTrue(os.path.exists(obj.report_file_path))

    def test_xml_format_sample_labels(self):
        # generate xml, compare hash

        obj = JUnitXMLReporter()
        obj.engine = EngineEmul()
        rep = BlazeMeterUploader()
        rep.results_url = "http://report/123"
        obj.engine.reporters.append(rep)
        obj.parameters = BetterDict()

        path_from_config = tempfile.mktemp(suffix='.xml', prefix='junit-xml-sample-labels',
                                           dir=obj.engine.artifacts_dir)

        # data-source: finalstats by default
        obj.parameters.merge({"filename": path_from_config})

        obj.prepare()

        datapoint = DataPoint(0, [])
        cumul_data = datapoint[DataPoint.CUMULATIVE]

        cumul_data[""] = KPISet.from_dict({
            KPISet.AVG_CONN_TIME: 7.890211417203362e-06,
            KPISet.RESP_TIMES: Counter({
                0.0: 32160, 0.001: 24919, 0.002: 1049, 0.003: 630, 0.004: 224, 0.005: 125,
                0.006: 73, 0.007: 46, 0.008: 32, 0.009: 20, 0.011: 8, 0.01: 8, 0.017: 3,
                0.016: 3, 0.014: 3, 0.013: 3, 0.04: 2, 0.012: 2, 0.079: 1, 0.081: 1,
                0.019: 1, 0.015: 1}),
            KPISet.ERRORS: [{'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                             'urls': Counter({'http://192.168.1.1/anotherquery': 7373}),
                             KPISet.RESP_CODES: '403'}],
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

        cumul_data["http://192.168.1.1/somequery"] = KPISet.from_dict({
            KPISet.AVG_CONN_TIME: 9.609548856969457e-06,
            KPISet.RESP_TIMES: Counter({
                0.0: 17219, 0.001: 11246, 0.002: 543, 0.003: 341,
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

        cumul_data["http://192.168.1.1/anotherquery"] = KPISet.from_dict({
            KPISet.AVG_CONN_TIME: 6.1707580253574335e-06,
            KPISet.RESP_TIMES: Counter({0.0: 14941, 0.001: 13673, 0.002: 506,
                                        0.003: 289, 0.004: 103,
                                        0.005: 59, 0.006: 37, 0.008: 14,
                                        0.007: 13, 0.009: 8, 0.01: 3,
                                        0.011: 2, 0.016: 2, 0.014: 2,
                                        0.017: 1, 0.013: 1, 0.015: 1,
                                        0.04: 1}),
            KPISet.ERRORS: [
                {'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                 'urls': Counter({'http://192.168.1.1/anotherquery': 7373}),
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

        cumul_data["http://192.168.100.100/somequery"] = KPISet.from_dict({
            KPISet.AVG_CONN_TIME: 9.609548856969457e-06,
            KPISet.RESP_TIMES: Counter({
                0.0: 17219, 0.001: 11246, 0.002: 543,
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

        obj.aggregated_second(datapoint)

        obj.post_process()

        with open(obj.report_file_path, 'rb') as fds:
            f_contents = fds.read()

        logging.info("File: %s", f_contents)
        xml_tree = etree.fromstring(f_contents)
        self.assertEqual('testsuite', xml_tree.tag)
        self.assertListEqual(['sample_labels', "bzt"], xml_tree.values())
        test_cases = xml_tree.getchildren()
        self.assertEqual(3, len(test_cases))
        self.assertEqual('testcase', test_cases[0].tag)
        self.assertEqual('error', test_cases[0].getchildren()[1].tag)
        self.assertEqual('system-out', test_cases[0].getchildren()[0].tag)
        self.assertIn('BlazeMeter report link: http://report/123', test_cases[0].getchildren()[0].text)

    def test_xml_format_passfail(self):
        obj = JUnitXMLReporter()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()
        obj.engine.provisioning = CloudProvisioning()
        obj.engine.provisioning.results_url = "http://test/report/123"

        pass_fail1 = PassFailStatus()

        fc1_triggered = DataCriterion({'stop': True, 'label': 'Sample 1 Triggered', 'fail': True,
                                       'timeframe': -1, 'threshold': '150ms', 'condition': '<', 'subject': 'avg-rt'},
                                      pass_fail1)

        fc1_not_triggered = DataCriterion({'stop': True, 'label': 'Sample 1 Not Triggered', 'fail': True,
                                           'timeframe': -1, 'threshold': '300ms', 'condition': '>',
                                           'subject': 'avg-rt'},
                                          pass_fail1)

        pass_fail2 = PassFailStatus()

        fc2_triggered = DataCriterion({'stop': True, 'label': 'Sample 2 Triggered', 'fail': True, 'timeframe': -1,
                                       'threshold': '150ms', 'condition': '<=', 'subject': 'avg-rt'}, pass_fail1)

        fc2_not_triggered = DataCriterion({'stop': True, 'label': 'Sample 2 Not Triggered', 'fail': True,
                                           'timeframe': -1, 'threshold': '300ms', 'condition': '=',
                                           'subject': 'avg-rt'},
                                          pass_fail1)

        pass_fail1.criteria.append(fc1_triggered)
        pass_fail1.criteria.append(fc1_not_triggered)
        pass_fail2.criteria.append(fc2_triggered)
        pass_fail2.criteria.append(fc2_not_triggered)

        fc1_triggered.is_triggered = True
        fc2_triggered.is_triggered = True

        obj.engine.reporters.append(pass_fail1)
        obj.engine.reporters.append(pass_fail2)
        obj.engine.reporters.append(BlazeMeterUploader())

        path_from_config = tempfile.mktemp(suffix='.xml', prefix='junit-xml_passfail', dir=obj.engine.artifacts_dir)

        obj.parameters.merge({"filename": path_from_config, "data-source": "pass-fail"})
        obj.prepare()
        obj.last_second = DataPoint(0)
        obj.post_process()

        with open(obj.report_file_path, 'rb') as fds:
            f_contents = fds.read()

        logging.info("File: %s", f_contents)
        xml_tree = etree.fromstring(f_contents)
        self.assertEqual('testsuite', xml_tree.tag)
        test_cases = xml_tree.getchildren()
        self.assertEqual(4, len(test_cases))
        self.assertEqual('testcase', test_cases[0].tag)
        self.assertEqual('error', test_cases[0].getchildren()[1].tag)
        self.assertEqual('error', test_cases[2].getchildren()[1].tag)

        sys_out = test_cases[0].getchildren()[0]
        self.assertEqual('system-out', sys_out.tag)
        self.assertIn('Cloud report link: http://test/report/123', sys_out.text)

    def test_report_criteria_without_label(self):
        obj = JUnitXMLReporter()
        obj.engine = EngineEmul()
        obj.parameters = BetterDict()

        pass_fail = PassFailStatus()

        criteria = DataCriterion({'stop': True, 'fail': True, 'timeframe': -1, 'threshold': '150ms',
                                  'condition': '<', 'subject': 'avg-rt'},
                                 pass_fail)
        pass_fail.criteria.append(criteria)
        criteria.is_triggered = True

        obj.engine.reporters.append(pass_fail)

        path_from_config = tempfile.mktemp(suffix='.xml', prefix='junit-xml_passfail', dir=obj.engine.artifacts_dir)
        obj.parameters.merge({"filename": path_from_config, "data-source": "pass-fail"})
        obj.prepare()
        obj.last_second = DataPoint(0)
        obj.post_process()
