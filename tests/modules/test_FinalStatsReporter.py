from io import StringIO
from tests import BZTestCase
from tests.mocks import EngineEmul
from bzt.modules.reporting import FinalStatus
from bzt.utils import BetterDict
from collections import Counter, defaultdict
import six
from bzt.modules.aggregator import DataPoint, KPISet


class logger_mock:
    def __init__(self):
        self.info_buf = StringIO()

    def info(self, str_template, *args):
        if args:
            self.info_buf.write(six.u(str_template % args))
            self.info_buf.write(six.u("\n"))
        else:
            self.info_buf.write(six.u(str_template))
            self.info_buf.write(six.u("\n"))


class TestFinalStatsReporter(BZTestCase):
    def test_log_messages_failed_labels(self):
        obj = FinalStatus()
        obj.engine = EngineEmul
        obj.settings = BetterDict()
        obj.log = logger_mock()
        obj.settings.merge({"failed-labels": True, "percentiles": False, "samples-count": False})

        datapoint = DataPoint(None, None)
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
             KPISet.PERCENTILES: defaultdict(None, {'95.0': 0.001, '0.0': 0.0, '99.9': 0.008, '90.0': 0.001,
                                                    '100.0': 0.081, '99.0': 0.003, '50.0': 0.0}),
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
             KPISet.PERCENTILES: defaultdict(None, {'95.0': 0.001, '0.0': 0.0,
                                                    '99.9': 0.009,
                                                    '90.0': 0.001,
                                                    '100.0': 0.081,
                                                    '99.0': 0.004,
                                                    '50.0': 0.0}),
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
             KPISet.PERCENTILES: defaultdict(None, {'95.0': 0.001, '0.0': 0.0,
                                                    '99.9': 0.008, '90.0': 0.001,
                                                    '100.0': 0.04, '99.0': 0.003,
                                                    '50.0': 0.0}),
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
             KPISet.PERCENTILES: defaultdict(None, {'95.0': 0.001, '0.0': 0.0,
                                                    '99.9': 0.009, '90.0': 0.001,
                                                    '100.0': 0.081, '99.0': 0.004,
                                                    '50.0': 0.0}),
             KPISet.SUCCESSES: 29658,
             KPISet.SAMPLE_COUNT: 29658,
             KPISet.CONCURRENCY: 0,
             KPISet.AVG_RESP_TIME: 0.0005164542450603551,
             KPISet.FAILURES: 0})

        obj.last_sec = datapoint
        obj.post_process()
        self.assertEqual("29656 failed samples: http://192.168.1.1/anotherquery\n", obj.log.info_buf.getvalue())

    def test_log_messages_percentiles(self):
        obj = FinalStatus()
        obj.engine = EngineEmul
        obj.settings = BetterDict()
        obj.log = logger_mock()
        obj.settings.merge({"failed-labels": False, "percentiles": True, "samples-count": False})

        datapoint = DataPoint(None, None)

        cumul_data = KPISet.from_dict(
            {KPISet.AVG_CONN_TIME: 7.890211417203362e-06,
             KPISet.RESP_TIMES: Counter(
                 {0.0: 32160, 0.001: 24919, 0.002: 1049, 0.003: 630, 0.004: 224, 0.005: 125,
                  0.006: 73, 0.007: 46, 0.008: 32, 0.009: 20, 0.011: 8, 0.01: 8, 0.017: 3,
                  0.016: 3, 0.014: 3, 0.013: 3, 0.04: 2, 0.012: 2, 0.079: 1, 0.081: 1,
                  0.019: 1, 0.015: 1}),
             KPISet.ERRORS: [{'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                              'urls': Counter({'http://192.168.25.8/': 7373}), KPISet.RESP_CODES: '403'}],
             KPISet.STDEV_RESP_TIME: 0.04947974228872108,
             KPISet.AVG_LATENCY: 0.0002825639815220692,
             KPISet.RESP_CODES: Counter({'304': 29656, '403': 29656, '200': 2}),
             KPISet.PERCENTILES: defaultdict(None, {'95.0': 0.001, '0.0': 0.0, '99.9': 0.008, '90.0': 0.001,
                                                    '100.0': 0.081, '99.0': 0.003, '50.0': 0.0}),
             KPISet.SUCCESSES: 29658,
             KPISet.SAMPLE_COUNT: 59314,
             KPISet.CONCURRENCY: 0,
             KPISet.AVG_RESP_TIME: 0.0005440536804127192,
             KPISet.FAILURES: 29656})

        datapoint[DataPoint.CUMULATIVE][""] = cumul_data
        obj.last_sec = datapoint

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
        self.assertEqual(target_output, obj.log.info_buf.getvalue())

    def test_log_messages_samples_count(self):
        obj = FinalStatus()
        obj.engine = EngineEmul
        obj.settings = BetterDict()
        obj.log = logger_mock()
        obj.settings.merge({"failed-labels": False, "percentiles": False, "samples-count": True})

        datapoint = DataPoint(None, None)

        cumul_data = KPISet.from_dict(
            {KPISet.AVG_CONN_TIME: 7.890211417203362e-06,
             KPISet.RESP_TIMES: Counter(
                 {0.0: 32160, 0.001: 24919, 0.002: 1049, 0.003: 630, 0.004: 224, 0.005: 125,
                  0.006: 73, 0.007: 46, 0.008: 32, 0.009: 20, 0.011: 8, 0.01: 8, 0.017: 3,
                  0.016: 3, 0.014: 3, 0.013: 3, 0.04: 2, 0.012: 2, 0.079: 1, 0.081: 1,
                  0.019: 1, 0.015: 1}),
             KPISet.ERRORS: [{'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                              'urls': Counter({'http://192.168.25.8/': 7373}), KPISet.RESP_CODES: '403'}],
             KPISet.STDEV_RESP_TIME: 0.04947974228872108,
             KPISet.AVG_LATENCY: 0.0002825639815220692,
             KPISet.RESP_CODES: Counter({'304': 29656, '403': 29656, '200': 2}),
             KPISet.PERCENTILES: defaultdict(None, {'95.0': 0.001, '0.0': 0.0, '99.9': 0.008, '90.0': 0.001,
                                                    '100.0': 0.081, '99.0': 0.003, '50.0': 0.0}),
             KPISet.SUCCESSES: 29658,
             KPISet.SAMPLE_COUNT: 59314,
             KPISet.CONCURRENCY: 0,
             KPISet.AVG_RESP_TIME: 0.0005440536804127192,
             KPISet.FAILURES: 29656})

        datapoint[DataPoint.CUMULATIVE][""] = cumul_data
        obj.last_sec = datapoint

        obj.post_process()

        self.assertEqual("Samples count: 59314, 50.00% failures\n", obj.log.info_buf.getvalue())