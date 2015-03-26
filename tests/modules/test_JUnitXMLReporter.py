__author__ = 'Coeurl'

from tests import BZTestCase, setup_test_logging
from tests.mocks import EngineEmul
from bzt.modules.reporting import JUnitXMLReporter
from bzt.utils import  BetterDict
from collections import Counter, defaultdict
import hashlib

import os

setup_test_logging()


class TestJUnitXML(BZTestCase):

    def test_prepare_filename_in_settings(self):
        #test path parameter from config
        obj = JUnitXMLReporter()
        obj.engine = EngineEmul()
        obj.settings = BetterDict()

        path_from_config = "/tmp/juintxml.xml"

        obj.settings.merge({"filename":path_from_config, "data-source":"finalstats"})

        obj.prepare()

        datapoint=BetterDict()
        test_data = BetterDict()

        test_data[""] = defaultdict(None,\
                        {'avg_ct': 7.890211417203362e-06,
                        'rt': Counter({0.0: 32160, 0.001: 24919, 0.002: 1049, 0.003: 630, 0.004: 224, 0.005: 125,
                                       0.006: 73, 0.007: 46, 0.008: 32, 0.009: 20, 0.011: 8, 0.01: 8, 0.017: 3,
                                       0.016: 3, 0.014: 3, 0.013: 3, 0.04: 2, 0.012: 2, 0.079: 1, 0.081: 1,
                                       0.019: 1, 0.015: 1}),
                        'errors': [{'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                                    'urls': Counter({'http://192.168.25.8/': 7373}), 'rc': '403'}],
                        'stdev_rt': 0.04947974228872108,
                        'avg_lt': 0.0002825639815220692,
                        'rc': Counter({'304': 29656, '403': 29656, '200': 2}),
                        'perc': defaultdict(None, {'95.0': 0.001, '0.0': 0.0, '99.9': 0.008, '90.0': 0.001,
                                                   '100.0': 0.081, '99.0': 0.003, '50.0': 0.0}),
                        'succ': 29658,
                        'throughput': 59314,
                        'concurrency': 0,
                        'avg_rt': 0.0005440536804127192,
                        'fail': 29656})

        datapoint["cumulative"] = test_data
        obj.aggregated_second(datapoint)

        obj.post_process()
        #check if result xml file exists
        self.assertTrue(os.path.exists(path_from_config))
        #we should delete it after tests
        #os.remove(self.report_file|_path)

        #search

    def test_prepare_no_filename_in_settings(self):
        obj = JUnitXMLReporter()
        obj.engine = EngineEmul()
        obj.settings = BetterDict()

        obj.settings.merge({"data-source":"finalstats"})

        obj.prepare()
        datapoint=BetterDict()
        test_data = BetterDict()
        test_data[""] = defaultdict(None,\
                        {'avg_ct': 7.890211417203362e-06,
                        'rt': Counter({0.0: 32160, 0.001: 24919, 0.002: 1049, 0.003: 630, 0.004: 224, 0.005: 125,
                                       0.006: 73, 0.007: 46, 0.008: 32, 0.009: 20, 0.011: 8, 0.01: 8, 0.017: 3,
                                       0.016: 3, 0.014: 3, 0.013: 3, 0.04: 2, 0.012: 2, 0.079: 1, 0.081: 1,
                                       0.019: 1, 0.015: 1}),
                        'errors': [{'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                                    'urls': Counter({'http://192.168.25.8/': 7373}), 'rc': '403'}],
                        'stdev_rt': 0.04947974228872108,
                        'avg_lt': 0.0002825639815220692,
                        'rc': Counter({'304': 29656, '403': 29656, '200': 2}),
                        'perc': defaultdict(None, {'95.0': 0.001, '0.0': 0.0, '99.9': 0.008, '90.0': 0.001,
                                                   '100.0': 0.081, '99.0': 0.003, '50.0': 0.0}),
                        'succ': 29658,
                        'throughput': 59314,
                        'concurrency': 0,
                        'avg_rt': 0.0005440536804127192,
                        'fail': 29656})

        datapoint["cumulative"] = test_data

        obj.aggregated_second(datapoint)
        obj.post_process()
        self.assertTrue(os.path.exists(obj.report_file_path))
        #we should delete it after tests
        #os.remove(self.report_file|_path)

    def test_xml_format_no_passfail(self):
        #generate xml, compare.

        ideal_xml_hash = 'c1784943f3f6d06e90ff2e8efba38625'


        obj = JUnitXMLReporter()
        obj.engine = EngineEmul()
        obj.settings = BetterDict()

        path_from_config = "/tmp/junitxml2.xml"

        obj.settings.merge({"filename":path_from_config})

        obj.prepare()

        test_data = BetterDict()

        test_data[""] = defaultdict(None,\
                        {'avg_ct': 7.890211417203362e-06,
                        'rt': Counter({0.0: 32160, 0.001: 24919, 0.002: 1049, 0.003: 630, 0.004: 224, 0.005: 125,
                                       0.006: 73, 0.007: 46, 0.008: 32, 0.009: 20, 0.011: 8, 0.01: 8, 0.017: 3,
                                       0.016: 3, 0.014: 3, 0.013: 3, 0.04: 2, 0.012: 2, 0.079: 1, 0.081: 1,
                                       0.019: 1, 0.015: 1}),
                        'errors': [{'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                                    'urls': Counter({'http://192.168.25.8/': 7373}), 'rc': '403'}],
                        'stdev_rt': 0.04947974228872108,
                        'avg_lt': 0.0002825639815220692,
                        'rc': Counter({'304': 29656, '403': 29656, '200': 2}),
                        'perc': defaultdict(None, {'95.0': 0.001, '0.0': 0.0, '99.9': 0.008, '90.0': 0.001,
                                                   '100.0': 0.081, '99.0': 0.003, '50.0': 0.0}),
                        'succ': 29658,
                        'throughput': 59314,
                        'concurrency': 0,
                        'avg_rt': 0.0005440536804127192,
                        'fail': 29656})


        test_data["http://192.168.1.1/somequery"] = defaultdict(None,
                        {'avg_ct': 9.609548856969457e-06,
                         'rt': Counter({0.0: 17219, 0.001: 11246, 0.002: 543, 0.003: 341, 0.004: 121,\
                                        0.005: 66, 0.006: 36, 0.007: 33, 0.008: 18, 0.009: 12, 0.011: 6,\
                                        0.01: 5, 0.013: 2, 0.017: 2, 0.012: 2, 0.079: 1, 0.016: 1,\
                                        0.014: 1, 0.019: 1, 0.04: 1, 0.081: 1}),
                         'errors': [],
                         'stdev_rt': 0.04073402130687656,
                         'avg_lt': 1.7196034796682178e-06,
                         'rc': Counter({'304': 29656, '200': 2}),
                         'perc': defaultdict(None, {'95.0': 0.001, '0.0': 0.0, '99.9': 0.009, '90.0': 0.001,\
                                                    '100.0': 0.081, '99.0': 0.004, '50.0': 0.0}),
                         'succ': 29658,
                         'throughput': 29658,
                         'concurrency': 0,
                         'avg_rt': 0.0005164542450603551, 'fail': 0})

        test_data["http://192.168.1.1/anotherquery"] = defaultdict(None,
                        {'avg_ct': 6.1707580253574335e-06,
                         'rt': Counter({0.0: 14941, 0.001: 13673, 0.002: 506, 0.003: 289, 0.004: 103,\
                                        0.005: 59, 0.006: 37, 0.008: 14, 0.007: 13, 0.009: 8, 0.01: 3,\
                                        0.011: 2, 0.016: 2, 0.014: 2, 0.017: 1, 0.013: 1, 0.015: 1, 0.04: 1}),
                         'errors': [{'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                                     'urls': Counter({'http://192.168.25.8/': 7373}), 'rc': '403'}],
                         'stdev_rt': 0.032465137860758844,
                         'avg_lt': 0.0005634272997032645,
                         'rc': Counter({'403': 29656}),
                         'perc': defaultdict(None, {'95.0': 0.001, '0.0': 0.0, '99.9': 0.008, '90.0': 0.001,\
                                                    '100.0': 0.04, '99.0': 0.003, '50.0': 0.0}),
                         'succ': 0,
                         'throughput': 29656,
                         'concurrency': 0,
                         'avg_rt': 0.0005716549770704078,
                         'fail': 29656})

        test_data["http://192.168.100.100/somequery"] = defaultdict(None,
                        {'avg_ct': 9.609548856969457e-06,
                         'rt': Counter({0.0: 17219, 0.001: 11246, 0.002: 543, 0.003: 341, 0.004: 121,\
                                        0.005: 66, 0.006: 36, 0.007: 33, 0.008: 18, 0.009: 12, 0.011: 6,\
                                        0.01: 5, 0.013: 2, 0.017: 2, 0.012: 2, 0.079: 1, 0.016: 1,\
                                        0.014: 1, 0.019: 1, 0.04: 1, 0.081: 1}),
                         'errors': [],
                         'stdev_rt': 0.04073402130687656,
                         'avg_lt': 1.7196034796682178e-06,
                         'rc': Counter({'304': 29656, '200': 2}),
                         'perc': defaultdict(None, {'95.0': 0.001, '0.0': 0.0, '99.9': 0.009, '90.0': 0.001,\
                                                    '100.0': 0.081, '99.0': 0.004, '50.0': 0.0}),
                         'succ': 29658,
                         'throughput': 29658,
                         'concurrency': 0,
                         'avg_rt': 0.0005164542450603551, 'fail': 0})

        datapoint = BetterDict()
        datapoint["cumulative"]=test_data
        obj.aggregated_second(datapoint)

        obj.post_process()

        with open(obj.report_file_path, 'rb') as fds:
            f_contents = fds.read()


        self.assertEqual(hashlib.md5(f_contents).hexdigest(), ideal_xml_hash)
        #check if result xml file exists

    def test_xml_format_with_passfail(self):
        pass #fail
