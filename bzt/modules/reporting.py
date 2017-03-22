"""
Basics of reporting capabilities

Copyright 2015 BlazeMeter Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""
import copy
import csv
import os
import time
from collections import Counter, OrderedDict
from datetime import datetime

from bzt import TaurusInternalException, TaurusConfigError
from bzt.engine import Reporter
from bzt.modules.aggregator import DataPoint, KPISet, AggregatorListener, ResultsProvider
from bzt.modules.blazemeter import BlazeMeterUploader, CloudProvisioning
from bzt.modules.functional import FunctionalAggregator, FunctionalAggregatorListener
from bzt.modules.passfail import PassFailStatus
from bzt.six import etree, iteritems, string_types
from bzt.utils import get_full_path


class FinalStatus(Reporter, AggregatorListener, FunctionalAggregatorListener):
    """
    A reporter that prints short statistics on test end
    """

    def __init__(self):
        super(FinalStatus, self).__init__()
        self.last_sec = None
        self.cumulative_results = None
        self.start_time = time.time()
        self.end_time = None

    def startup(self):
        self.start_time = time.time()

    def prepare(self):
        super(FinalStatus, self).prepare()
        if isinstance(self.engine.aggregator, ResultsProvider):
            self.engine.aggregator.add_listener(self)
        elif isinstance(self.engine.aggregator, FunctionalAggregator):
            self.engine.aggregator.add_listener(self)

    def aggregated_second(self, data):
        """
        Just store the latest info

        :type data: bzt.modules.aggregator.DataPoint
        """
        self.last_sec = data

    def aggregated_results(self, results, cumulative_results):
        """
        Just store the latest info

        :type cumulative_results: bzt.modules.functional.ResultsTree
        :type results: bzt.modules.functional.ResultsTree
        """
        self.cumulative_results = cumulative_results

    def post_process(self):
        """
        Log basic stats
        """
        super(FinalStatus, self).post_process()

        self.end_time = time.time()

        if self.parameters.get("test-duration", True):
            self.__report_duration()

        if self.last_sec:
            summary_kpi = self.last_sec[DataPoint.CUMULATIVE][""]

            if self.parameters.get("summary", True):
                self.__report_samples_count(summary_kpi)
            if self.parameters.get("percentiles", True):
                self.__report_percentiles(summary_kpi)

            if self.parameters.get("failed-labels", False):
                self.__report_failed_labels(self.last_sec[DataPoint.CUMULATIVE])

            if self.parameters.get("dump-xml", None):
                self.__dump_xml(self.parameters.get("dump-xml"))

            if self.parameters.get("dump-csv", None):
                self.__dump_csv(self.parameters.get("dump-csv"))
        elif self.cumulative_results:
            self.__report_summary()
            report_mode = self.parameters.get("report-tests", "failed")
            if report_mode == "failed":
                self.__report_failed_tests()
            else:
                self.__report_all_tests()

    def __plural(self, count, noun):
        return noun + 's' if count > 1 else noun

    def __report_all_tests(self):
        for test_suite in self.cumulative_results.test_suites():
            for case in self.cumulative_results.test_cases(test_suite):
                full_name = case.test_suite + "." + case.test_case
                self.log.info("Test %s - %s", full_name, case.status)
                print_trace = self.parameters.get("print-stacktrace", True)
                if print_trace and case.error_trace:
                    self.log.info("Stacktrace:\n%s", case.error_trace)

    def __report_failed_tests(self):
        for test_suite in self.cumulative_results.test_suites():
            for case in self.cumulative_results.test_cases(test_suite):
                if case.status in ("FAILED", "BROKEN"):
                    full_name = case.test_suite + "." + case.test_case
                    self.log.info("Test %s failed:\n%s", full_name, case.error_trace)

    def __report_summary(self):
        status_counter = Counter()
        for test_suite in self.cumulative_results.test_suites():
            for case in self.cumulative_results.test_cases(test_suite):
                status_counter[case.status] += 1

        total = sum(count for _, count in iteritems(status_counter))
        self.log.info("Total: %s %s", total, self.__plural(total, 'test')) # FIXME: it's actually not tests, but test cases

    def __report_samples_count(self, summary_kpi_set):
        """
        reports samples count
        """
        if summary_kpi_set[KPISet.SAMPLE_COUNT]:
            err_rate = 100 * summary_kpi_set[KPISet.FAILURES] / float(summary_kpi_set[KPISet.SAMPLE_COUNT])
            self.log.info("Samples count: %s, %.2f%% failures", summary_kpi_set[KPISet.SAMPLE_COUNT], err_rate)

    def __report_percentiles(self, summary_kpi_set):
        """
        reports percentiles
        """

        fmt = "Average times: total %.3f, latency %.3f, connect %.3f"
        self.log.info(fmt, summary_kpi_set[KPISet.AVG_RESP_TIME], summary_kpi_set[KPISet.AVG_LATENCY],
                      summary_kpi_set[KPISet.AVG_CONN_TIME])

        for key in sorted(summary_kpi_set[KPISet.PERCENTILES].keys(), key=float):
            self.log.info("Percentile %.1f%%: %.3f", float(key), summary_kpi_set[KPISet.PERCENTILES][key])

    def __report_failed_labels(self, cumulative):
        """
        reports failed labels
        """
        report_template = "%d failed samples: %s"
        sorted_labels = sorted(cumulative.keys())
        for sample_label in sorted_labels:
            if sample_label != "":
                failed_samples_count = cumulative[sample_label]['fail']
                if failed_samples_count:
                    self.log.info(report_template, failed_samples_count, sample_label)

    def __report_duration(self):
        """
        asks executors start_time and end_time, provides time delta
        """

        date_start = datetime.fromtimestamp(int(self.start_time))
        date_end = datetime.fromtimestamp(int(self.end_time))
        self.log.info("Test duration: %s", date_end - date_start)

    def __dump_xml(self, filename):
        self.log.info("Dumping final status as XML: %s", filename)
        root = etree.Element("FinalStatus")
        report_info = get_bza_report_info(self.engine, self.log)
        if report_info:
            link, _ = report_info[0]
            report_element = etree.Element("ReportURL")
            report_element.text = link
            root.append(report_element)
        if self.last_sec:
            for label, kpiset in iteritems(self.last_sec[DataPoint.CUMULATIVE]):
                root.append(self.__get_xml_summary(label, kpiset))

        with open(get_full_path(filename), 'wb') as fhd:
            tree = etree.ElementTree(root)
            tree.write(fhd, pretty_print=True, encoding="UTF-8", xml_declaration=True)

    def __get_xml_summary(self, label, kpiset):
        elem = etree.Element("Group", label=label)
        for kpi_name, kpi_val in iteritems(kpiset):
            if kpi_name in ('errors', 'rt'):
                continue

            if isinstance(kpi_val, dict):
                for param_name, param_val in iteritems(kpi_val):
                    elem.append(self.__get_kpi_xml(kpi_name, param_val, param_name))
            else:
                elem.append(self.__get_kpi_xml(kpi_name, kpi_val))

        return elem

    def __get_kpi_xml(self, kpi_name, kpi_val, param=None):
        kpi = etree.Element(kpi_name)
        kpi.attrib['value'] = self.__val_to_str(kpi_val)
        elm_name = etree.Element("name")
        elm_name.text = kpi_name
        if param is not None:
            kpi.attrib['param'] = self.__val_to_str(param)
            elm_name.text += "/" + param

        kpi.append(elm_name)

        elm_value = etree.Element("value")
        elm_value.text = self.__val_to_str(kpi_val)
        kpi.append(elm_value)

        return kpi

    def __val_to_str(self, kpi_val):
        if isinstance(kpi_val, float):
            return '%.5f' % kpi_val
        elif isinstance(kpi_val, int):
            return '%d' % kpi_val
        elif isinstance(kpi_val, string_types):
            return kpi_val
        else:
            raise TaurusInternalException("Unhandled kpi type: %s" % type(kpi_val))

    def __dump_csv(self, filename):
        self.log.info("Dumping final status as CSV: %s", filename)
        # FIXME: what if there's no last_sec
        with open(get_full_path(filename), 'wt') as fhd:
            writer = csv.DictWriter(fhd, self.__get_csv_dict('', self.last_sec[DataPoint.CUMULATIVE]['']).keys())
            writer.writeheader()
            for label, kpiset in iteritems(self.last_sec[DataPoint.CUMULATIVE]):
                writer.writerow(self.__get_csv_dict(label, kpiset))

    def __get_csv_dict(self, label, kpiset):
        kpi_copy = copy.deepcopy(kpiset)
        # sort label
        res = OrderedDict()
        for key in sorted(kpi_copy.keys()):
            res[key] = kpi_copy[key]

        for level, val in iteritems(kpiset[KPISet.PERCENTILES]):
            res['perc_%s' % level] = val

        for rcd, val in iteritems(kpiset[KPISet.RESP_CODES]):
            res['rc_%s' % rcd] = val

        for key in res:
            if isinstance(res[key], float):
                res[key] = "%.5f" % res[key]

        del res['errors']
        del res['rt']
        del res['rc']
        del res['perc']
        res['label'] = label
        return res


class JUnitXMLReporter(Reporter, AggregatorListener):
    """
    A reporter that exports results in Jenkins JUnit XML format.
    """

    def __init__(self):
        super(JUnitXMLReporter, self).__init__()
        self.last_second = None
        self.report_file_path = None

    def prepare(self):
        if isinstance(self.engine.aggregator, ResultsProvider):
            self.engine.aggregator.add_listener(self)

    def aggregated_second(self, data):
        self.last_second = data

    def post_process(self):
        """
        Get report data, generate xml report.
        """
        filename = self.parameters.get("filename", None)
        if not filename:
            filename = self.engine.create_artifact(XUnitFileWriter.REPORT_FILE_NAME, XUnitFileWriter.REPORT_FILE_EXT)
        self.parameters["filename"] = filename  # reflect it in effective config

        test_data_source = self.parameters.get("data-source", "sample-labels")

        if test_data_source == "sample-labels":
            if not self.last_second:
                self.log.warning("No last second data to generate XUnit.xml")
            else:
                writer = XUnitFileWriter(self.engine, 'sample_labels')
                self.process_sample_labels(writer)
                writer.save_report(filename)
        elif test_data_source == "pass-fail":
            writer = XUnitFileWriter(self.engine, 'bzt_pass_fail')
            self.process_pass_fail(writer)
            writer.save_report(filename)
        else:
            raise TaurusConfigError("Unsupported data source: %s" % test_data_source)

        self.report_file_path = filename  # TODO: just for backward compatibility, remove later

    def process_sample_labels(self, xunit):
        """
        :type xunit: XUnitFileWriter
        """
        labels = self.last_second[DataPoint.CUMULATIVE]

        for key in sorted(labels.keys()):
            if key == "":  # skip total label
                continue

            errors = []
            for er_dict in labels[key][KPISet.ERRORS]:
                err_message = str(er_dict["rc"])
                err_type = str(er_dict["msg"])
                err_desc = "total errors of this type:" + str(er_dict["cnt"])
                err_element = etree.Element("error", message=err_message, type=err_type)
                err_element.text = err_desc
                errors.append(err_element)

            xunit.add_test_case(key, errors)

    def process_pass_fail(self, xunit):
        """
        :type xunit: XUnitFileWriter
        """
        mods = self.engine.reporters + self.engine.services  # TODO: remove it after passfail is only reporter
        pass_fail_objects = [_x for _x in mods if isinstance(_x, PassFailStatus)]
        self.log.debug("Processing passfail objects: %s", pass_fail_objects)
        fail_criteria = []
        for pf_obj in pass_fail_objects:
            if pf_obj.criteria:
                for _fc in pf_obj.criteria:
                    fail_criteria.append(_fc)

        for fc_obj in fail_criteria:
            if 'label' in fc_obj.config:
                data = (fc_obj.config['subject'], fc_obj.config['label'], fc_obj.config['condition'],
                        fc_obj.config['threshold'])
                tpl = "%s of %s%s%s"
            else:
                data = (fc_obj.config['subject'], fc_obj.config['condition'], fc_obj.config['threshold'])
                tpl = "%s%s%s"
            if fc_obj.config['timeframe']:
                tpl += " for %s"
                data += (fc_obj.config['timeframe'],)
            disp_name = tpl % data

            if fc_obj.is_triggered and fc_obj.fail:
                errors = [etree.Element("error", message=str(fc_obj), type="pass/fail criteria triggered")]
            else:
                errors = ()

            xunit.add_test_case(disp_name, errors)


def get_bza_report_info(engine, log):
    """
    :return: [(url, test), (url, test), ...]
    """
    result = []
    if isinstance(engine.provisioning, CloudProvisioning):
        cloud_prov = engine.provisioning
        test_name = cloud_prov.settings.get('test', None)
        report_url = cloud_prov.results_url
        result.append((report_url, test_name if test_name is not None else report_url))
    else:
        bza_reporters = [_x for _x in engine.reporters if isinstance(_x, BlazeMeterUploader)]
        """:type : list[bzt.modules.blazemeter.BlazeMeterUploader]"""
        for bza_reporter in bza_reporters:
            if bza_reporter.results_url:
                test_name = bza_reporter.parameters.get("test", None)
                report_url = bza_reporter.results_url
                result.append((report_url, test_name if test_name is not None else report_url))

        if len(result) > 1:
            log.warning("More than one blazemeter reporter found")
    return result


class XUnitFileWriter(object):
    REPORT_FILE_NAME = "xunit"
    REPORT_FILE_EXT = ".xml"

    def __init__(self, engine, suite_name):
        """
        :type engine: bzt.engine.Engine
        :type suite_name: str
        """
        super(XUnitFileWriter, self).__init__()
        self.engine = engine
        self.log = engine.log.getChild(self.__class__.__name__)
        self.test_suite = etree.Element("testsuite", name=suite_name, package="bzt")
        bza_report_info = get_bza_report_info(engine, self.log)
        self.class_name = bza_report_info[0][1] if bza_report_info else "bzt-" + str(self.__hash__())
        self.report_urls = ["BlazeMeter report link: %s\n" % info_item[0] for info_item in bza_report_info]

    def save_report(self, fname):
        """
        :type fname: str
        """
        try:
            if os.path.exists(fname):
                self.log.warning("File %s already exists, it will be overwritten", fname)
            else:
                dirname = os.path.dirname(fname)
                if dirname and not os.path.exists(dirname):
                    os.makedirs(dirname)

            etree_obj = etree.ElementTree(self.test_suite)
            self.log.info("Writing JUnit XML report into: %s", fname)
            with open(get_full_path(fname), 'wb') as _fds:
                etree_obj.write(_fds, xml_declaration=True, encoding="UTF-8", pretty_print=True)
        except BaseException:
            raise TaurusInternalException("Cannot create file %s" % fname)

    def add_test_case(self, case_name, children=()):
        """
        :type case_name: str
        :type children: list[bzt.six.etree.Element]
        """
        test_case = etree.Element("testcase", classname=self.class_name, name=case_name)
        if self.report_urls:
            system_out_etree = etree.SubElement(test_case, "system-out")
            system_out_etree.text = "".join(self.report_urls)

        for child in children:
            test_case.append(child)
        self.test_suite.append(test_case)
