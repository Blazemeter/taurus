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
import locale
import os
import sys
import time
from collections import Counter, OrderedDict
from datetime import datetime

from terminaltables import AsciiTable

from bzt import TaurusInternalException, TaurusConfigError
from bzt.engine import Reporter
from bzt.modules.aggregator import DataPoint, KPISet, AggregatorListener, ResultsProvider
from bzt.modules.blazemeter import BlazeMeterUploader, CloudProvisioning
from bzt.modules.functional import FunctionalAggregatorListener
from bzt.modules.passfail import PassFailStatus
from bzt.utils import etree, iteritems, get_full_path, is_windows

if is_windows():
    from terminaltables import AsciiTable as SingleTable
else:
    from terminaltables import SingleTable


class FinalStatus(Reporter, AggregatorListener, FunctionalAggregatorListener):
    """
    A reporter that prints short statistics on test end
    """

    def __init__(self):
        super(FinalStatus, self).__init__()
        self.last_sec = None
        self.cumulative_results = None
        self.start_time = time.time()  # default value
        self.end_time = time.time()
        self.first_ts = float("inf")
        self.last_ts = 0

    def startup(self):
        self.start_time = time.time()

    def prepare(self):
        super(FinalStatus, self).prepare()
        if isinstance(self.engine.aggregator, ResultsProvider):
            self.engine.aggregator.add_listener(self)
        elif self.engine.is_functional_mode():
            self.engine.aggregator.add_listener(self)

    def aggregated_second(self, data):
        """
        Just store the latest info

        :type data: bzt.modules.aggregator.DataPoint
        """
        self.first_ts = min(self.first_ts, data[DataPoint.TIMESTAMP])
        self.last_ts = max(self.last_ts, data[DataPoint.TIMESTAMP])
        self.last_sec = data

    def aggregated_results(self, results, cumulative_results):
        """
        Just store the latest info

        :type cumulative_results: bzt.modules.functional.ResultsTree
        :type results: bzt.modules.functional.ResultsTree
        """
        self.cumulative_results = cumulative_results

    def shutdown(self):
        self.end_time = time.time()

    def post_process(self):
        """
        Log basic stats
        """
        super(FinalStatus, self).post_process()

        if self.parameters.get("test-duration", True):
            self.__report_duration()

        if self.last_sec:
            if '' in self.last_sec[DataPoint.CUMULATIVE]:
                summary_kpi = self.last_sec[DataPoint.CUMULATIVE][""]

                if self.parameters.get("summary", True):
                    self.__report_samples_count(summary_kpi)
                if self.parameters.get("percentiles", True):
                    self.__report_percentiles(summary_kpi)

            if self.parameters.get("summary-labels", True):
                self.__report_summary_labels(self.last_sec[DataPoint.CUMULATIVE])

            if self.parameters.get("failed-labels"):
                self.__report_failed_labels(self.last_sec[DataPoint.CUMULATIVE])

            if self.parameters.get("dump-xml"):
                self.__dump_xml(self.parameters.get("dump-xml"))

            if self.parameters.get("dump-csv"):
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
                    msg = "Test {test_case} failed: {error_msg}".format(test_case=full_name, error_msg=case.error_msg)
                    if case.error_trace:
                        msg += "\n" + case.error_trace
                    self.log.warning(msg)

    def __report_summary(self):
        status_counter = Counter()
        for test_suite in self.cumulative_results.test_suites():
            for case in self.cumulative_results.test_cases(test_suite):
                status_counter[case.status] += 1

        # FIXME: it's actually not tests, but test cases
        total = sum(count for _, count in iteritems(status_counter))
        self.log.info("Total: %s %s", total, self.__plural(total, 'test'))

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

        data = [("Percentile, %", "Resp. Time, s")]
        for key in sorted(summary_kpi_set[KPISet.PERCENTILES].keys(), key=float):
            data.append((float(key), summary_kpi_set[KPISet.PERCENTILES][key]))
            # self.log.info("Percentile %.1f%%: %.3f", )
        table = SingleTable(data) if sys.stdout and sys.stdout.isatty() else AsciiTable(data)
        table.justify_columns[0] = 'right'
        table.justify_columns[1] = 'right'
        self.log.info("Percentiles:\n%s", table.table)

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

    def __get_sample_element(self, sample, label_name):
        failed_samples_count = sample['fail']
        success_samples_count = sample['succ']
        total_samples_count = failed_samples_count + success_samples_count
        assert total_samples_count > 0, "Total samples is zero for %s" % label_name
        success_samples_perc = (success_samples_count * 100) / total_samples_count

        errors = set()
        for err_desc in sample['errors']:
            errors.add(err_desc["msg"])

        return (
            label_name,
            "FAIL" if failed_samples_count > 0 else "OK",
            "{0:.2f}%".format(round(success_samples_perc, 2)),
            "{0:.3f}".format(round(sample['avg_rt'], 3)),
            "\n".join(errors)
        )

    def __report_summary_labels(self, cumulative):
        data = [("label", "status", "succ", "avg_rt", "error")]
        justify = {0: "left", 1: "center", 2: "right", 3: "right", 4: "left"}

        sorted_labels = sorted(cumulative.keys())
        for sample_label in sorted_labels:
            if sample_label != "":
                data.append(self.__get_sample_element(cumulative[sample_label], sample_label))
        table = SingleTable(data) if sys.stdout and sys.stdout.isatty() else AsciiTable(data)
        table.justify_columns = justify
        self.log.info("Request label stats:\n%s", table.table)

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

        if self.first_ts < float("inf") and self.last_ts > 0:
            duration_elem = etree.Element("TestDuration")
            duration_elem.text = str(round(float(self.last_ts - self.first_ts), 3))
            root.append(duration_elem)

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
            if kpi_name in (KPISet.ERRORS, KPISet.RESP_TIMES):
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
        elif isinstance(kpi_val, str):
            return kpi_val
        else:
            raise TaurusInternalException("Unhandled kpi type: %s" % type(kpi_val))

    def __dump_csv(self, filename):
        self.log.info("Dumping final status as CSV: %s", filename)
        # FIXME: what if there's no last_sec
        with open(get_full_path(filename), 'wt') as fhd:
            if '' in self.last_sec[DataPoint.CUMULATIVE]:
                fieldnames = self.__get_csv_dict('', self.last_sec[DataPoint.CUMULATIVE]['']).keys()
                writer = csv.DictWriter(fhd, fieldnames)
                writer.writeheader()
                for label, kpiset in iteritems(self.last_sec[DataPoint.CUMULATIVE]):
                    writer.writerow(self.__get_csv_dict(label, kpiset))

    def __get_csv_dict(self, label, kpiset):
        kpi_copy = copy.deepcopy(kpiset)
        res = OrderedDict()
        res['label'] = label

        # sort label
        for key in sorted(kpi_copy.keys()):
            res[key] = kpi_copy[key]

        del res[KPISet.ERRORS]
        del res[KPISet.RESP_TIMES]
        del res[KPISet.RESP_CODES]
        del res[KPISet.PERCENTILES]

        percentiles = list(iteritems(kpiset[KPISet.PERCENTILES]))
        for level, val in sorted(percentiles, key=lambda lv: (float(lv[0]), lv[1])):
            res['perc_%s' % level] = val

        resp_codes = list(iteritems(kpiset[KPISet.RESP_CODES]))
        for rcd, val in sorted(resp_codes):
            res['rc_%s' % rcd] = val

        for key in res:
            if isinstance(res[key], float):
                res[key] = "%.5f" % res[key]

        return res


class JUnitXMLReporter(Reporter, AggregatorListener, FunctionalAggregatorListener):
    """
    A reporter that exports results in Jenkins JUnit XML format.
    """

    def __init__(self):
        super(JUnitXMLReporter, self).__init__()
        self.last_second = None
        self.report_file_path = None
        self.cumulative_results = None

    def prepare(self):
        if isinstance(self.engine.aggregator, ResultsProvider):
            self.engine.aggregator.add_listener(self)
        elif self.engine.is_functional_mode():
            self.engine.aggregator.add_listener(self)

    def aggregated_second(self, data):
        self.last_second = data

    def aggregated_results(self, _, cumulative_results):
        """
        :type cumulative_results: bzt.modules.functional.ResultsTree
        """
        self.cumulative_results = cumulative_results

    def post_process(self):
        """
        Get report data, generate xml report.
        """
        filename = self.parameters.get("filename", None)
        if not filename:
            filename = self.engine.create_artifact(XUnitFileWriter.REPORT_FILE_NAME, XUnitFileWriter.REPORT_FILE_EXT)
        self.parameters["filename"] = filename  # reflect it in effective config

        if self.cumulative_results is None:
            test_data_source = self.parameters.get("data-source", "sample-labels")

            if test_data_source == "sample-labels":
                if not self.last_second:
                    self.log.warning("No last second data to generate XUnit.xml")
                else:
                    writer = XUnitFileWriter(self.engine)
                    self.process_sample_labels(writer)
                    writer.save_report(filename)
            elif test_data_source == "pass-fail":
                writer = XUnitFileWriter(self.engine)
                self.process_pass_fail(writer)
                writer.save_report(filename)
            else:
                raise TaurusConfigError("Unsupported data source: %s" % test_data_source)
        else:
            writer = XUnitFileWriter(self.engine)
            self.process_functional(writer)
            writer.save_report(filename)

        self.report_file_path = filename  # TODO: just for backward compatibility, remove later

    def process_sample_labels(self, xunit):
        """
        :type xunit: XUnitFileWriter
        """
        xunit.report_test_suite('sample_labels')
        labels = self.last_second[DataPoint.CUMULATIVE]

        for key in sorted(labels.keys()):
            if key == "":  # skip total label
                continue

            errors = []
            for er_dict in labels[key][KPISet.ERRORS]:
                rc = str(er_dict["rc"])
                msg = str(er_dict["msg"])
                cnt = str(er_dict["cnt"])
                if er_dict["type"] == KPISet.ERRTYPE_ASSERT:
                    err_element = etree.Element("failure", message=msg, type="Assertion Failure")
                else:
                    err_element = etree.Element("error", message=msg, type="Error")
                err_desc = "%s\n(status code is %s)\n(total errors of this type: %s)" % (msg, rc, cnt)
                err_element.text = err_desc
                errors.append(err_element)

            xunit.report_test_case('sample_labels', key, errors)

    def process_pass_fail(self, xunit):
        """
        :type xunit: XUnitFileWriter
        """
        xunit.report_test_suite('bzt_pass_fail')
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

            xunit.report_test_case('bzt_pass_fail', disp_name, errors)

    def process_functional(self, xunit):
        for suite_name, samples in iteritems(self.cumulative_results):
            duration = max(s.start_time for s in samples) - min(s.start_time for s in samples)
            duration += max(samples, key=lambda s: s.start_time).duration
            attrs = {
                "name": suite_name,
                "tests": str(len(samples)),
                "errors": str(len([sample for sample in samples if sample.status == "BROKEN"])),
                "skipped": str(len([sample for sample in samples if sample.status == "SKIPPED"])),
                "failures": str(len([sample for sample in samples if sample.status == "FAILED"])),
                "time": str(round(duration, 3)),
                # TODO: "timestamp" attribute
            }
            xunit.add_test_suite(suite_name, attributes=attrs)
            for sample in samples:
                attrs = {
                    "classname": sample.test_suite,
                    "name": sample.test_case,
                    "time": str(round(sample.duration, 3))
                }
                children = []
                if sample.status == "BROKEN":
                    error = etree.Element("error", type=sample.error_msg)
                    if sample.error_trace:
                        error.text = sample.error_trace
                    children.append(error)
                elif sample.status == "FAILED":
                    failure = etree.Element("failure", message=sample.error_msg)
                    if sample.error_trace:
                        failure.text = sample.error_trace
                    children.append(failure)
                elif sample.status == "SKIPPED":
                    skipped = etree.Element("skipped")
                    children.append(skipped)
                xunit.add_test_case(suite_name, attributes=attrs, children=children)


def get_bza_report_info(engine, log):
    """
    :return: [(url, test), (url, test), ...]
    """
    result = []
    if isinstance(engine.provisioning, CloudProvisioning):
        cloud_prov = engine.provisioning
        test_name = cloud_prov.settings.get("test")
        report_url = cloud_prov.results_url
        result.append((report_url, test_name if test_name else report_url))
    else:
        bza_reporters = [_x for _x in engine.reporters if isinstance(_x, BlazeMeterUploader)]
        for bza_reporter in bza_reporters:
            if bza_reporter.results_url:
                test_name = bza_reporter.parameters.get("test")
                report_url = bza_reporter.results_url
                result.append((report_url, test_name if test_name else report_url))

        if len(result) > 1:
            log.warning("More than one blazemeter reporter found")
    return result


class XUnitFileWriter(object):
    REPORT_FILE_NAME = "xunit"
    REPORT_FILE_EXT = ".xml"

    def __init__(self, engine):
        """
        :type engine: bzt.engine.Engine
        """
        super(XUnitFileWriter, self).__init__()
        self.engine = engine
        self.log = engine.log.getChild(self.__class__.__name__)
        self.test_suites = OrderedDict()
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

            testsuites = etree.Element("testsuites")
            for _, suite in iteritems(self.test_suites):
                testsuites.append(suite)
            etree_obj = etree.ElementTree(testsuites)

            self.log.info("Writing JUnit XML report into: %s", fname)
            with open(get_full_path(fname), 'wb') as _fds:
                etree_obj.write(_fds, xml_declaration=True, encoding="UTF-8", pretty_print=True)
        except BaseException:
            raise TaurusInternalException("Cannot create file %s" % fname)

    def report_test_suite(self, suite_name):
        """
        :type suite_name: str
        :type children: list[lxml.etree.Element]
        """
        self.add_test_suite(suite_name, attributes={"name": suite_name, "package_name": "bzt"})

    def report_test_case(self, suite_name, case_name, children=None):
        """
        :type suite_name: str
        :type case_name: str
        :type children: list[lxml.etree.Element]
        """
        children = children or []
        if self.report_urls:
            system_out = etree.Element("system-out")
            system_out.text = "".join(self.report_urls)
            children.insert(0, system_out)
        self.add_test_case(suite_name, attributes={"classname": self.class_name, "name": case_name}, children=children)

    def add_test_suite(self, suite_name, attributes=None, children=()):
        attributes = attributes or {}

        suite = etree.Element("testsuite", **attributes)

        for child in children:
            suite.append(child)

        if not suite_name in self.test_suites:
            self.test_suites[suite_name] = suite

    def add_test_case(self, suite_name, attributes=None, children=()):
        attributes = attributes or {}

        case = etree.Element("testcase", **attributes)

        for child in children:
            case.append(child)

        self.test_suites[suite_name].append(case)
