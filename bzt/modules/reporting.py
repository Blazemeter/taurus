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

import os

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.engine import Reporter, AggregatorListener
from bzt.modules.passfail import PassFailStatus


try:
    from urlparse import urlparse
except ImportError:
    from urllib.parse import urlparse

try:
    from lxml import etree
except ImportError:
    try:
        import cElementTree as etree
    except ImportError:
        import elementtree.ElementTree as etree


class FinalStatus(Reporter, AggregatorListener):
    """
    A reporter that prints short statistics on test end
    """

    def __init__(self):
        super(FinalStatus, self).__init__()
        self.last_sec = None

    def aggregated_second(self, data):
        """
        Just store the latest info

        :type data: bzt.modules.aggregator.DataPoint
        """
        self.last_sec = data

    def post_process(self):
        """
        Log basic stats
        """
        super(FinalStatus, self).post_process()

        if self.last_sec:
            summary_kpi = self.last_sec[DataPoint.CUMULATIVE][""]

            if self.parameters.get("summary", True):
                self.__report_samples_count(summary_kpi)
            if self.parameters.get("percentiles", True):
                self.__report_percentiles(summary_kpi)

            if self.parameters.get("failed-labels", False):
                self.__report_failed_labels(self.last_sec[DataPoint.CUMULATIVE])

    def __report_samples_count(self, summary_kpi_set):
        """
        reports samples count
        """
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


class JUnitXMLReporter(Reporter, AggregatorListener):
    """
    A reporter that exports results in Jenkins JUnit XML format.
    """

    REPORT_FILE_NAME = "xunit"
    REPORT_FILE_EXT = ".xml"

    def __init__(self):
        super(JUnitXMLReporter, self).__init__()
        self.report_file_path = "junit.xml"
        self.last_second = None

    def prepare(self):
        """
        create artifacts, parse options.
        report filename from parameters
        :return:
        """
        filename = self.parameters.get("filename", None)
        if filename:
            self.report_file_path = filename
        else:
            self.report_file_path = self.engine.create_artifact(JUnitXMLReporter.REPORT_FILE_NAME,
                                                                JUnitXMLReporter.REPORT_FILE_EXT)
        self.parameters["filename"] = self.report_file_path

    def aggregated_second(self, data):
        """
        :param data:
        :return:
        """
        self.last_second = data

    def post_process(self):
        """
        Get report data, generate xml report.
        """
        super(JUnitXMLReporter, self).post_process()
        test_data_source = self.parameters.get("data-source", "sample-labels")

        # data-source sample-labels
        if test_data_source == "sample-labels":
            root_xml_element = self.__process_sample_labels()
            self.__save_report(root_xml_element)
        # data-source pass-fail
        elif test_data_source == "pass-fail":
            root_xml_element = self.__process_pass_fail()
            self.__save_report(root_xml_element)

    def __convert_label_name(self, url):
        """
        http://some.address/path/resource?query -> http.some_address.path.resource.query
        :param url:
        :return: string
        """

        # split url on domain resource, protocol, etc
        parsed_url = urlparse(url)
        # remove dots from url and join all pieces on dot
        # small fix needed - better do not use blank pieces
        class_name = parsed_url.scheme + "." + parsed_url.netloc.replace(".", "_")
        resource_name = ".".join([parsed_url.path.replace(".", "_"),
                                  parsed_url.params.replace(".", "_"),
                                  parsed_url.query.replace(".", "_"),
                                  parsed_url.fragment.replace(".", "_")])
        return class_name, resource_name

    def __save_report(self, root_node):
        """
        :param root_node:
        :return:
        """
        try:
            if os.path.exists(self.report_file_path):
                self.log.warning("File %s already exists, will be overwritten", self.report_file_path)
            else:
                dirname = os.path.dirname(self.report_file_path)
                if dirname and not os.path.exists(dirname):
                    os.makedirs(dirname)

            etree_obj = etree.ElementTree(root_node)
            self.log.info("Writing JUnit XML report into: %s", self.report_file_path)
            with open(self.report_file_path, 'wb') as _fds:
                etree_obj.write(_fds, xml_declaration=True, encoding="UTF-8", pretty_print=True)

        except BaseException as exc_obj:
            self.log.error("Cannot create file %s", self.report_file_path)
            raise

    def __make_summary_error_report(self, summary_kpi_set):
        """
        Makes summary error report
        :return: str
        """
        err_template = "Error code: {rc}, Message: {msg}, count: {cnt}\n"
        url_err_template = "URL: {url}, Error count {cnt}\n"
        error_report = ""  # errors with descriptions and urls (summary report)
        err_counter = 0  # used in summary report (summary report)

        for error in summary_kpi_set[KPISet.ERRORS]:
            error_report += err_template.format(rc=error['rc'], msg=error['msg'], cnt=error['cnt'])
            urls_err_string = ""
            # enumerate urls and count errors (from Counter object)
            for _url, _err_count in error["urls"].items():
                err_counter += _err_count
                urls_err_string += url_err_template.format(url=_url, cnt=str(_err_count))
                error_report += urls_err_string

        return str(err_counter), error_report

    def __make_xml_header(self, summary_kpi_set):
        """
        get summary KPI, generate root_xml_element and summary report for erros.
        used in __process_sample_labels
        :return:
        etree xml root node
        """
        summary_report_template = "Success: {success}, Sample count: {throughput}, " \
                                  "Failures: {fail}, Errors: {errors}\n"
        succ = str(summary_kpi_set[KPISet.SUCCESSES])
        throughput = str(summary_kpi_set[KPISet.SAMPLE_COUNT])
        fail = str(summary_kpi_set[KPISet.FAILURES])
        errors, error_report = self.__make_summary_error_report(summary_kpi_set)
        summary_report = summary_report_template.format(success=succ, throughput=throughput, fail=fail,
                                                        errors=errors)
        summary_report += error_report

        # generate xml root element <testsuite>
        root_xml_element = etree.Element("testsuite", name="taurus_sample-labels", tests=throughput,
                                         failures=fail, skip="0")
        summary_test_case = etree.SubElement(root_xml_element, "testcase", class_name="summary",
                                             name="summary_report")
        etree.SubElement(summary_test_case, "error", type="http error",
                         message="error statistics:").text = summary_report

        return root_xml_element

    def __process_sample_labels(self):

        root_xml_element = None
        # _kpiset - cumulative test data, type: KPISet
        _kpiset = self.last_second[DataPoint.CUMULATIVE]
        # enumerate all sample-labels, blank url is a summary data
        for key in sorted(_kpiset.keys()):
            if key == "":
                summary_kpiset = _kpiset[key]
                root_xml_element = self.__make_xml_header(summary_kpiset)
            else:  # if label is not blank
                class_name, resource_name = self.__convert_label_name(key)
                # generate <testcase> subelement
                test_case = etree.SubElement(root_xml_element, "testcase", classname=class_name,
                                             name=resource_name, time="0")
                # if any errors in label report, generate <error> subelement with error description
                if _kpiset[key][KPISet.ERRORS]:
                    for er_dict in _kpiset[key][KPISet.ERRORS]:
                        err_message = str(er_dict["rc"])
                        err_type = str(er_dict["msg"])
                        err_desc = "total errors of this type:" + str(er_dict["cnt"])
                        etree.SubElement(test_case, "error", type=err_type,
                                         message=err_message).text = err_desc
        return root_xml_element

    def __process_pass_fail(self):
        """

        :return: etree xml root element
        """
        pass_fail_objects = [_x for _x in self.engine.reporters if isinstance(_x, PassFailStatus)]
        fail_criterias = []
        for pf_obj in pass_fail_objects:
            if pf_obj.criterias:
                for _fc in pf_obj.criterias:
                    fail_criterias.append(_fc)
        # count total failed tests, tests, create root <testsuite>
        failures = [x for x in fail_criterias if x.is_triggered and x.fail]
        total_failed = str(len(failures))
        tests_count = str(len(fail_criterias))
        root_xml_element = etree.Element("testsuite", name="taurus_junitxml_pass_fail", tests=tests_count,
                                         failures=total_failed, skip="0")
        for fc_obj in fail_criterias:
            if fc_obj.config['label']:
                data = (fc_obj.config['subject'], fc_obj.config['label'],
                        fc_obj.config['condition'], fc_obj.config['threshold'])
                tpl = "%s of %s%s%s"
            else:
                data = (fc_obj.config['subject'], fc_obj.config['condition'], fc_obj.config['threshold'])
                tpl = "%s%s%s"

            if fc_obj.config['timeframe']:
                tpl += " for %s"
                data += (fc_obj.config['timeframe'],)
            classname = tpl % data

            fc_xml_element = etree.SubElement(root_xml_element, "testcase", classname=classname, name="")
            if fc_obj.is_triggered and fc_obj.fail:
                # NOTE: we can add error description im err_element.text()
                etree.SubElement(fc_xml_element, "error", type="pass/fail criteria triggered", message="")

        # FIXME: minor fix criteria representation in report
        return root_xml_element
