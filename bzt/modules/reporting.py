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
import time
from datetime import datetime

from bzt.modules.aggregator import DataPoint, KPISet
from bzt.engine import Reporter, AggregatorListener
from bzt.modules.passfail import PassFailStatus
from bzt.modules.blazemeter import BlazeMeterUploader
from bzt.six import iteritems
from tempfile import NamedTemporaryFile


class FinalStatus(Reporter, AggregatorListener):
    """
    A reporter that prints short statistics on test end
    """

    def __init__(self):
        super(FinalStatus, self).__init__()
        self.last_sec = None
        self.start_time = time.time()
        self.end_time = None

    def prepare(self):
        self.start_time = time.time()

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

    def __report_duration(self):
        """
        asks executors start_time and end_time, provides time delta
        """

        date_start = datetime.fromtimestamp(int(self.start_time))
        date_end = datetime.fromtimestamp(int(self.end_time))
        self.log.info("Test duration: %s", date_end - date_start)


class JUnitXMLReporter(Reporter, AggregatorListener):
    """
    A reporter that exports results in Jenkins JUnit XML format.
    """

    REPORT_FILE_NAME = "xunit"
    REPORT_FILE_EXT = ".xml"

    def __init__(self):
        super(JUnitXMLReporter, self).__init__()
        self.report_file_path = "xunit.xml"
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

        if self.last_second:
            if test_data_source == "pass-fail":
                tmp_file_name = self.process_pass_fail()
            else:
                tmp_file_name = self.process_sample_labels()
            self.save_report(tmp_file_name, self.report_file_path)

    def process_sample_labels(self):
        """
        :return: NamedTemporaryFile.name
        """
        with NamedTemporaryFile(suffix=".xml", delete=False, dir=self.engine.artifacts_dir, mode='wt') as tmp_xml_file:
            _kpiset = self.last_second[DataPoint.CUMULATIVE]
            summary_kpiset = _kpiset[""]
            xml_writer = JUnitXMLWriter(tmp_xml_file)
            report_url, test_name = self.get_bza_report_info()
            class_name = "bzt" if not test_name else test_name

            self.write_summary_report(xml_writer, summary_kpiset, report_url)
            for key in sorted(_kpiset.keys()):
                if key != "":
                    xml_writer.add_testcase(close=False, classname=class_name, name=key)
                    if _kpiset[key][KPISet.ERRORS]:
                        for er_dict in _kpiset[key][KPISet.ERRORS]:
                            err_message = str(er_dict["rc"])
                            err_type = str(er_dict["msg"])
                            err_desc = "total errors of this type:" + str(er_dict["cnt"])
                            xml_writer.add_error(close=False, message=err_message, type=err_type)
                            xml_writer.raw_write(err_desc)
                            xml_writer.close_element()
                    xml_writer.close_element()
            xml_writer.close_element()
        return tmp_xml_file.name

    def write_summary_report(self, xml_writer, summary_kpiset, report_url):
        """
        :return:
        """
        succ = str(summary_kpiset[KPISet.SUCCESSES])
        throughput = str(summary_kpiset[KPISet.SAMPLE_COUNT])
        fail = str(summary_kpiset[KPISet.FAILURES])

        xml_writer.add_testsuite(close=False, name='sample_labels', package="bzt")
        xml_writer.add_testcase(close=False, classname="bzt", name="summary_report")
        xml_writer.add_skipped()
        xml_writer.add_system_out(close=False)
        errors_count = str(self.count_errors(summary_kpiset))
        summary_report_template = "Success: {success}, Sample count: {throughput}, " \
                                  "Failures: {fail}, Errors: {errors}\n"
        summary_report = summary_report_template.format(success=succ, throughput=throughput, fail=fail,
                                                        errors=errors_count)
        xml_writer.raw_write(summary_report)
        if report_url:
            xml_writer.raw_write(report_url)

        self.write_errors(xml_writer, summary_kpiset)
        xml_writer.close_element(2)

    def count_errors(self, summary_kpi_set):
        """
        Returns overall errors count
        :return:
        """
        err_counter = 0  # used in summary report
        for error in summary_kpi_set[KPISet.ERRORS]:
            for _url, _err_count in iteritems(error["urls"]):
                err_counter += _err_count
        return err_counter

    def get_bza_report_info(self):
        """
        :return: url, test
        """
        report_url = ""
        test_name = ""
        bza_reporters = [_x for _x in self.engine.reporters if isinstance(_x, BlazeMeterUploader)]
        if bza_reporters:
            bza_reporter = bza_reporters[-1]
            if bza_reporter.client.results_url:
                report_url = "bza report link:%s\n" % bza_reporter.client.results_url
            if bza_reporter.client.test_id:
                test_name = bza_reporter.parameters.get("test")
        return report_url, test_name

    def write_errors(self, xml_writer, summary_kpi_set):
        """
        Writes error descriptions in summary_report
        :return:
        """
        err_template = "Error code: {rc}, Message: {msg}, count: {cnt}\n"
        url_err_template = "URL: {url}, Error count {cnt}\n"
        for error in summary_kpi_set[KPISet.ERRORS]:
            xml_writer.raw_write(err_template.format(rc=error['rc'], msg=error['msg'], cnt=error['cnt']))
            for _url, _err_count in iteritems(error["urls"]):
                xml_writer.raw_write(url_err_template.format(url=_url, cnt=str(_err_count)))

    def save_report(self, tmp_name, orig_name):
        """
        :param tmp_name:
        :param orig_name:
        :return:
        """
        try:
            os.rename(tmp_name, orig_name)
            self.log.info("Writing JUnit XML report into: %s", orig_name)
        except BaseException:
            self.log.error("Cannot create file %s", orig_name)
            raise

    def process_pass_fail(self):
        """
        :return: NamedTemporaryFile.name
        """
        conditions = {">": "&gt;", ">=": "&gt;=", "<": "&lt;", "<=": "&lt;=", "=": "=", "==": "="}

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
        with NamedTemporaryFile(suffix=".xml", delete=False, dir=self.engine.artifacts_dir, mode='wt') as tmp_xml_file:
            xml_writer = JUnitXMLWriter(tmp_xml_file)
            xml_writer.add_testsuite(close=False, name='junitxml_pass_fail', package="bzt", tests=tests_count,
                                     failures=total_failed, skip="0")
            report_url, test_name = self.get_bza_report_info()
            classname = "bzt"
            if test_name:
                classname = test_name
            for fc_obj in fail_criterias:
                if fc_obj.config['label']:
                    data = (fc_obj.config['subject'], fc_obj.config['label'],
                            conditions.get(fc_obj.config['condition']), fc_obj.config['threshold'])
                    tpl = "%s of %s%s%s"
                else:
                    data = (
                    fc_obj.config['subject'], conditions.get(fc_obj.config['condition']), fc_obj.config['threshold'])
                    tpl = "%s%s%s"

                if fc_obj.config['timeframe']:
                    tpl += " for %s"
                    data += (fc_obj.config['timeframe'],)

                disp_name = tpl % data

                xml_writer.add_testcase(close=False, classname=classname, name=disp_name)
                if fc_obj.is_triggered and fc_obj.fail:
                    xml_writer.add_error(close=True, type="pass/fail criteria triggered")
                xml_writer.close_element()
            xml_writer.close_element()
        return tmp_xml_file.name


class JUnitXMLWriter(object):
    """
    Writes report in JUnitXML format in file
    """

    def __init__(self, fds):
        self.fds = fds
        self.endings = []
        self.write_header()

    def add_element(self, element_name, text="", close=True, **kwargs):
        """
        adds element
        :param element_name:
        :param kwargs:
        :return:
        """
        self.fds.write("<%s" % element_name)
        for k, v in kwargs.items():
            self.fds.write(" %s='%s'" % (k, v))
        self.fds.write(">\n")
        if text:
            self.fds.write(text)
            self.fds.write("\n")
        if close:
            self.fds.write("</%s>\n" % element_name)
        else:
            self.endings.append("</%s>\n" % element_name)

    def add_testsuite(self, close=True, **kwargs):
        self.add_element("testsuite", close=close, **kwargs)

    def add_testcase(self, close=True, **kwargs):
        self.add_element("testcase", close=close, **kwargs)

    def add_error(self, close=True, **kwargs):
        self.add_element("error", close=close, **kwargs)

    def add_system_out(self, close=True, **kwargs):
        self.add_element("system-out", close=close, **kwargs)

    def add_skipped(self, close=True, **kwargs):
        self.add_element("skipped", close=close, **kwargs)

    def close_element(self, count=1):
        if self.endings:
            while count > 0:
                self.fds.write(self.endings.pop())
                count -= 1

    def write_header(self):
        self.fds.write("<?xml version='1.0' encoding='UTF-8'?>\n")

    def raw_write(self, data):
        self.fds.write(data)
