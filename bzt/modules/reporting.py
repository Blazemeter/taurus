""" Basics of reporting capabilities """

from bzt.modules.aggregator import DataPoint, KPISet

from bzt.engine import Reporter, AggregatorListener
from lxml import etree
import os
from passfail import PassFailStatus

import urlparse


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
            cumul = self.last_sec[DataPoint.CUMULATIVE]
            overall = cumul['']
            err_rate = 100 * overall[KPISet.FAILURES] / float(overall[KPISet.SAMPLE_COUNT])
            self.log.info("Samples count: %s, %.2f%% failures", overall[KPISet.SAMPLE_COUNT], err_rate)
            fmt = "Average times: total %.3f, latency %.3f, connect %.3f"
            self.log.info(fmt, overall[KPISet.AVG_RESP_TIME], overall[KPISet.AVG_LATENCY],
                          overall[KPISet.AVG_CONN_TIME])
            for key in sorted(overall[KPISet.PERCENTILES].keys(), key=float):
                self.log.info("Percentile %.1f%%: %.3f", float(key), overall[KPISet.PERCENTILES][key])

                # todo: add optional errors summary


class JUnitXMLReporter(Reporter, AggregatorListener):
    """
    A reporter that exports results in Jenkins JUnit XML format.
    """

    REPORT_FILE_NAME = "xunit"
    REPORT_FILE_EXT = ".xml"

    def __init__(self):
        super(JUnitXMLReporter, self).__init__()
        self.report_file_path = None
        self.last_second = None

    def prepare(self):
        """
        create artifacts, parse options.
        report filename from settings
        :return:
        """
        filename = self.settings.get("filename", None)
        if filename:
            self.report_file_path = filename
        else:
            self.report_file_path = self.engine.create_artifact(JUnitXMLReporter.REPORT_FILE_NAME,
                                                                JUnitXMLReporter.REPORT_FILE_EXT)
            self.settings["filename"] = self.report_file_path

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
        test_data_source = self.settings.get("data-source", "sample-labels")

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
        parsed_url = urlparse.urlparse(url)
        # remove dots from url and join all pieces on dot
        # small fix needed - better do not use blank pieces
        class_name = parsed_url.scheme + "." + parsed_url.netloc.replace(".", "_")
        resource_name = ".".join([parsed_url.path.replace(".", "_"),
                                  parsed_url.params.replace(".", "_"),
                                  parsed_url.query.replace(".", "_"),
                                  parsed_url.fragment.replace(".", "_")])
        return class_name, resource_name

    def __save_report(self, etree_obj):
        """
        :param etree_obj:
        :return:
        """
        try:
            if os.path.exists(self.report_file_path):
                self.log.warning("File %s already exists, will be overwritten" % self.report_file_path)
            else:
                dirname = os.path.dirname(self.report_file_path)
                if not os.path.exists(dirname):
                    os.makedirs(dirname)
            with open(self.report_file_path, 'w') as _fds:
                _fds.write(etree.tostring(etree_obj, xml_declaration=True, pretty_print=True, encoding="utf-8"))
        except BaseException as exc_obj:
            self.log.error("Cannot create file %s" % self.report_file_path)
            raise exc_obj

    def __make_summary_error_report(self, summary_kpi_set):
        """
        Makes summary error report J
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
                summary_kpi_set = _kpiset[key]
                root_xml_element = self.__make_xml_header(summary_kpi_set)
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
        pass_fail_objects = filter(lambda x: isinstance(x, PassFailStatus), self.engine.reporters)
        fail_criterias = []
        for pf_obj in pass_fail_objects:
            if pf_obj.criterias:
                for fc in pf_obj.criterias:
                    fail_criterias.append(fc)
        # count total failed tests, tests, create root <testsuite>
        total_failed = str(len(filter(lambda x: x.is_triggered, fail_criterias)))
        tests_count = str(len(fail_criterias))
        root_xml_element = etree.Element("testsuite", name="taurus_junitxml_pass_fail", tests=tests_count,
                                         failures=total_failed, skip="0")
        for fc_obj in fail_criterias:
            classname = str(fc_obj)
            fc_xml_element = etree.SubElement(root_xml_element, "testcase", classname=classname, name="")
            if fc_obj.is_triggered:
                # NOTE: we can add error description im err_element.text()
                etree.SubElement(fc_xml_element, "error", type="criteria failed", message="")

        # FIXME: minor fix criteria representation in report
        return root_xml_element