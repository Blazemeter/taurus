""" Basics of reporting capabilities """

from bzt.modules.aggregator import DataPoint, KPISet

from bzt.engine import Reporter, AggregatorListener
from lxml import etree
import os.path
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

    REPORT_FILE_NAME = "junitxml_report"
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
            try:
                if os.path.exists(filename):
                    self.log.warning("File %s already exists, will be overwritten." % filename)
                    os.remove(filename)
                with open(filename, 'w') as _fds:
                    pass
            except BaseException as exc_obj:
                self.log.error("Cannot create file %s" % filename)
                raise exc_obj

            self.report_file_path = filename

        else:
            self.report_file_path = self.engine.create_artifact(JUnitXMLReporter.REPORT_FILE_NAME,\
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
        Get summary data, generate xml report.
        """
        super(JUnitXMLReporter, self).post_process()

        test_data_source = self.settings.get("data-source", "finalstats")
        root_xml_element = None

        if test_data_source=="finalstats":

            # _kpiset - cumulative test data, type: KPISet
            _kpiset = self.last_second[DataPoint.CUMULATIVE]



            # enumerate all urls, blank url is a summary data
            for key in sorted(_kpiset.keys()):
                if key == "":
                    summary_report_template = "Success: {success},\
                     Sample count: {throughput}, Failures: {fail}, Errors: {errors}\n"
                    err_template = "Error code: {rc}, Message: {msg}, count: {cnt}\n"
                    url_err_template = "URL: {url}, Error count {cnt}\n"
                    error_report = ""  # errors with descriptions and urls (summary report)
                    err_counter = 0  # used in summary report (summary report)

                    for error in _kpiset[key][KPISet.ERRORS]:
                        error_report += err_template.format(rc=error['rc'], msg=error['msg'], cnt=error['cnt'])
                        urls_err_string = ""

                        # enumerate urls and error count (from Counter object)
                        for _url, _err_count in error["urls"].items():
                            err_counter += _err_count
                            urls_err_string += url_err_template.format(url=_url, cnt=str(_err_count))
                            error_report += urls_err_string

                    succ = str(_kpiset[key][KPISet.SUCCESSES])
                    throughput = str(_kpiset[key][KPISet.SAMPLE_COUNT])
                    fail = str(_kpiset[key][KPISet.FAILURES])
                    errors = str(err_counter)

                    summary_report = summary_report_template.format(success=succ, throughput=throughput, fail=fail,
                                                                    errors=errors)
                    summary_report += error_report

                    # generate xml root element <testsuite>
                    root_xml_element = etree.Element("testsuite", name="taurus_junitxml_finalstats", tests=throughput,
                                                     failures=fail, skip="0")
                    summary_test_case = etree.SubElement(root_xml_element, "testcase", class_name="summary",
                                                         name="summary_report")
                    error_xml_sub = etree.SubElement(summary_test_case, "error", type="http error",
                                                     message="error statistics:").text = summary_report

                else:  # if url is not blank
                    # split url on domain resource, protocol, etc
                    parsed_url = urlparse.urlparse(key)
                    # remove dots from url and join all pieces on dot
                    class_name = parsed_url.scheme + "." + parsed_url.netloc.replace(".", "_")
                    resource_name = ".".join([parsed_url.path.replace(".", "_"),
                                              parsed_url.params.replace(".", "_"),
                                              parsed_url.query.replace(".", "_"),
                                              parsed_url.fragment.replace(".", "_")])

                    # generate <testcase> subelement
                    test_case = etree.SubElement(root_xml_element, "testcase", classname=class_name,
                                                 name=resource_name, time="0")

                    # if any errors in url report, generate <error> subelement with error description
                    if _kpiset[key]["errors"]:

                        for er_dict in _kpiset[key]["errors"]:
                            err_message = str(er_dict["rc"])
                            err_type = str(er_dict["msg"])
                            err_desc = "total errors of this type:" + str(er_dict["cnt"])

                            errors = etree.SubElement(test_case, "error", type=err_type,
                                                      message=err_message).text = err_desc

        # data-source pass-fail
        elif test_data_source == "pass-fail":

            pass_fail_objects = filter(lambda x: isinstance(x, PassFailStatus), self.engine.reporters)
            fail_criteries = []

            for pf_obj in pass_fail_objects:
                if pf_obj.criterias:
                    for fc in pf_obj.criterias:
                        fail_criteries.append(fc)

            # count total failed tests, tests, create root <testsuite>
            total_failed = str(len(filter(lambda x: x.is_triggered, fail_criteries)))
            tests_count = str(len(fail_criteries))
            root_xml_element = etree.Element("testsuite", name="taurus_junitxml_pass_fail", tests=tests_count,
                                             failures=total_failed, skip="0")

            for fc_obj in fail_criteries:
                classname = str(fc_obj)
                fc_xml_element = etree.SubElement(root_xml_element, "testcase", classname=classname, name="")

                if fc_obj.is_triggered:
                    # NOTE: we can add error description im err_element.text()
                    err_element=etree.SubElement(fc_xml_element, "error", type="criteria failed", message="")

        # unknown data-source
        else:
            self.log.error("Unknown data-source: %s", test_data_source)

        if root_xml_element != None:
            self.log.info("Writing report to %s...", self.report_file_path)
            with open(self.report_file_path, 'a') as fds:
                fds.write(etree.tostring(root_xml_element, xml_declaration=True, pretty_print=True, encoding="utf-8"))

    def shutdown(self):
        super(JUnitXMLReporter, self).shutdown()