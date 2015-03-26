""" Basics of reporting capabilities """

from bzt.modules.aggregator import DataPoint, KPISet

from bzt.engine import Reporter, AggregatorListener
from lxml import etree
import os.path
from passfail import FailCriteria, PassFailStatus

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
    A reporter that exports results in JUnitXML format
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
                    self.log.warning("File %s already exists, will be rewrited." % filename)
                    os.remove(filename)
                with open(filename, 'w') as fds:
                    pass
            except BaseException as e:
                self.log.error("Cannot create file %s" % filename)
            self.report_file_path = filename

        else:
            self.report_file_path = self.engine.create_artifact(JUnitXMLReporter.REPORT_FILE_NAME,\
                                                                JUnitXMLReporter.REPORT_FILE_EXT)
        self.settings["filename"] = self.report_file_path

        # TODO: check if file is a directory, fail if true

    def aggregated_second(self, data):
        """
        :param data:
        :return:
        """
        self.last_second = data

    def post_process(self):
        """
        if pass-fail, then
        :return:
        """
        super(JUnitXMLReporter, self).post_process()

        #if "pass-fail" in self.engine.reporters:
        #    pass

        if self.settings.get("data_source", None)=="finalstats":
        #if data source = final stats"

            # take cumulative data
            _kpiset = self.last_second[DataPoint.CUMULATIVE]

            root_xml_element = None

            for key in sorted(_kpiset.keys()):
                if key == "":
                    #first element
                    #count all errors
                    summary_report_template = "Success: {success}, Sample count: {throughput}, Failures: {fail}, Errors: {errors}\n"
                    err_template = "Error code: {rc}, Message: {msg}, count: {cnt}\n"
                    url_err_template = "URL: {url}, Error count {cnt}\n"

                    #'errors': [{'msg': 'Forbidden', 'cnt': 7373, 'type': 0,
                    #'urls': Counter({'http://192.168.25.8/': 7373}), 'rc': '403'}]

                    error_report = ""

                    err_counter = 0 #used in summary report

                    for error in _kpiset[key][KPISet.ERRORS]:
                        #error: {'msg': 'Forbidden', 'cnt': 7373, 'type': 0, 'urls': Counter({'http://192.168.25.8/': 7373}), 'rc': '403'}
                        error_report += err_template.format(rc=error['rc'], msg=error['msg'], cnt=error['cnt'])
                        urls_err_string = ""

                        #enumerate urls and error count
                        for _url, _err_count in error["urls"].items():
                            err_counter += _err_count
                            urls_err_string += url_err_template.format(url= _url, cnt=str(_err_count))
                            error_report += urls_err_string


                    succ = str(_kpiset[key][KPISet.SUCCESSES])
                    throughput = str(_kpiset[key][KPISet.SAMPLE_COUNT])
                    fail = str(_kpiset[key][KPISet.FAILURES])
                    errors = str(err_counter)

                    summary_report = summary_report_template.format(success=succ, throughput=throughput,fail=fail, errors=errors )
                    summary_report += error_report

                    root_xml_element = etree.Element("testsuite", name="taurus junitxml",tests=throughput, failures=fail, skip="0")

                    summary_test_case = etree.SubElement(root_xml_element, "testcase", class_name="summary", name="summary_report")
                    error_xml_sub = etree.SubElement(summary_test_case, "error", type="http error", message="error statistics:").text=summary_report

                else:
                    #take url, split it on domain and other pieces

                    parsed_url = urlparse.urlparse(key)

                    class_name = parsed_url.scheme + "." + parsed_url.netloc.replace(".","_")
                    resource_name = ".".join([parsed_url.path.replace(".","_"),
                                              parsed_url.params.replace(".","_"),
                                              parsed_url.query.replace(".","_"),
                                              parsed_url.fragment.replace(".","_")])

                    test_case = etree.SubElement(root_xml_element, "testcase", classname=class_name, name=resource_name, time="0")


                    if _kpiset[key]["errors"]:

                        for er_dict in _kpiset[key]["errors"]:
                            err_message = str(er_dict["rc"])
                            err_type = str(er_dict["msg"])
                            err_desc = "total errors of this type:" + str(er_dict["cnt"])
                            #add counter data

                            errors = etree.SubElement(test_case, "error", type=err_type, message=err_message).text = err_desc







            self.log.info("writing xml...")
            with open(self.report_file_path, 'a') as fds:
                #fds.write("""<?xml version="1.0" encoding="UTF-8"?>""")
                fds.write(etree.tostring(root_xml_element, xml_declaration=True, pretty_print=True, encoding="utf-8"))

        else:
        #data source = passfail
            pass_fail_objects = filter(lambda x: isinstance(x, PassFailStatus), self.engine.reporters)
            fail_criterias = []
            for pf_obj in pass_fail_objects:
                if pf_obj.criterias:
                    for fc in pf_obj.criterias:
                        fail_criterias.append(fc)

            total_failed = str(len(filter(lambda x: x.is_triggered, fail_criterias)))
            total_len = str(len(fail_criterias))
            root_xml_element = etree.Element("testsuite", name="taurus junitxml",tests=total_len, failures=total_failed, skip="0")
            for fc_obj in fail_criterias:
                classname = str(fc_obj)
                fc_xml_element = etree.SubElement(root_xml_element, "testcase", classname=classname, name="")
                if fc_obj.is_triggered:
                    err_element=etree.SubElement(fc_xml_element, "error", type="criteria failed", message="")#.text=..


            self.log.info("writing xml...")
            with open(self.report_file_path, 'a') as fds:
                #fds.write("""<?xml version="1.0" encoding="UTF-8"?>""")
                fds.write(etree.tostring(root_xml_element, xml_declaration=True, pretty_print=True, encoding="utf-8"))
        self.log.debug("post_process done")


    def shutdown(self):
        self.log.debug("Shutdown stage")