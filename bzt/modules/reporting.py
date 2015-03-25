""" Basics of reporting capabilities """

from bzt.modules.aggregator import DataPoint, KPISet

from bzt.engine import Reporter, AggregatorListener
from lxml import etree

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

        filename = self.settings.get("file-name", JUnitXMLReporter.REPORT_FILE_NAME + JUnitXMLReporter.REPORT_FILE_EXT)
        self.report_file_path = self.engine.create_artifact(filename, "")

        header = """<?xml version="1.0" encoding="UTF-8"?><testsuitename="junitxml" tests="10: errors="1" failures="1" skip="0">
        <testcase classname="blah" name="blah" time="3"></testcase>"""


        self.log.debug("Prepare finished")

    def aggregated_second(self, data):
        """

        if fail-criteria in self.engine.modules

        :param data:
        :return:
        """

        self.last_second = data

        #with open(self.report_file_path, 'a') as fds:
        #    fds.write(str(data))
        #    fds.write("\n\n\n\n")

#        self.log.debug(data)
#        self.log.debug(self.settings)


    def post_process(self):
        """
        if pass-fail, then
        :return:
        """
        super(JUnitXMLReporter, self).post_process()


        result_xml=""

        if "pass-fail" in self.engine.modules:
            pass
        root = None

        for k in sorted(self.last_second[DataPoint.CUMULATIVE].keys()):
            if k == "":
                #first element, we need header
                #count all errors
                errors = 0
                #for er_dict in self.last_second[k]["errors"]:
                #    errors += er_dict["cnt"]

                errors = str(errors)
                tests = str(self.last_second[DataPoint.CUMULATIVE][k]['throughput'])
                failures = str(self.last_second[DataPoint.CUMULATIVE][k]['fail'])
                root = etree.Element("testsuite", tests=tests, errors=errors, failures=failures, skip="0")

            else:
                class_name = k

                test_case = etree.SubElement(root, "testcase", classname=class_name, name=class_name, time="0")
                root.append(etree.Element("testcase", classname=class_name))


                if self.last_second[DataPoint.CUMULATIVE][k]["errors"]:

                    for er_dict in self.last_second[DataPoint.CUMULATIVE][k]["errors"]:
                        err_message = str(er_dict["rc"])
                        err_type = str(er_dict["msg"])
                        err_desc = "total errors of this type:" + str(er_dict["cnt"])
                        #add counter data

                        errors = etree.SubElement(test_case, "error", type=err_type, message=err_message).text = err_desc





        #self.log.debug(etree.tostring(root, pretty_print=True))
        with open(self.report_file_path, 'a') as fds:
            fds.write("""<?xml version="1.0" encoding="UTF-8"?>""")
            fds.write(etree.tostring(root, pretty_print=True))

        self.log.debug("post_process done")


    def shutdown(self):
        self.log.debug("Shutdown stage")