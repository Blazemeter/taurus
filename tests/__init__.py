""" unit test """
import difflib
import inspect
import json
import logging
import os
import tempfile
from io import StringIO
from logging import Handler
from random import random
from unittest.case import TestCase

from bzt.cli import CLI
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.six import u
from bzt.utils import run_once, EXE_SUFFIX

TestCase.shortDescription = lambda self: None  # suppress nose habit to show docstring instead of method name


@run_once
def setup_test_logging():
    """ set up test logging for convenience in IDE """
    root = logging.getLogger('')
    if not root.handlers:
        CLI.log = None
        CLI.verbose = True
        CLI.setup_logging(CLI)
    else:
        root.debug("Already set up logging")


setup_test_logging()
logging.info("Bootstrapped test")


def __dir__():
    filename = inspect.getouterframes(inspect.currentframe())[1][1]
    return os.path.dirname(filename)


def r(mul=5):
    return 1 + int(mul * random()) / 1000.0


def rc():
    return "%s00" % (int(4 * random()) + 1)


def err():
    if int(50 * random()) == 0:
        return "Some Error"
    else:
        return None


def random_sample(ts, label='', conc=1):
    return ts, label, conc, r(), r(), r(), rc(), err()


def random_datapoint(n):
    point = DataPoint(n)
    overall = point[DataPoint.CURRENT].get('', KPISet())
    overall[KPISet.CONCURRENCY] = r(100)
    overall[KPISet.SAMPLE_COUNT] = int(100 * r(1000)) + 1
    overall[KPISet.SUCCESSES] = int(overall[KPISet.SAMPLE_COUNT] * random())
    overall[KPISet.FAILURES] = overall[KPISet.SAMPLE_COUNT] - overall[KPISet.SUCCESSES]
    overall[KPISet.PERCENTILES]['25.0'] = r(10)
    overall[KPISet.PERCENTILES]['50.0'] = r(20)
    overall[KPISet.PERCENTILES]['75.0'] = r(30)
    overall[KPISet.PERCENTILES]['90.0'] = r(40)
    overall[KPISet.PERCENTILES]['99.0'] = r(50)
    overall[KPISet.PERCENTILES]['100.0'] = r(100)
    overall[KPISet.RESP_CODES][rc()] = 1

    overall[KPISet.AVG_RESP_TIME] = r(100)
    overall[KPISet.AVG_CONN_TIME] = overall[KPISet.AVG_RESP_TIME] / 3.0
    overall[KPISet.AVG_LATENCY] = 2.0 * overall[KPISet.AVG_RESP_TIME] / 3.0

    overall.sum_rt = overall[KPISet.AVG_RESP_TIME] * overall[KPISet.SAMPLE_COUNT]
    overall.sum_cn = overall[KPISet.AVG_CONN_TIME] * overall[KPISet.SAMPLE_COUNT]
    overall.sum_lt = overall[KPISet.AVG_LATENCY] * overall[KPISet.SAMPLE_COUNT]
    cumul = point[DataPoint.CUMULATIVE].get('', KPISet())
    cumul.merge_kpis(overall)
    cumul.recalculate()

    point.recalculate()

    overall[KPISet.AVG_RESP_TIME] = r(100)
    overall[KPISet.AVG_CONN_TIME] = overall[KPISet.AVG_RESP_TIME] / 3.0
    overall[KPISet.AVG_LATENCY] = 2.0 * overall[KPISet.AVG_RESP_TIME] / 3.0
    return point


class BZTestCase(TestCase):
    def setUp(self):
        self.captured_logger = None
        self.log_recorder = None

    def sniff_log(self, log):
        self.log_recorder = RecordingHandler()
        self.captured_logger = log
        self.captured_logger.addHandler(self.log_recorder)

    def tearDown(self):
        if self.captured_logger:
            self.captured_logger.removeHandler(self.log_recorder)
            self.log_recorder.close()

    def assertFilesEqual(self, expected, actual):
        with open(expected) as exp, open(actual) as act:
            diff = list(difflib.unified_diff(exp.readlines(), act.readlines()))
            if diff:
                msg = "Failed asserting that two files are equal:\n" + actual + "\nversus\n" + expected + "\nDiff is:\n"
                raise AssertionError(msg + "".join(diff))


def local_paths_config():
    """ to fix relative paths """
    dirname = os.path.dirname(__file__)
    fds, fname = tempfile.mkstemp()
    os.close(fds)
    settings = {
        "modules": {
            "jmeter": {
                "path": dirname + "/resources/jmeter/jmeter-loader" + EXE_SUFFIX,
            },
            "grinder": {
                "path": dirname + "/resources/grinder/fake_grinder.jar",
            },
            "gatling": {
                "path": dirname + "/resources/gatling/gatling" + EXE_SUFFIX,
            },
            "junit": {
                "path": dirname + "/../build/selenium/tools/junit/junit.jar",
                "selenium-server": dirname + "/../build/selenium/selenium-server.jar"
            }
        }
    }
    jstring = json.dumps(settings)
    with open(fname, 'w') as fds:
        fds.write(jstring)
    return fname


class RecordingHandler(Handler):
    def __init__(self):
        super(RecordingHandler, self).__init__()
        self.info_buff = StringIO()
        self.err_buff = StringIO()
        self.debug_buff = StringIO()
        self.warn_buff = StringIO()

    def emit(self, record):
        """

        :type record: logging.LogRecord
        :return:
        """
        if record.levelno == logging.INFO:
            self.write_log(self.info_buff, record.msg, record.args)
        elif record.levelno == logging.ERROR:
            self.write_log(self.err_buff, record.msg, record.args)
        elif record.levelno == logging.WARNING:
            self.write_log(self.warn_buff, record.msg, record.args)
        elif record.levelno == logging.DEBUG:
            self.write_log(self.debug_buff, record.msg, record.args)

    def write_log(self, buff, str_template, args):
        str_template += "\n"
        if args:
            buff.write(u(str_template % args))
        else:
            buff.write(u(str_template))


