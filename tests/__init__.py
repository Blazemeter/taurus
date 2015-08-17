""" unit test """
import inspect
import json
import logging
import os
import tempfile
from unittest.case import TestCase
from random import random

from bzt.cli import CLI
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.utils import run_once
from logging import Handler
from io import StringIO
from bzt.six import u

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
    overall[KPISet.PERCENTILES]['25'] = r(10)
    overall[KPISet.PERCENTILES]['50'] = r(20)
    overall[KPISet.PERCENTILES]['75'] = r(30)
    overall[KPISet.PERCENTILES]['90'] = r(40)
    overall[KPISet.PERCENTILES]['99'] = r(50)
    overall[KPISet.PERCENTILES]['100'] = r(100)
    overall[KPISet.RESP_CODES][rc()] = 1

    overall[KPISet.AVG_RESP_TIME] = r(100)
    overall[KPISet.AVG_CONN_TIME] = overall[KPISet.AVG_RESP_TIME] / 3.0
    overall[KPISet.AVG_LATENCY] = 2.0 * overall[KPISet.AVG_RESP_TIME] / 3.0

    cumul = point[DataPoint.CUMULATIVE].get('', KPISet())
    cumul.merge(overall)

    point.recalculate()

    overall[KPISet.AVG_RESP_TIME] = r(100)
    overall[KPISet.AVG_CONN_TIME] = overall[KPISet.AVG_RESP_TIME] / 3.0
    overall[KPISet.AVG_LATENCY] = 2.0 * overall[KPISet.AVG_RESP_TIME] / 3.0
    return point


class BZTestCase(TestCase):
    pass


def local_paths_config():
    """ to fix relative paths """
    dirname = os.path.dirname(__file__)
    fds, fname = tempfile.mkstemp()
    os.close(fds)
    settings = {
        "modules": {
            "jmeter": {
                "path": dirname + "/../build/jmeter/bin/jmeter",
            },
            "grinder": {
                "path": dirname + "/../build/grinder/lib/grinder.jar",
            },
            "gatling": {
                "path": dirname + "/../build/gatling/bin/gatling.sh",
            },
            "selenium": {
                "selenium-tools": {
                    "junit": {
                        "path": dirname + "/../build/selenium/tools/junit/junit.jar",
                        "selenium-server": dirname + "/../build/selenium/selenium-server.jar"
                    }
                }
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
            self.write_log(self.info_buff, record.msg, *record.args)
        elif record.levelno == logging.ERROR:
            self.write_log(self.err_buff, record.msg, *record.args)
        elif record.levelno == logging.WARN:
            self.write_log(self.warn_buff, record.msg, *record.args)
        elif record.levelno == logging.DEBUG:
            self.write_log(self.debug_buff, record.msg, *record.args)

    def write_log(self, buff, str_template, *args):
        if args:
            buff.write(u(str_template % args))
            buff.write(u("\n"))
        else:
            buff.write(u(str_template))
            buff.write(u("\n"))