""" unit test """
import difflib
import inspect
import json
import logging
import tempfile
from random import random
from unittest.case import TestCase

import os

from bzt.cli import CLI
from bzt.modules.aggregator import DataPoint, KPISet
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
                "path": dirname + "/jmeter/jmeter-loader" + EXE_SUFFIX,
            },
            "grinder": {
                "path": dirname + "//grinder/fake_grinder.jar",
            },
            "gatling": {
                "path": dirname + "/gatling/gatling" + EXE_SUFFIX,
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
