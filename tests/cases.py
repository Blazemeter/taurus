import logging
import difflib
import sys
from io import StringIO
from logging import Handler
from unittest.case import TestCase

from bzt.engine import ScenarioExecutor
from bzt.engine import SelfDiagnosable
from bzt.six import u
from bzt.utils import get_full_path
from tests import ROOT_LOGGER
from tests.mocks import EngineEmul

TestCase.shortDescription = lambda self: None  # suppress nose habit to show docstring instead of method name


class BZTestCase(TestCase):
    def setUp(self):
        self.captured_logger = None
        self.log_recorder = None
        self.func_args = []
        self.func_results = None
        self.log = ROOT_LOGGER

    def func_mock(self, *args, **kwargs):
        self.func_args.append({'args': args, 'kargs': kwargs})
        if isinstance(self.func_results, list):
            return self.func_results.pop(0)
        else:
            return self.func_results

    def sniff_log(self, log=ROOT_LOGGER):
        if not self.captured_logger:
            self.log_recorder = RecordingHandler()
            self.captured_logger = log
            self.captured_logger.addHandler(self.log_recorder)

    def tearDown(self):
        exc, _, _ = sys.exc_info()
        if exc:
            try:
                if hasattr(self, 'obj') and isinstance(self.obj, SelfDiagnosable):
                    diags = self.obj.get_error_diagnostics()
                    if diags:
                        for line in diags:
                            ROOT_LOGGER.info(line)
            except BaseException:
                pass
        if self.captured_logger:
            self.captured_logger.removeHandler(self.log_recorder)
            self.log_recorder.close()

    def assertFilesEqual(self, expected, actual, replace_str="", replace_with=""):
        # import shutil; shutil.copy(actual, expected)

        with open(expected) as exp, open(actual) as act:
            act_lines = [x.replace(replace_str, replace_with).rstrip() for x in act.readlines()]
            exp_lines = [x.replace(replace_str, replace_with).rstrip() for x in exp.readlines()]
            diff = list(difflib.unified_diff(exp_lines, act_lines))
            if diff:
                ROOT_LOGGER.info("Replacements are: %s => %s", replace_str, replace_with)
                msg = "Failed asserting that two files are equal:\n" + actual + "\nversus\n" + expected + "\nDiff is:\n"
                raise AssertionError(msg + "\n".join(diff))

    def assertPathsEqual(self, p1, p2):
        if not isinstance(p1, list):
            p1 = [p1]

        if not isinstance(p2, list):
            p2 = [p2]

        for num in range(len(p1)):
            self.assertEqual(get_full_path(p1[num]), get_full_path(p2[num]))


class ExecutorTestCase(BZTestCase):
    EXECUTOR = ScenarioExecutor

    def setUp(self):
        super(ExecutorTestCase, self).setUp()
        self.engine = EngineEmul()
        self.obj = self.EXECUTOR()
        self.obj.engine = self.engine

    def configure(self, config):
        self.obj.engine.config.merge({"settings": {"default-executor": "mock"}})
        self.obj.engine.config.merge(config)
        self.obj.engine.unify_config()
        self.obj.execution = self.obj.engine.config.get(ScenarioExecutor.EXEC)[0]


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
