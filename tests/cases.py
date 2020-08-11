import logging
import difflib
import ast
import astunparse
import sys
from io import StringIO
from logging import Handler
from unittest.case import TestCase

from bzt.engine import ScenarioExecutor, EXEC
from bzt.engine import SelfDiagnosable
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

    @staticmethod
    def assertFilesEqual(expected, actual, replace_str="", replace_with="", python_files=False):
        def order(line):
            line = line.replace(',', ' ,')  # for imports
            line = line.replace('(', '( ')  # for
            line = line.replace(')', ' )')  # calls
            line = line.split(" ")
            line.sort()
            return ' '.join(line)

        def equal_by_content(diff):
            # todo: it doesn't show diff for follow case, shouldn't we fix it?
            # 01: + func1()
            # 02:   func2()
            # 03: - func1()
            # func1 moved and order has been changed
            act_lines = [line[1:] for line in diff if line.startswith('-')]
            exp_lines = [line[1:] for line in diff if line.startswith('+')]
            for pair in zip(act_lines, exp_lines):
                if order(pair[0]) != order(pair[1]):
                    return False

            return True

        if isinstance(replace_str, str):
            replace_str = [replace_str]
        if isinstance(replace_with, str):
            replace_with = [replace_with]
        with open(expected) as exp, open(actual) as act:
            act_lines = act.readlines()
            exp_lines = exp.readlines()

        subs = dict(zip(replace_str, replace_with))
        subs.update({'<': '< ', '>': ' >'})     # for xml

        for key in subs:
            act_lines = [x.replace(key, subs[key]).rstrip() for x in act_lines]
            exp_lines = [x.replace(key, subs[key]).rstrip() for x in exp_lines]

        if python_files:
            act_lines = astunparse.unparse(ast.parse('\n'.join(act_lines))).split('\n')
            exp_lines = astunparse.unparse(ast.parse('\n'.join(exp_lines))).split('\n')

        diff = list(difflib.unified_diff(exp_lines, act_lines))

        if diff and not equal_by_content(diff[2:]):
            ROOT_LOGGER.info("Replacements are: %s => %s", replace_str, replace_with)
            msg = "Failed asserting that two files are equal:\n%s\nversus\n%s\nDiff is:\n\n%s"
            # here we show full diff, even equal_by_content
            # todo: show only really different lines

            raise AssertionError(msg % (actual, expected, "\n".join(diff)))

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
        self.obj.execution = self.obj.engine.config.get(EXEC)[0]

    def tearDown(self):
        if self.obj.stdout:
            self.obj.stdout.close()
        if self.obj.stderr:
            self.obj.stderr.close()
        super(ExecutorTestCase, self).tearDown()


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
            buff.write(str_template % args)
        else:
            buff.write(str_template)
