""" test """
import logging
import os
import random
import sys
import tempfile
from _socket import SOCK_STREAM, AF_INET
from io import StringIO
from logging import Handler

from bzt.engine import Engine, Configuration, FileLister, HavingInstallableTools
from bzt.engine import Provisioning, ScenarioExecutor, Reporter
from bzt.modules.aggregator import ResultsReader, AggregatorListener
from bzt.modules.functional import FunctionalResultsReader
from bzt.six import u
from bzt.utils import load_class
from tests import random_sample

try:
    from exceptions import KeyboardInterrupt
except ImportError:
    # noinspection PyUnresolvedReferences
    from builtins import KeyboardInterrupt


class EngineEmul(Engine):
    def __init__(self):
        super(EngineEmul, self).__init__(logging.getLogger(''))
        self.config.get('settings')['artifacts-dir'] = os.path.dirname(__file__) + "/../build/test/%Y-%m-%d_%H-%M-%S.%f"
        self.config.get('settings')['check-updates'] = False
        self.create_artifacts_dir()
        self.config.merge({"provisioning": "local"})
        self.finalize_exc = None
        self.was_finalize = False

    def dump_config(self):
        """ test """
        fname = tempfile.mkstemp()[1]
        self.config.dump(fname, Configuration.JSON)
        with open(fname) as fh:
            logging.debug("JSON:\n%s", fh.read())


class ModuleMock(ScenarioExecutor, Provisioning, Reporter, FileLister, HavingInstallableTools):
    """ mock """

    def __init__(self):
        super(ModuleMock, self).__init__()
        self.postproc_exc = None
        self.check_exc = None
        self.prepare_exc = None
        self.startup_exc = None
        self.shutdown_exc = None

        self.check_iterations = sys.maxsize

        self.was_shutdown = False
        self.was_startup = False
        self.was_prepare = False
        self.was_check = False
        self.was_postproc = False

        self.is_has_results = False

    def prepare(self):
        """
        :raise self.prepare_exc:
        """
        self.log.info("Preparing mock")
        self.was_prepare = True
        self.check_iterations = int(self.settings.get('check_iterations', "2"))
        self.postproc_exc = self.get_exc("postproc")
        self.check_exc = self.get_exc("check")
        self.prepare_exc = self.get_exc("prepare")
        self.startup_exc = self.get_exc("startup")
        self.shutdown_exc = self.get_exc("shutdown")

        if isinstance(self.engine.aggregator, ResultsReader):
            reader = MockReader()
            for num in range(0, self.check_iterations):
                for quan in range(0, int(random.random() * 10)):
                    reader.data.append(random_sample(num))
            # noinspection PyUnresolvedReferences
            self.engine.aggregator.add_reader(reader)

        if self.prepare_exc:
            raise self.prepare_exc

    def startup(self):
        """
        :raise self.startup_exc:
        """
        self.log.info("Startup mock")
        self.was_startup = True
        if self.startup_exc:
            raise self.startup_exc

    def check(self):
        """
        :return: :raise self.check_exc:
        """
        self.was_check = True
        self.log.info("Checks remaining: %s", self.check_iterations)
        self.check_iterations -= 1
        if not self.check_iterations:
            if self.check_exc:
                raise self.check_exc
            else:
                return True
        return False

    def shutdown(self):
        """
        :raise self.shutdown_exc:
        """
        self.log.info("Shutdown mock")
        self.was_shutdown = True
        if self.shutdown_exc:
            raise self.shutdown_exc

    def post_process(self):
        """
        :raise self.postproc_exc:
        """
        self.log.info("Postproc mock")
        self.was_postproc = True
        if self.postproc_exc:
            raise self.postproc_exc

    def get_exc(self, param):
        """
        :type param: str
        :return:
        """
        name = self.settings.get(param, "")
        if name:
            cls = load_class(name)
            return cls()
        return None

    def resource_files(self):
        """


        :return:
        """
        self.execution.get('files', []).append(__file__)
        return [__file__]

    def has_results(self):
        return self.is_has_results

    def install_required_tools(self):
        self.log.debug("All is good")


class MockReader(ResultsReader, AggregatorListener):
    """
    test
    """

    def __init__(self):
        super(MockReader, self).__init__()
        self.results = []
        self.data = []
        self.add_listener(self)
        self.track_percentiles = [0, 50, 90, 99, 99.5, 100]

    def _read(self, final_pass=False):
        """
        Emulating read samples

        :type final_pass: bool
        :return:
        """
        while self.data:
            # logging.debug("Emul read: %s", self.data[0])
            yield self.data.pop(0)

    def aggregated_second(self, data):
        """
        Store and assert aggregate sequence

        :type data: dict
        :raise AssertionError:
        """
        if self.results:
            if self.results[-1]["ts"] >= data["ts"]:
                raise AssertionError("TS sequence wrong: %s>=%s" % (self.results[-1]["ts"], data["ts"]))
        logging.info("Data: %s", data)
        self.results.append(data)


class MockFunctionalReader(FunctionalResultsReader):
    def __init__(self):
        super(MockFunctionalReader, self).__init__()
        self.data = []

    def read(self, last_pass=False):
        while self.data:
            yield self.data.pop(0)


# noinspection PyUnusedLocal
def download_progress_mock(blocknum, blocksize, totalsize):
    pass


class ResultChecker(AggregatorListener):
    def __init__(self, callback):
        super(ResultChecker, self).__init__()
        self.callback = callback

    def aggregated_second(self, data):
        self.callback(data)


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
        elif record.levelno == logging.WARN:
            self.write_log(self.warn_buff, record.msg, record.args)
        elif record.levelno == logging.DEBUG:
            self.write_log(self.debug_buff, record.msg, record.args)

    def write_log(self, buff, str_template, args):
        str_template += "\n"
        if args:
            buff.write(u(str_template % args))
        else:
            buff.write(u(str_template))


class SocketEmul(object):
    # noinspection PyUnusedLocal
    def __init__(self, family=AF_INET, atype=SOCK_STREAM, proto=0, _sock=None):
        self.recv_data = ""
        self.sent_data = ""

    def connect(self, address):
        logging.debug("Emulated connect to %s", address)

    # noinspection PyUnusedLocal
    def recv(self, buf_size, flags=None):
        data = self.recv_data[:buf_size]
        self.recv_data = self.recv_data[buf_size + 1:]
        logging.debug("Emulated recv: %s", data)
        return data

    # noinspection PyUnusedLocal
    def send(self, data, flags=None):
        self.sent_data += data
        logging.debug("Emulated send: %s", data)

    def setblocking(self, state):
        logging.debug("Emulate setblocking=%s", state)

    def fileno(self):
        return 0

    def close(self):
        logging.debug("Emulated close")
