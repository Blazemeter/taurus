""" test """
import logging
import os
import tempfile
import sys
import random

from bzt.engine import Engine, Configuration
from bzt.utils import load_class
from bzt.engine import Provisioning, ScenarioExecutor, Reporter, AggregatorListener
from bzt.modules.provisioning import FileLister
from bzt.modules.aggregator import ResultsReader
from tests import random_sample


class EngineEmul(Engine):
    """
    emulating engine
    """

    def __init__(self):
        Engine.__init__(self, logging.getLogger(''))
        self.artifacts_base_dir = os.path.dirname(__file__) + "/../build/test"
        self._create_artifacts_dir()

        self.finalize_exc = None
        self.was_finalize = False

    def dump_config(self):
        """ test """
        fname = tempfile.mkstemp()[1]
        self.config.dump(fname, Configuration.JSON)
        with open(fname) as fh:
            logging.debug("JSON:\n%s", fh.read())


class ModuleMock(ScenarioExecutor, Provisioning, Reporter, FileLister):
    """ mock """

    def __init__(self):
        super(ModuleMock, self).__init__()
        self.postproc_exc = None
        self.check_exc = None
        self.prepare_exc = None
        self.startup_exc = None
        self.shutdown_exc = None

        self.check_iterations = sys.maxint

        self.was_shutdown = False
        self.was_startup = False
        self.was_prepare = False
        self.was_check = False
        self.was_postproc = False

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
        return [__file__]


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
