import json
import tempfile
import inspect
import os
import logging

ROOT_LOGGER = logging.getLogger("")


def __dir__():
    filename = inspect.getouterframes(inspect.currentframe())[1][1]
    return os.path.dirname(filename)


# execute tests regardless of working directory
root_dir = __dir__() + '/../'
os.chdir(root_dir)

RESOURCES_DIR = os.path.join(__dir__(), 'resources') + os.path.sep
BUILD_DIR = __dir__() + "/../build/tmp/"
TEST_DIR = __dir__() + "/../build/test/"
BASE_CONFIG = __dir__() + "/../bzt/resources/10-10-base-config.yml"

from bzt.cli import CLI
from bzt.utils import EXE_SUFFIX, run_once


@run_once
def setup_test_logging():
    """ set up test logging for convenience in IDE """
    if not ROOT_LOGGER.handlers:
        CLI.log = ''  # means no log file will be created
        CLI.verbose = True
        CLI.setup_logging(CLI)
    else:
        ROOT_LOGGER.debug("Already set up logging")


setup_test_logging()
ROOT_LOGGER.info("Bootstrapped test")


def close_reader_file(obj):
    if obj and obj.file and obj.file.fds:
        obj.file.fds.close()


def local_paths_config():
    """ to fix relative paths """
    dirname = os.path.dirname(__file__)
    fds, fname = tempfile.mkstemp()
    os.close(fds)
    settings = {
        "modules": {
            "jmeter": {
                "path": RESOURCES_DIR + "jmeter/jmeter-loader" + EXE_SUFFIX,
            },
            "grinder": {
                "path": RESOURCES_DIR + "grinder/fake_grinder.jar",
            },
            "gatling": {
                "path": RESOURCES_DIR + "gatling/gatling" + EXE_SUFFIX,
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
