import json
import inspect
import os
import logging

from bzt.cli import CLI
from bzt.utils import EXE_SUFFIX, run_once, temp_file, get_full_path

ROOT_LOGGER = logging.getLogger("")


def _get_dir():
    filename = inspect.getouterframes(inspect.currentframe())[1][1]
    return get_full_path(filename, step_up=1)


# execute tests regardless of working directory
BZT_DIR = get_full_path(_get_dir(), step_up=2)
RESOURCES_DIR = os.path.join(BZT_DIR, 'tests', 'resources') + os.path.sep   # we need this sep cos a lot of
BUILD_DIR = os.path.join(BZT_DIR, 'tests', 'build', 'tmp') + os.path.sep    # wrong sting joining in tests
TEST_DIR = os.path.join(BZT_DIR, 'tests', 'build', 'test') + os.path.sep
BASE_CONFIG = os.path.join(BZT_DIR, 'bzt', 'resources', '10-base-config.yml')

os.chdir(BZT_DIR)

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
    fname = temp_file()
    settings = {
        "modules": {
            "jmeter": {
                "path": RESOURCES_DIR + "jmeter/jmeter-loader" + EXE_SUFFIX,
            },
            "grinder": {
                "path": RESOURCES_DIR + "grinder/fake_grinder.jar",
            },
            "gatling": {
                "path": RESOURCES_DIR + "gatling/gatling3" + EXE_SUFFIX,
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
