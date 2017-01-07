import os
import sys

import logging

from bzt.bza import User
from bzt.engine import Configuration


def get_token():
    a = Configuration()
    a.load([os.path.expanduser("~/.bzt-rc")])
    return a['modules']['blazemeter']['token']


def test_flow():
    user = User()
    user.token = get_token()
    user.logger_limit = sys.maxsize

    user.fetch()
    accounts = user.accounts()
    workspaces = accounts.workspaces()
    # opls = workspaces.private_locations()
    # sel_test = workspaces.tests(name='Selenium')
    projects = workspaces.projects()
    tests = projects.multi_tests()
    # tests2 = workspaces.tests()


if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    test_flow()
