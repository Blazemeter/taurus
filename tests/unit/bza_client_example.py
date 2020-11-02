import logging
import os
import sys

from bzt.bza import User, Test
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
    tests = workspaces.multi_tests(ident=10005302)
    tests.delete()
    print(tests)
    # for wsp in workspaces:
    #    wsp.fetch()
    # opls = workspaces.private_locations()
    # sel_test = workspaces.tests(name='Selenium')
    # projects = workspaces.projects()
    # tests = projects.multi_tests()
    # tests2 = workspaces.tests()


def test_external():
    user = Test()
    user.address = 'https://qa.blazemeter.com'
    user.logger_limit = sys.maxsize
    session, master, url = user.start_anonymous_external_test()
    session.stop_anonymous()


if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    test_flow()
