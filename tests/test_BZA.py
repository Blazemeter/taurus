import os
import sys

from bzt.bza import User
from bzt.engine import Configuration
from tests import BZTestCase


class TestBZA(BZTestCase):
    def test_flow(self):
        user = User()
        user.token = self.get_token()
        user.logger_limit = sys.maxsize

        user.fetch()
        accounts = user.accounts()
        workspaces = accounts.workspaces()
        opls = workspaces.private_locations()
        # sel_test = workspaces.tests(name='Selenium')
        # projects = workspaces.projects()
        # tests = projects.tests(name='Selenium')
        # tests2 = workspaces.tests()
        print

    def get_token(self):
        a = Configuration()
        a.load([os.path.expanduser("~/.bzt-rc")])
        return a['modules']['blazemeter']['token']
