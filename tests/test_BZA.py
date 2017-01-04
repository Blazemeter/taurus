import json
import logging
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

        accounts = user.accounts()
        workspaces = accounts.workspaces()
        projects = workspaces.projects()
        opls = workspaces.private_locations()
        tests = projects.tests()
        logging.info("Result: %s", json.dumps(tests, indent=True))

    def get_token(self):
        a = Configuration()
        a.load([os.path.expanduser("~/.bzt-rc")])
        return a['modules']['blazemeter']['token']
