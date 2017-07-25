import time

from bzt.modules.csharp import NUnitExecutor
from tests import __dir__
from tests.modules.selenium import SeleniumTestCase


class TestNUnitExecutor(SeleniumTestCase):
    def test_build_and_run(self):
        self.obj.execution.merge({
            "scenario": {
                "script": __dir__() + "/../../resources/selenium/nunit/NUnitSelenium"
            }
        })
        self.obj.prepare()
        self.obj.startup()
        while not self.obj.check():
            time.sleep(1)
        self.obj.shutdown()
