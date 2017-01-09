import re
import time
import logging

from unittest import TestCase


class TestInstallation(TestCase):
    def test_install_sdk(self):
        pass
        # todo: install sdk from local archive

    def test_install_appium(self):
        pass
        # todo: check for exception


class MockWebDriverRemote(object):
    def __init__(self, addr, caps):
        self.addr = addr
        self.caps = caps
        self.cmd_list = []
        self.data = []

    def get(self):
        self.cmd_list.append('get')
        return self.data.pop()

    def page_source(self):
        self.cmd_list.append('page_source')
        return self.data.pop()

    def quit(self):
        self.cmd_list.append('quit')




