import logging
import sys

import os

from bzt.modules.services import VirtualDisplay
from tests import BZTestCase, __dir__, local_paths_config

from bzt.modules.selenium import SeleniumExecutor
from tests.mocks import EngineEmul


class SeleniumTestCase(BZTestCase):
    """
    :type obj: SubprocessedExecutor
    """

    def __init__(self, methodName='runTest'):
        super(SeleniumTestCase, self).__init__(methodName)
        self.obj = None

    def setUp(self):
        super(SeleniumTestCase, self).setUp()
        engine_obj = EngineEmul()
        paths = [__dir__() + "/../../../bzt/resources/base-config.yml", local_paths_config()]
        engine_obj.configure(paths)  # FIXME: avoid using whole engine in particular module test!
        self.obj = SeleniumExecutor()
        self.obj.settings = engine_obj.config.get("modules").get("selenium")
        self.obj.settings.merge({"virtual-display": {"width": 1024, "height": 768}})
        engine_obj.create_artifacts_dir(paths)
        self.obj.engine = engine_obj

    def configure(self, config):
        self.obj.engine.config.merge(config)
        self.obj.execution = self.obj.engine.config.get('execution')
        if isinstance(self.obj.execution, list):
            self.obj.execution = self.obj.execution[0]

    def tearDown(self):
        super(SeleniumTestCase, self).tearDown()
        if isinstance(self.obj, SeleniumExecutor):
            if isinstance(self.obj.virtual_display_service, VirtualDisplay):
                self.obj.virtual_display_service.free_virtual_display()
