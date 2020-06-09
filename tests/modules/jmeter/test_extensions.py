# coding=utf-8
import json
import os
import shutil
import time
from unittest import skipUnless, skipIf
from distutils.version import LooseVersion
from xml.etree import ElementTree

import yaml

from bzt import ToolError, TaurusConfigError, TaurusInternalException
from bzt.jmx import JMX
from bzt.jmx.tools import ProtocolHandler
from bzt.modules.aggregator import ConsolidatingAggregator
from bzt.modules.blazemeter import CloudProvisioning
from bzt.modules.functional import FunctionalAggregator
from bzt.modules.jmeter import JTLReader, FuncJTLReader, JMeter
from bzt.modules.provisioning import Local
from bzt.six import etree, u
from bzt.utils import EXE_SUFFIX, get_full_path, BetterDict, is_windows, JavaVM
from tests import RESOURCES_DIR, BUILD_DIR, close_reader_file, ExecutorTestCase
from . import MockJMeterExecutor, MockHTTPClient

_jvm = JavaVM()
_jvm.check_if_installed()
java_version = _jvm.version
java10 = LooseVersion(java_version) >= LooseVersion("10")


class TestJMeterExecutor(ExecutorTestCase):
    EXECUTOR = MockJMeterExecutor

    def tearDown(self):
        if self.obj.modified_jmx and os.path.exists(self.obj.modified_jmx):
            os.remove(self.obj.modified_jmx)
        if self.obj.reader:
            if isinstance(self.obj.reader, FuncJTLReader):
                close_reader_file(self.obj.reader)
            if isinstance(self.obj.reader, JTLReader):
                close_reader_file(self.obj.reader.csvreader)
                close_reader_file(self.obj.reader.errors_reader)

        super(TestJMeterExecutor, self).tearDown()

    def configure(self, config):
        """
        Merge config into engine, setup provisioning,
        setup execution and settings attributes for executor.

        :return:
        """
        path = os.path.join(RESOURCES_DIR, "jmeter/jmeter-loader" + EXE_SUFFIX)
        self.obj.settings.merge({
            'path': path,
            'force-ctg': False,
            'protocol-handlers': {"rte": "bzt.jmx.extension.rte.RTEProtocolHandler"}})

        super(TestJMeterExecutor, self).configure(config)
        self.obj.settings.merge(self.obj.engine.config.get('modules').get('jmeter'))

        prov = self.obj.engine.config.get('provisioning')
        if prov == 'local':
            self.obj.engine.provisioning = Local()
        elif prov == 'cloud':
            self.obj.engine.provisioning = CloudProvisioning()
        else:
            self.fail('Wrong provisioning value: %s' % prov)

    def test_rte_server_and_connect(self):
        self.configure({"execution":
            {"scenario":
                {
                    "protocol": "rte",
                    "rte-server": "myHost",
                    "rte-config": {
                        "terminal-type": "IBM-3179-2",
                        "connection-timeout": "60s0ms"
                    },
                    "rte-protocol": "TN5250",
                    "requests": [{
                        "rte-action": "CONNECT",
                        "label": "Connection to server"
                    }]}}})
        self.obj.prepare()
        xml_tree = open(self.obj.modified_jmx, "rb").read().decode("utf-8")
        self.assertTrue("RTEConnectionConfig.server" in xml_tree)
        self.assertTrue("RTEConnectionConfig.protocol" in xml_tree)
        self.assertTrue("RTEConnectionConfig.terminalType" in xml_tree)
        self.assertTrue("RTEConnectionConfig.connectTimeout" in xml_tree)

    def test_rte_send_text(self):
        self.configure({"execution":
            {"scenario":
                {
                    "protocol": "rte",
                    "rte-server": "myHost",
                    "rte-config": {
                        "terminal-type": "IBM-3179-2",
                        "connection-timeout": "60s0ms"
                    },
                    "rte-protocol": "TN5250",
                    "requests": [{
                        "rte-action": "CONNECT",
                        "label": "Connection to server"
                    },
                    {
                        "rte-action": "SEND_INPUT",
                        "label": "RTE Action",
                        "attention-key": "ENTER",
                        "fields": [
                            {"tabs": "0",
                             "text": "MyHost"}

                        ]
                    }
                    ]}}})
        self.obj.prepare()
        xml_tree = open(self.obj.modified_jmx, "rb").read().decode("utf-8")
        self.assertTrue("RTEConnectionConfig.server" in xml_tree)
        self.assertTrue("RTEConnectionConfig.protocol" in xml_tree)
        self.assertTrue("RTEConnectionConfig.terminalType" in xml_tree)
        self.assertTrue("RTEConnectionConfig.connectTimeout" in xml_tree)
