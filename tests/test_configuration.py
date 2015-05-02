import logging
import six
import tempfile

from bzt.engine import Configuration
from bzt.utils import BetterDict
from tests import BZTestCase, __dir__


class TestConfiguration(BZTestCase):
    def test_load(self):
        obj = Configuration()
        configs = [
            __dir__() + "/../bzt/etc/10-base.json",
            __dir__() + "/json/jmx.json",
            __dir__() + "/json/concurrency.json"
        ]
        obj.load(configs)
        logging.debug("config:\n%s", obj)

        fname = tempfile.mkstemp()[1]
        obj.dump(fname, Configuration.JSON)
        with open(fname) as fh:
            logging.debug("JSON:\n%s", fh.read())

        fname = tempfile.mkstemp()[1]
        obj.dump(fname, Configuration.YAML)
        with open(fname) as fh:
            logging.debug("YAML:\n%s", fh.read())

        fname = tempfile.mkstemp()[1]
        obj.dump(fname, Configuration.INI)
        with open(fname) as fh:
            logging.debug("INI:\n%s", fh.read())

    def test_merge(self):
        obj = Configuration()
        configs = [
            __dir__() + "/yaml/test.yml",
            __dir__() + "/json/merge1.json",
            __dir__() + "/json/merge2.json",
        ]
        obj.load(configs)
        fname = tempfile.mkstemp()[1]
        obj.dump(fname, Configuration.JSON)
        with open(fname) as fh:
            logging.debug("JSON:\n%s", fh.read())
        jmeter = obj['modules']['jmeter']
        classval = jmeter['class']
        self.assertEquals("bzt.modules.jmeter.JMeterExecutor", classval)
        self.assertEquals("value", obj['key'])
        self.assertEquals(6, len(obj["list-append"]))
        self.assertEquals(2, len(obj["list-replace"]))
        self.assertEquals(2, len(obj["list-replace-notexistent"]))
        self.assertIsInstance(obj["list-complex"][1][0], BetterDict)
        self.assertIsInstance(obj["list-complex"][1][0], BetterDict)
        self.assertIsInstance(obj["list-complex"][1][0], BetterDict)
        self.assertFalse("properties" in jmeter)

        fname = tempfile.mkstemp()[1]
        obj.dump(fname, Configuration.JSON)
        checker = Configuration()
        checker.load([fname])
        token = checker["list-complex"][1][0]['token']
        self.assertNotEquals('test', token)
        token_orig = obj["list-complex"][1][0]['token']
        self.assertEquals('test', token_orig)

    def test_save(self):
        obj = Configuration()
        obj.merge({
            "str": "text",
            "uc": six.u("ucstring")
        })
        fname = tempfile.mkstemp()[1]
        obj.dump(fname, Configuration.YAML)
        with open(fname) as fh:
            written = fh.read()
            logging.debug("YAML:\n%s", written)
            self.assertNotIn("unicode", written)