# coding=utf-8
import logging
import tempfile

from bzt import six
from bzt.engine import Configuration
from bzt.utils import BetterDict
from tests import BZTestCase, __dir__


class TestConfiguration(BZTestCase):
    def test_load(self):
        obj = Configuration()
        configs = [
            __dir__() + "/../bzt/resources/base-config.yml",
            __dir__() + "/resources/json/jmx.json",
            __dir__() + "/resources/json/concurrency.json"
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

    def test_merge(self):
        obj = Configuration()
        configs = [
            __dir__() + "/resources/yaml/test.yml",
            __dir__() + "/resources/json/merge1.json",
            __dir__() + "/resources/json/merge2.json",
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

    def test_unicode(self):
        obj = Configuration()
        expected = six.u("Юникод")
        obj.merge({
            "ustr": expected,
        })
        ustr = obj.get("ustr", "nope")
        self.assertEqual(ustr, expected)

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

    def test_masq_sensitive(self):
        obj = Configuration()
        obj.merge({
            "token": "my-precious",
            "my_password": "qweasdzxc",
            "secret": "secret",
            "secret_story": "story",
        })
        BetterDict.traverse(obj, Configuration.masq_sensitive)
        self.assertEquals(obj["token"], "*" * 8)
        self.assertEquals(obj["my_password"], "*" * 8)
        self.assertEquals(obj["secret"], "*" * 8)
        self.assertEquals(obj["secret_story"], "story")

    def test_filtering(self):
        obj = Configuration()
        obj.merge({
            "drop": "me",
            "also-drop": {"this": "drop"},
            "and-also-drop": ["thelist"],
            "but-keep": "value",
            "and-also-keep": {
                "nested": "value",
                "while-dropping": "some"
            }

        })
        obj.filter({"but-keep": True, "and-also-keep": {"nested": True}})
        self.assertEquals({"and-also-keep": {"nested": "value"}, "but-keep": "value"}, obj)

    def test_tabs(self):
        obj = Configuration()
        obj.tab_replacement_spaces = 4
        obj.load([__dir__() + "/resources/yaml/tabs-issue.yml"])
        fname = tempfile.mkstemp()[1]
        obj.dump(fname, Configuration.YAML)
        # import shutil; shutil.copy(fname, __dir__() + "/resources/yaml/tabs-issue-spaces.yml")
        self.assertFilesEqual(__dir__() + "/resources/yaml/tabs-issue-spaces.yml", fname)
