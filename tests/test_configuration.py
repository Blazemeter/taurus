# coding=utf-8
import json

from bzt.engine import Configuration
from bzt.utils import BetterDict, dehumanize_time, temp_file
from tests import BZTestCase, RESOURCES_DIR, BASE_CONFIG, ROOT_LOGGER
from tests.mocks import EngineEmul


class TestConfiguration(BZTestCase):
    def test_load(self):
        obj = Configuration()
        configs = [
            BASE_CONFIG,
            RESOURCES_DIR + "json/jmx.json",
            RESOURCES_DIR + "json/concurrency.json"
        ]
        obj.load(configs)
        ROOT_LOGGER.debug("config:\n%s", obj)

        fname = temp_file()
        obj.dump(fname, Configuration.JSON)
        with open(fname) as fh:
            ROOT_LOGGER.debug("JSON:\n%s", fh.read())

        fname = temp_file()
        obj.dump(fname, Configuration.YAML)
        with open(fname) as fh:
            ROOT_LOGGER.debug("YAML:\n%s", fh.read())

    def test_merge(self):
        obj = Configuration()
        configs = [
            RESOURCES_DIR + "yaml/test.yml",
            RESOURCES_DIR + "json/merge1.json",
            RESOURCES_DIR + "json/merge2.json",
        ]
        obj.load(configs)
        fname = temp_file()
        obj.dump(fname, Configuration.JSON)
        with open(fname) as fh:
            ROOT_LOGGER.debug("JSON:\n%s", fh.read())
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

        fname = temp_file()
        obj.dump(fname, Configuration.JSON)
        checker = Configuration()
        checker.load([fname])
        token = checker["list-complex"][1][0]['token']
        self.assertNotEquals('test', token)
        token_orig = obj["list-complex"][1][0]['token']
        self.assertEquals('test', token_orig)

    def test_unicode(self):
        obj = Configuration()
        expected = "Юникод"
        obj.merge({
            "ustr": expected,
        })
        ustr = obj.get("ustr", "nope")
        self.assertEqual(ustr, expected)

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
            },
            "filter-subitems": {
                "keep": "value",
                "drop": "some"
            }

        })
        rules = {
            "but-keep": True,
            "and-also-keep": {"nested": True},
            "!filter-subitems": {"drop": True},
        }
        obj.filter(rules)

        expected = {
            "but-keep": "value",
            "and-also-keep": {"nested": "value"},
            "filter-subitems": {"keep": "value"},
        }
        self.assertEquals(expected, obj)

    def test_tabs(self):
        obj = Configuration()
        obj.tab_replacement_spaces = 4
        obj.load([RESOURCES_DIR + "yaml/tabs-issue.yml"])
        fname = temp_file()
        obj.dump(fname, Configuration.YAML)
        self.assertFilesEqual(RESOURCES_DIR + "yaml/tabs-issue-spaces.yml", fname)

    def test_merge_removal(self):
        obj = Configuration()
        obj.merge({
            "foo": "bar",
        })
        obj.merge({
            "^foo": "baz",
        })
        self.assertNotIn("foo", obj)

    def test_merge_overwrite(self):
        obj = Configuration()
        obj.merge({
            "foo": {"bar": "baz"},
        })
        obj.merge({
            "~foo": "baz",
        })
        self.assertEqual(obj["foo"], "baz")

    def test_elementwise_merge(self):
        obj = Configuration()
        obj.merge({
            "execution": [{
                "executor": "jmeter",
                "iterations": 10,
            }],
        })
        obj.merge({
            "$execution": [{"iterations": 20}],
        })
        self.assertEqual(obj["execution"][0]["iterations"], 20)

    def test_elementwise_merge_do_not_erase(self):
        obj = Configuration()
        obj.merge({
            "execution": [{
                "executor": "jmeter",
                "iterations": 10,
            }, {
                "executor": "selenium",
                "iterations": 30,
            }],
        })
        obj.merge({
            "$execution": [{"iterations": 20}],
        })
        self.assertEqual(obj["execution"][0]["iterations"], 20)
        self.assertEqual(obj["execution"][1]["iterations"], 30)

    def test_elementwise_merge_right_is_bigger(self):
        obj = Configuration()
        obj.merge({
            "execution": [{
                "executor": "jmeter",
                "iterations": 10,
            }],
        })
        obj.merge({
            "$execution": [{"iterations": 20}, {"iterations": 30}],
        })
        self.assertEqual(obj["execution"][0]["iterations"], 20)
        self.assertEqual(obj["execution"][1]["iterations"], 30)

    def test_encode_decode_infinities(self):
        engine = EngineEmul()
        obj = Configuration()
        obj.merge({
            "foo": float("inf"),
        })
        cfg = engine.create_artifact("config", ".json")
        obj.dump(cfg, Configuration.JSON)
        with open(cfg) as fds:
            dump = json.loads(fds.read())
        self.assertEqual(dump["foo"], "inf")
        self.assertEqual(dehumanize_time(dump["foo"]), float("inf"))

    def test_overwrite_execution_locations(self):
        obj = Configuration()
        obj.merge({
            "execution": [{"locations": {"us-central1-a": 1}}],
        })
        obj.merge({
            "$execution": [{"~locations": {"harbor-1": 1}}],
        })
        ROOT_LOGGER.info(obj)
        self.assertEqual(obj, {"execution": [{"locations": {"harbor-1": 1}}]})
