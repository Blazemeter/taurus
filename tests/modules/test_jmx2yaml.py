from tests import BZTestCase
from bzt.jmx2yml import Converter
from bzt.six import etree, u
from io import StringIO
import yaml
import os

class logger_mock(object):
    def __init__(self):
        self.info_buff = StringIO()
        self.err_buff = StringIO()
        self.debug_buff = StringIO()
        self.warn_buff = StringIO()

    def write_log(self, buff, str_template, *args):
        if args:
            buff.write(u(str_template % args))
            buff.write(u("\n"))
        else:
            buff.write(u(str_template))
            buff.write(u("\n"))

    def info(self, str_template, *args):
        self.write_log(self.info_buff, str_template, *args)

    def error(self, str_template, *args):
        self.write_log(self.err_buff, str_template, *args)

    def warning(self, str_template, *args):
        self.write_log(self.err_buff, str_template, *args)

    def debug(self, str_template, *args):
        self.write_log(self.debug_buff, str_template, *args)


class TestConverter(BZTestCase):

    def tearDown(self):
        if os.path.exists("build/tmp.yml"):
            os.remove("build/tmp.yml")

    def test_load_jmx_file(self):

        try:
            obj = Converter()
            obj.log = logger_mock()
            obj.load_jmx("tests/jmx/http.jmx")
        except:
            self.fail()

        self.assertIn("Loading jmx file", obj.log.debug_buff.getvalue())
        self.assertEqual("", obj.log.info_buff.getvalue())
        self.assertEqual("", obj.log.err_buff.getvalue())

        try:
            obj = Converter()
            obj.log = logger_mock()
            obj.load_jmx("tests/jmx/notfound.jmx")
        except:
            self.fail()

        self.assertIn("Loading jmx file", obj.log.debug_buff.getvalue())
        self.assertIn("does not exist", obj.log.err_buff.getvalue())
        self.assertEqual("", obj.log.info_buff.getvalue())

        try:
            obj = Converter()
            obj.log = logger_mock()
            obj.load_jmx("tests/jmx/broken.jmx")
            self.assertIn("Loading jmx file", obj.log.debug_buff.getvalue())
            self.assertIn("Error while loading jmx file", obj.log.err_buff.getvalue())
            self.assertEqual("", obj.log.info_buff.getvalue())
        except:
            self.fail()

    def test_clean_disabled_jmx(self):
        obj = Converter()
        obj.load_jmx("tests/yaml/converter/disabled.jmx")
        obj.clean_disabled_elements(obj.tree)
        disabled_elements = [element for element in obj.tree.iter() if element.get("enabled") == "false"]
        self.assertEquals(0, len(disabled_elements))

    def test_copy_global_csv_dataset(self):
        obj = Converter()
        obj.convert("tests/yaml/converter/global_copy.jmx")
        obj.log.debug(obj.scenario)
        obj.dump_yaml("build/tmp.yml")
        yml = yaml.load(open("build/tmp.yml").read())
        datasets_first_tg = yml.get("scenarios").get("Thread Group one").get("data-sources")
        datasets_second_tg = yml.get("scenarios").get("Thread Group two").get("data-sources")

        global_csv_tg_one = [dataset for dataset in datasets_first_tg if dataset.get('path') == 'global.csv']
        global_csv_tg_two = [dataset for dataset in datasets_second_tg if dataset.get('path') == 'global.csv']

        local_csv_tg_one = [dataset for dataset in datasets_first_tg if dataset.get('path') == 'local.csv']
        local_csv_tg_two = [dataset for dataset in datasets_second_tg if dataset.get('path') == 'local.csv']
        self.assertEqual(len(global_csv_tg_one), len(global_csv_tg_two), 1)
        self.assertEqual(len(local_csv_tg_one), 1)
        self.assertEqual(len(local_csv_tg_two), 0)

    def test_copy_global_headers(self):
        obj = Converter()
        obj.convert("tests/yaml/converter/global_copy.jmx")
        obj.log.debug(obj.scenario)
        obj.dump_yaml("build/tmp.yml")
        yml = yaml.load(open("build/tmp.yml").read())
        headers_first_tg = yml.get("scenarios").get("Thread Group one").get("headers", [])
        headers_second_tg = yml.get("scenarios").get("Thread Group two").get("headers", [])
        self.assertEqual(len(headers_first_tg), 3)
        self.assertEqual(len(headers_second_tg), 2)

    def test_cache_cookie_dns_overrides(self):
        obj = Converter()
        obj.convert("tests/yaml/converter/global_copy.jmx")
        obj.log.debug(obj.scenario)
        obj.dump_yaml("build/tmp.yml")
        yml = yaml.load(open("build/tmp.yml").read())
        tg_one = yml.get("scenarios").get("Thread Group one")
        tg_two = yml.get("scenarios").get("Thread Group two")
        cache_first_tg = tg_one.get("store-cache")
        cache_second_tg = tg_two.get("store-cache")
        cookie_first_tg = tg_one.get("store-cookie")
        cookie_second_tg = tg_two.get("store-cookie")
        dns_cache_mgr_first_tg = tg_one.get("use-dns-cache-mgr")
        dns_cache_mgr_second_tg = tg_two.get("use-dns-cache-mgr")
        self.assertEqual(cache_first_tg, True)
        self.assertEqual(cache_second_tg, True)
        self.assertEqual(cookie_first_tg, False)
        self.assertEqual(cookie_second_tg, True)
        self.assertEqual(dns_cache_mgr_first_tg, True)
        self.assertEqual(dns_cache_mgr_second_tg, True)

    def test_think_time_overrides(self):
        obj = Converter()
        obj.convert("tests/yaml/converter/global_copy.jmx")
        obj.log.debug(obj.scenario)
        obj.dump_yaml("build/tmp.yml")
        yml = yaml.load(open("build/tmp.yml").read())
        tg_one = yml.get("scenarios").get("Thread Group one")
        tg_two = yml.get("scenarios").get("Thread Group two")
        request_tg_two = tg_two.get("requests")[0]
        tg_one_timer = tg_one.get("think-time")
        tg_two_timer = tg_two.get("think-time")
        req_timer = request_tg_two.get("think-time")

        self.assertEqual(tg_one_timer, "200ms")
        self.assertEqual(tg_two_timer, "300ms")
        self.assertEqual(req_timer, "100ms")

    def test_request_defaults(self):
        obj = Converter()
        obj.convert("tests/yaml/converter/global_copy.jmx")
        obj.log.debug(obj.scenario)
        obj.dump_yaml("build/tmp.yml")
        yml = yaml.load(open("build/tmp.yml").read())
        tg_one = yml.get("scenarios").get("Thread Group one")
        tg_two = yml.get("scenarios").get("Thread Group two")
        self.assertEqual(tg_one.get("default-address"), "https://127.0.0.2/")
        self.assertEqual(tg_two.get("default-address"), "http://127.0.0.3:2582/resources/")
        self.assertEqual(tg_one.get("timeout"), "500ms")
        self.assertEqual(tg_two.get("timeout"), "100ms")
        self.assertEqual(tg_one.get("retrieve-resources"), True)
        self.assertEqual(tg_two.get("retrieve-resources"), True)
        self.assertEqual(tg_one.get("concurrent-pool-size"), 5)
        self.assertEqual(tg_two.get("concurrent-pool-size"), 10)

    def test_two_tg(self):
        obj = Converter()
        obj.convert("tests/yaml/converter/disabled.jmx")
        obj.log.debug(obj.scenario)
        obj.dump_yaml("build/tmp.yml")
        yml = yaml.load(open("build/tmp.yml").read())
        # with open("tmp/result.jmx", 'wb') as fds:
        #     fds.write(etree.tostring(obj.tree, pretty_print=True, xml_declaration=True))
        self.assertEqual(obj.scenario, yml)
