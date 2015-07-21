from tests import BZTestCase
from bzt.jmx2yml import Converter
from bzt.six import etree, u
from io import StringIO
import yaml


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
    def test_load_jmx_file(self):

        try:
            obj = Converter("tests/jmx/http.jmx")
            obj.log = logger_mock()
            obj.load_jmx()
            self.assertIn("Loading jmx file", obj.log.debug_buff.getvalue())
            self.assertEqual("", obj.log.info_buff.getvalue())
            self.assertEqual("", obj.log.err_buff.getvalue())
        except:
            self.fail()

        try:
            obj = Converter("tests/jmx/notfound.jmx")
            obj.log = logger_mock()
            obj.load_jmx()
            self.assertIn("Loading jmx file", obj.log.debug_buff.getvalue())
            self.assertIn("does not exist", obj.log.err_buff.getvalue())
            self.assertEqual("", obj.log.info_buff.getvalue())
        except:
            self.fail()

        try:
            obj = Converter("tests/jmx/broken.jmx")
            obj.log = logger_mock()
            obj.load_jmx()
            self.assertIn("Loading jmx file", obj.log.debug_buff.getvalue())
            self.assertIn("Error while loading jmx file", obj.log.err_buff.getvalue())
            self.assertEqual("", obj.log.info_buff.getvalue())
        except:
            self.fail()

    def test_clean_jmx(self):
        obj = Converter("tests/jmx/http.jmx")
        obj.load_jmx()
        obj.clean_disabled_elements(obj.tree)
        obj.clean_jmx_tree(obj.tree)
        # with open("/tmp/result.jmx", 'wb') as fds:
        #     fds.write(etree.tostring(obj.tree, pretty_print=True, xml_declaration=True))

    def test_one_tg_disabled_one_sampler(self):
        obj = Converter("tests/yaml/converter/disabled.jmx")
        obj.load_jmx()
        obj.clean_disabled_elements(obj.tree)
        obj.convert()
        obj.log.debug(obj.scenario)
        obj.dump_yaml()
        yml = yaml.load(open("tests/yaml/converter/disabled.yml").read())
        self.assertEqual(obj.scenario, yml)
