import yaml

from bzt.soapui2yaml import SoapUI2YAML

from tests.unit import BZTestCase, RESOURCES_DIR, EngineEmul


class FakeOptions(object):
    def __init__(self, verbose=True, file_name=None, test_case=None, quiet=False, json=False, log=False):
        self.verbose = verbose
        self.file_name = file_name
        self.test_case = test_case
        self.quiet = quiet
        self.json = json
        self.log = log


class TestConverter(BZTestCase):
    def setUp(self):
        super(TestConverter, self).setUp()
        self.engine = EngineEmul()

    def _get_tmp(self, prefix='test', suffix='.yml'):
        return self.engine.create_artifact(prefix, suffix)

    def configure(self, options, source):
        self.tool = SoapUI2YAML(options, source)
        self.clean_log()

    def test_convert(self):
        source = RESOURCES_DIR + "soapui/project.xml"
        result = self._get_tmp()
        options = FakeOptions(file_name=result, test_case="index")
        self.configure(options, source)
        self.tool.process()
        actual = yaml.full_load(open(result).read())
        expected = yaml.full_load(open(RESOURCES_DIR + "soapui/project.xml.yml").read())
        self.assertEqual(actual, expected)

    def test_flickr(self):
        source = RESOURCES_DIR + "soapui/flickr-sample.xml"
        result = self._get_tmp()
        options = FakeOptions(file_name=result)
        self.configure(options, source)
        self.tool.process()
        actual = yaml.full_load(open(result).read())
        expected = yaml.full_load(open(RESOURCES_DIR + "soapui/flickr-sample.xml.yml").read())
        self.assertEqual(actual, expected)

    def test_egalaxy(self):
        source = RESOURCES_DIR + "soapui/egalaxy.xml"
        result = self._get_tmp()
        options = FakeOptions(file_name=result)
        self.configure(options, source)
        self.tool.process()
        actual = yaml.full_load(open(result).read())
        expected = yaml.full_load(open(RESOURCES_DIR + "soapui/egalaxy.xml.yml").read())
        self.assertEqual(actual, expected)

    def test_smart(self):
        source = RESOURCES_DIR + "soapui/smart.xml"
        result = self._get_tmp()
        options = FakeOptions(file_name=result)
        self.configure(options, source)
        self.tool.process()
        self.configure(options, source)
        self.tool.process()
        actual = yaml.full_load(open(result).read())
        expected = yaml.full_load(open(RESOURCES_DIR + "soapui/smart.xml.yml").read())
        self.assertEqual(actual, expected)