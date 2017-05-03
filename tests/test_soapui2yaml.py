import yaml

from bzt.soapui2yaml import SoapUI2YAML, process

from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


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
        self.engine = EngineEmul()

    def _get_soapui2yaml(self, path, file_name=None, test_case=None):
        return SoapUI2YAML(FakeOptions(file_name=file_name, test_case=test_case), __dir__() + path)

    def _get_tmp(self, prefix='test', suffix='.yml'):
        return self.engine.create_artifact(prefix, suffix)

    def test_convert(self):
        source = __dir__() + "/resources/soapui/project.xml"
        result = self._get_tmp()
        options = FakeOptions(file_name=result, test_case="index")
        process(options, [source])
        actual = yaml.load(open(result).read())
        expected = yaml.load(open(__dir__() + "/resources/soapui/project.xml.yml").read())
        self.assertEqual(actual, expected)

    def test_flickr(self):
        source = __dir__() + "/resources/soapui/flickr-sample.xml"
        result = self._get_tmp()
        options = FakeOptions(file_name=result)
        process(options, [source])
        actual = yaml.load(open(result).read())
        expected = yaml.load(open(__dir__() + "/resources/soapui/flickr-sample.xml.yml").read())
        self.assertEqual(actual, expected)
