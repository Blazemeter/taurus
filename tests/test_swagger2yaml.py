import yaml

from bzt.swagger2yaml import Swagger2YAML, process

from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul


class FakeOptions(object):
    def __init__(self, verbose=True, file_name=None, get_only=True, quiet=False, json=False, log=False):
        self.verbose = verbose
        self.file_name = file_name
        self.get_only = get_only
        self.quiet = quiet
        self.json = json
        self.log = log


class TestConverter(BZTestCase):
    def setUp(self):
        self.engine = EngineEmul()

    def _get_swagger2yaml(self, path, file_name=None, get_only=True):
        return Swagger2YAML(FakeOptions(file_name=file_name, get_only=get_only), __dir__() + path)

    def _get_tmp(self, prefix='test', suffix='.yml'):
        return self.engine.create_artifact(prefix, suffix)

    def test_convert(self):
        source = __dir__() + "/swagger/petstore.json"
        result = self._get_tmp()
        options = FakeOptions(file_name=result, get_only=True)
        process(options, [source])
        actual = yaml.load(open(result).read())
        expected = yaml.load(open(__dir__() + "/swagger/petstore-converted.yaml").read())
        self.assertEqual(actual, expected)

    def test_convert_all_methods(self):
        source = __dir__() + "/swagger/petstore.json"
        result = self._get_tmp()
        options = FakeOptions(file_name=result, get_only=False)
        process(options, [source])
        actual = yaml.load(open(result).read())
        expected = yaml.load(open(__dir__() + "/swagger/petstore-converted-all.yaml").read())
        self.assertEqual(actual, expected)

