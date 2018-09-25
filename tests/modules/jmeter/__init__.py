from tests import ROOT_LOGGER
from tests.mocks import EngineEmul
from bzt.modules.jmeter import JMeter, JMeterExecutor
from bzt.utils import get_full_path, HTTPClient


class MockResponse(object):
    def __init__(self, text=None, file=None):
        self.text = text
        self.file = file


class MockHTTPClient(HTTPClient):
    def __init__(self, responses_dict=None):
        super(MockHTTPClient, self).__init__()
        self.responses_dict = {}

    def add_response(self, method, url, text=None, file=None):
        self.responses_dict[(method, url)] = MockResponse(text, file)

    def download_file(self, url, filename, reporthook=None, data=None, timeout=None):
        key = ('GET', url)
        resp = self.responses_dict[key]
        with open(filename, 'wb') as fdsto:
            with open(resp.file, 'rb') as fdsfrom:
                fdsto.write(fdsfrom.read())
        return filename, {}

    def request(self, method, url, *args, **kwargs):
        key = (method, url)
        return self.responses_dict[key]


class MockJMeter(JMeter):
    def __init__(self, jmeter_version=JMeterExecutor.JMETER_VER, has_ctg=None, reaction=None, http_client=None):
        jmeter_path = "~/.bzt/jmeter-taurus/{version}/"
        jmeter_path = get_full_path(jmeter_path)

        if http_client is None:
            http_client = MockHTTPClient()

        super(MockJMeter, self).__init__(tool_path=jmeter_path, parent_logger=ROOT_LOGGER,
                                         jmeter_version=jmeter_version, jmeter_download_link=None, plugins=[],
                                         http_client=http_client)
        self.has_ctg = has_ctg
        self.reaction = reaction if reaction else []

    def ctg_plugin_installed(self):
        return self.has_ctg

    def _pmgr_call(self, params, env=None):
        # replaces real pmgr call
        reaction = self.reaction.pop(0)
        if 'raise' in reaction:
            raise reaction['raise']

        return reaction['output']


class MockJMeterExecutor(JMeterExecutor):
    def __init__(self, load=None, settings=None, has_ctg=None):
        super(MockJMeterExecutor, self).__init__()
        self.mock_install = True

        if load is None: load = {}
        if settings is None: settings = {}
        if has_ctg is None: has_ctg = True

        self.engine = EngineEmul()
        self.env = self.engine.env
        self.execution.merge(load)
        self.settings.merge({"detect-plugins": False})
        self.settings.merge(settings)
        self.tool = MockJMeter(has_ctg=has_ctg)

    def install_required_tools(self):
        if self.mock_install:
            self.tool = MockJMeter(jmeter_version=self.settings.get('version'))
        else:
            super(MockJMeterExecutor, self).install_required_tools()
