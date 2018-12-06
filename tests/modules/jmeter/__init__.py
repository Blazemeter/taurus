from tests.mocks import EngineEmul
from bzt.modules.jmeter import JMeter, JMeterExecutor
from bzt.utils import HTTPClient


class MockResponse(object):
    def __init__(self, text=None, file=None):
        self.text = text
        self.file = file


class MockHTTPClient(HTTPClient):
    def __init__(self):
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
    def __init__(self, has_ctg=None, reaction=None, **kwargs):
        kwargs["http_client"] = MockHTTPClient()
        super(MockJMeter, self).__init__(**kwargs)

        self.has_ctg = has_ctg
        self.reaction = reaction if reaction else []

    def ctg_plugin_installed(self):
        return self.has_ctg

    def _pmgr_call(self, params):
        # replaces real pmgr call
        reaction = self.reaction.pop(0)
        if 'raise' in reaction:
            raise reaction['raise']

        return reaction['output']


class MockJMeterExecutor(JMeterExecutor):
    def __init__(self, settings=None, has_ctg=None):
        super(MockJMeterExecutor, self).__init__()
        self.mock_install = True

        if has_ctg is None: has_ctg = True
        self.has_ctg = has_ctg

        self.settings.merge({"detect-plugins": False})
        if settings is not None:
            self.settings.merge(settings)

    def configure(self, load=None):
        if load is None: load = {}

        self.engine.config.merge({"execution": load, "settings": {"default-executor": "jmeter"}})
        self.engine.unify_config()
        self.execution.merge(self.engine.config.get("execution")[0])

    def install_required_tools(self):
        if self.mock_install:
            self.tool = self._get_tool(MockJMeter, props=self.properties, config=self.settings, has_ctg=self.has_ctg)
        else:
            super(MockJMeterExecutor, self).install_required_tools()
