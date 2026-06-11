from unittest import mock

from bzt import ToolError
from bzt.modules import RemoteExecutor
from bzt.utils import BetterDict
from tests.unit import BZTestCase


class FakeResponse:
    def __init__(self, status_code, payload):
        self.status_code = status_code
        self._payload = payload
        self.text = str(payload)

    def json(self):
        return self._payload


class TestRemoteExecutorListFiles(BZTestCase):
    def test_list_files_parses_files(self):
        obj = RemoteExecutor()
        obj.bridge_list_url = "http://bridge:8080/list"
        payload = {"files": [{"name": "apiritif.0.csv", "size": 10},
                             {"name": "apiritif.1.csv", "size": 0}]}
        with mock.patch("bzt.modules.requests.get", return_value=FakeResponse(200, payload)) as get:
            result = obj.list_files("C:/art", "apiritif.*.csv")
        self.assertEqual(2, len(result))
        self.assertEqual("apiritif.0.csv", result[0]["name"])
        get.assert_called_once_with("http://bridge:8080/list",
                                    params={"path": "C:/art", "glob": "apiritif.*.csv"})

    def test_list_files_raises_on_non_200(self):
        obj = RemoteExecutor()
        obj.bridge_list_url = "http://bridge:8080/list"
        with mock.patch("bzt.modules.requests.get", return_value=FakeResponse(500, "boom")):
            self.assertRaises(ToolError, obj.list_files, "C:/art", "*.csv")

    def test_list_files_missing_files_key_returns_empty(self):
        obj = RemoteExecutor()
        obj.bridge_list_url = "http://bridge:8080/list"
        with mock.patch("bzt.modules.requests.get", return_value=FakeResponse(200, {})):
            self.assertEqual([], obj.list_files("C:/art", "*.csv"))


class TestRemoteExecutorPrepare(BZTestCase):

    def _make_executor(self):
        obj = RemoteExecutor()
        obj.settings = BetterDict()
        obj.settings.merge({"bridge-url": "http://bridge:8080"})
        return obj

    def test_prepare_uses_root_path_from_info(self):
        obj = self._make_executor()
        info_payload = {"rootPath": "C:/bridge/test_results", "os": "windows"}
        mkdir_response = FakeResponse(200, {"output": "", "pid": 0})
        with mock.patch("bzt.modules.requests.get",
                        return_value=FakeResponse(200, info_payload)), \
             mock.patch("bzt.modules.requests.post",
                        return_value=mkdir_response):
            obj.prepare()
        self.assertTrue(obj.remote_root_path.startswith("C:/bridge/test_results"))
        self.assertEqual("windows", obj.bridge_os)

    def test_prepare_falls_back_to_TEMP_when_info_fails(self):
        obj = self._make_executor()
        mkdir_response = FakeResponse(200, {"output": "", "pid": 0})
        with mock.patch("bzt.modules.requests.get",
                        side_effect=Exception("connection refused")), \
             mock.patch("bzt.modules.requests.post",
                        return_value=mkdir_response):
            obj.prepare()
        self.assertEqual("%TEMP%", obj.remote_root_path)
        self.assertEqual("windows", obj.bridge_os)

    def test_prepare_sets_bridge_info_url(self):
        obj = self._make_executor()
        info_payload = {"rootPath": "C:/bridge/test_results", "os": "windows"}
        mkdir_response = FakeResponse(200, {"output": "", "pid": 0})
        with mock.patch("bzt.modules.requests.get",
                        return_value=FakeResponse(200, info_payload)), \
             mock.patch("bzt.modules.requests.post",
                        return_value=mkdir_response):
            obj.prepare()
        self.assertEqual("http://bridge:8080/info", obj.bridge_info_url)
