from unittest import mock

from bzt import ToolError
from bzt.modules import RemoteExecutor
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
