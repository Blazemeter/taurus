import base64
import logging
import os
import tempfile
import threading
import time
from unittest import mock

from tests.unit import BZTestCase
from bzt.modules._bridge_file_puller import BridgeFilePuller


class FakeResp:
    def __init__(self, status_code, payload):
        self.status_code = status_code
        self._payload = payload

    def json(self):
        return self._payload


def _make_puller(local_path):
    return BridgeFilePuller(
        file_url="http://bridge/file",
        remote_path="C:/art/file.csv",
        local_path=local_path,
        log=logging.getLogger("test"),
    )


class TestBridgeFilePuller(BZTestCase):

    def test_is_daemon_thread(self):
        with tempfile.NamedTemporaryFile(delete=False) as f:
            path = f.name
        try:
            puller = _make_puller(path)
            self.assertTrue(puller.daemon)
        finally:
            os.unlink(path)

    def test_writes_chunks_to_file(self):
        chunk1 = base64.b64encode(b"hello").decode()
        chunk2 = base64.b64encode(b" world").decode()
        with tempfile.NamedTemporaryFile(delete=False) as f:
            path = f.name
        try:
            puller = _make_puller(path)
            call_count = [0]
            payloads = [
                {"chunk": chunk1, "limit": 5},
                {"chunk": chunk2, "limit": 6},
            ]

            def fake_get(url, params=None):
                idx = call_count[0]
                call_count[0] += 1
                if idx < len(payloads):
                    return FakeResp(200, payloads[idx])
                puller.stop()
                return FakeResp(200, {"chunk": "", "limit": 0})

            with mock.patch("bzt.modules._bridge_file_puller.requests.get", side_effect=fake_get):
                puller.start()
                puller.join(timeout=5)

            with open(path, "rb") as f:
                self.assertEqual(b"hello world", f.read())
        finally:
            os.unlink(path)

    def test_stop_exits_loop(self):
        with tempfile.NamedTemporaryFile(delete=False) as f:
            path = f.name
        try:
            puller = _make_puller(path)

            def fake_get(url, params=None):
                return FakeResp(200, {"chunk": "", "limit": 0})

            with mock.patch("bzt.modules._bridge_file_puller.requests.get", side_effect=fake_get):
                puller.start()
                time.sleep(0.05)
                puller.stop()
                puller.join(timeout=3)

            self.assertFalse(puller.is_alive())
        finally:
            os.unlink(path)

    def test_network_error_does_not_propagate(self):
        with tempfile.NamedTemporaryFile(delete=False) as f:
            path = f.name
        try:
            puller = _make_puller(path)
            call_count = [0]

            def fake_get(url, params=None):
                call_count[0] += 1
                if call_count[0] == 1:
                    raise ConnectionError("bridge unreachable")
                puller.stop()
                return FakeResp(200, {"chunk": "", "limit": 0})

            with mock.patch("bzt.modules._bridge_file_puller.requests.get", side_effect=fake_get):
                puller.start()
                puller.join(timeout=5)

            self.assertFalse(puller.is_alive())
        finally:
            os.unlink(path)

    def test_non_200_retries_without_advancing_offset(self):
        with tempfile.NamedTemporaryFile(delete=False) as f:
            path = f.name
        try:
            puller = _make_puller(path)
            call_count = [0]
            seen_offsets = []

            def fake_get(url, params=None):
                call_count[0] += 1
                seen_offsets.append(params.get("offset", 0))
                if call_count[0] == 1:
                    return FakeResp(500, {})
                puller.stop()
                return FakeResp(200, {"chunk": "", "limit": 0})

            with mock.patch("bzt.modules._bridge_file_puller.requests.get", side_effect=fake_get):
                puller.start()
                puller.join(timeout=5)

            self.assertEqual([0, 0], seen_offsets)
        finally:
            os.unlink(path)
