import base64
import threading

import requests


class BridgeFilePuller(threading.Thread):
    CHUNK_SIZE = 1024 * 1024

    def __init__(self, file_url, remote_path, local_path, log):
        super().__init__(daemon=True)
        self._file_url = file_url
        self._remote_path = remote_path
        self._local_path = local_path
        self.log = log
        self._stop_event = threading.Event()

    def stop(self):
        self._stop_event.set()

    def run(self):
        try:
            self._pull_loop()
        except Exception:
            self.log.debug("BridgeFilePuller terminated with error", exc_info=True)

    def _pull_loop(self):
        offset = 0
        with open(self._local_path, "wb") as f:
            while not self._stop_event.is_set():
                try:
                    resp = requests.get(self._file_url, params={
                        "offset": offset,
                        "limit": self.CHUNK_SIZE,
                        "path": self._remote_path,
                    })
                    if resp.status_code != 200:
                        self.log.debug("BridgeFilePuller: status %s, retrying", resp.status_code)
                        self._stop_event.wait(1)
                        continue
                    data = resp.json()
                    chunk_b64 = data.get("chunk", "")
                    resp_limit = data.get("limit", self.CHUNK_SIZE)
                    if chunk_b64:
                        f.write(base64.b64decode(chunk_b64))
                        f.flush()
                    offset += resp_limit
                except Exception:
                    self.log.debug("BridgeFilePuller: request error, retrying", exc_info=True)
                self._stop_event.wait(1)
