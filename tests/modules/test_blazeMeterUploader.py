import logging
import os
import shutil

from bzt.modules.blazemeter import BlazeMeterUploader, BlazeMeterClient
from tests import BZTestCase, random_datapoint
from tests.mocks import EngineEmul


class TestBlazeMeterUploader(BZTestCase):
    def test_check(self):
        client = BlazeMeterClientEmul(logging.getLogger(''))
        client.results.append({"marker": "ping", 'result': {}})
        #client.results.append({"marker": "tests", 'result': {}})
        #client.results.append({"marker": "test-create", 'result': {'id': 'unittest1'}})
        client.results.append(
            {"marker": "sess-start", 'result': {'session': {'id': 'sess1', 'userId': 1}, 'signature': ''}})
        client.results.append({"marker": "first push", 'result': {'session': {}}})
        # client.results.append(None)  # first check error stats
        client.results.append(
            {"marker": "second push", 'result': {'session': {"statusCode": 140, 'status': 'ENDED'}}})
        client.results.append(None)  # second check error stats
        client.results.append({"marker": "post-proc push", 'result': {'session': {}}})
        client.results.append(None)  # post-proc error stats
        client.results.append({"marker": "terminate", 'result': {'session': {}}})

        obj = BlazeMeterUploader()
        obj.engine = EngineEmul()
        shutil.copy(__file__, obj.engine.artifacts_dir + os.path.basename(__file__))
        obj.client = client
        obj.prepare()
        obj.startup()
        obj.aggregated_second(random_datapoint(0))
        obj.aggregated_second(random_datapoint(1))
        obj.aggregated_second(random_datapoint(2))
        obj.aggregated_second(random_datapoint(3))
        obj.aggregated_second(random_datapoint(4))
        obj.check()
        obj.aggregated_second(random_datapoint(5))
        obj.aggregated_second(random_datapoint(6))
        obj.aggregated_second(random_datapoint(7))
        obj.aggregated_second(random_datapoint(8))
        obj.aggregated_second(random_datapoint(9))
        try:
            obj.check()
            self.fail()
        except KeyboardInterrupt:
            pass
        obj.aggregated_second(random_datapoint(10))
        obj.shutdown()
        obj.post_process()


class BlazeMeterClientEmul(BlazeMeterClient):
    def __init__(self, parent_logger):
        super(BlazeMeterClientEmul, self).__init__(parent_logger)
        self.results = []

    def _request(self, url, data=None, headers=None, checker=None):
        self.log.debug("Request %s: %s", url, data)
        res = self.results.pop(0)
        self.log.debug("Response: %s", res)
        return res
