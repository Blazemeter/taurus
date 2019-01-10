import json
import logging
import math
import os
import shutil
import time
from io import BytesIO

from bzt import TaurusException
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.modules.influx import InfluxUploader
from bzt.six import HTTPError
from bzt.six import iteritems, viewvalues
from tests import BZTestCase, random_datapoint, RESOURCES_DIR, ROOT_LOGGER
from tests.mocks import EngineEmul, InfluxMock


class TestInfluxUploader(BZTestCase):
    def test_check(self):
        mock = InfluxMock()
        #mock.mock_get.update({
        #    'http://localhost:8086/write?db=jmeter': {},
        #})
        #mock.mock_post.update({
        #    'http://localhost:8086/write?db=jmeter': {},
        #})

        obj = InfluxUploader()
        obj.parameters['measurement'] = 'jmeter'
        obj.parameters['application'] = 'taurus'
        obj.parameters['influx-url'] = 'http://localhost:8086/write?db=jmeter'
        obj.engine = EngineEmul()
        shutil.copy(__file__, os.path.join(obj.engine.artifacts_dir, os.path.basename(__file__)))
        
        #obj._user.timeout = 0.1 
        obj.prepare()
        mock.overwrite_request(obj)
        obj.startup()
        for x in range(0, 31):
            obj.aggregated_second(random_datapoint(x))
        #mon = [{"ts": 1, "source": "local", "cpu": 1, "mem": 2, "bytes-recv": 100, "other": 0}]
        #obj.monitoring_data(mon)
        #obj.check()
        for x in range(32, 65):
            obj.aggregated_second(random_datapoint(x))
        obj.last_dispatch = time.time() - 2 * obj.send_interval
        obj.aggregated_second(random_datapoint(10))
        obj.shutdown()
        log_file = obj.engine.create_artifact('log', '.tmp')
        handler = logging.FileHandler(log_file)
        obj.engine.log.parent.addHandler(handler)
        obj.engine.config.get('modules').get('shellexec').get('env')['TAURUS_INDEX_ALL'] = 1
        obj.post_process()
        #self.assertEqual(20, len(mock.requests))
        obj.engine.log.parent.removeHandler(handler)


    def test_send_datapoint(self):
        obj = InfluxUploader()
        obj.engine = EngineEmul()
        
        mock = InfluxMock()

        obj.parameters['measurement'] = 'jmeter'
        obj.parameters['application'] = 'taurus'
        obj.parameters['influx-url'] = 'http://localhost:8086/write?db=jmeter'
        #mock.overwrite_request(obj)
        #mock.mock_post.update({'http://localhost:8086/write?db=jmeter': ""
        #    })
    
        obj.prepare()
        mock.overwrite_request(obj)
        obj.startup()
        obj.check()
        obj.shutdown()
        obj.post_process()
        #self.assertEquals(1, )
#        self.assertEqual(6, len(mock.requests), "Requests were: %s" % mock.requests)

    #{"error":"database not found: \"atlantis\""}
    #no database

    def test_invalid_database(self):
        obj = InfluxUploader()
        obj.engine = EngineEmul()
        obj.parameters['measurement'] = 'jmeter'
        obj.parameters['application'] = 'taurus'
        obj.parameters['influx-url'] = 'http://localhost:8086/write?db=wrongdb'
        obj.prepare()
        mock = InfluxMock()
        mock.overwrite_request(obj)
        mock.mock_post.update({
            'http://localhost:8086/write?db=wrongdb': HTTPError(None, 404, "{\"error\":\"database not found: \"wrongdb\"\"}", None, None, )
        })
        #mock.overwrite_request(obj)
        #self.assertRaises(HTTPError, obj.startup)

    def test_busy_server(self):
        obj = InfluxUploader()
        obj.engine = EngineEmul()
        obj.parameters['measurement'] = 'jmeter'
        obj.parameters['application'] = 'taurus'
        obj.parameters['influx-url'] = 'http://localhost:8086/write?db=jmeter'
        obj.prepare()
        mock = InfluxMock()
        mock.overwrite_request(obj)
        mock.mock_post.update({
            'http://localhost:8086/write?db=jmeter': HTTPError(None, 500, "", None, None, )
        })


class DummyHttpResponse(object):
    def __init__(self):
        self.fake_socket = BytesIO()
        self.fake_socket.write(open(RESOURCES_DIR + "unicode_file", 'rb').read())

    def read(self):
        self.fake_socket.seek(0)
        return self.fake_socket.read(1024)