import json
import os
import zipfile

from bzt.engine import Service, Provisioning
from bzt.modules.blazemeter import CloudProvisioning, BlazeMeterClientEmul
from bzt.modules.services import Unpacker, Proxy2JMX
from bzt.utils import get_files_recursive
from tests import BZTestCase, __dir__
from tests.mocks import EngineEmul
from bzt.modules.selenium import SeleniumExecutor


class Proxy2JMXEmul(Proxy2JMX):
    responses = []

    def api_request(self, path='', method='GET', check=True):
        return self.responses.pop(0)


class ResponseEmul:
    def __init__(self, status_code, content):
        self.status_code = status_code
        self.content = content


class TestProxy2JMX(BZTestCase):
    def test_no_token(self):
        obj = Proxy2JMXEmul()
        obj.engine = EngineEmul()
        obj.engine.config.merge({})
        obj.settings = obj.engine.config.get('recorder')
        self.assertRaises(ValueError, obj.prepare)

    def test_full(self):
        obj = Proxy2JMXEmul()
        obj.api_delay = 1
        obj.responses = [
            ResponseEmul(404, ''),
            ResponseEmul(200, '{"result" : {"port": "port1", "host": "host1"}}'),
            ResponseEmul(200, ''),
            ResponseEmul(200, ''),  # startup: startRecording
            ResponseEmul(200, ''),  # shutdown: stopRecording
            ResponseEmul(200, '{"result" : {"smartjmx": "unavailable"}}'),
            ResponseEmul(200, '{"result" : {"smartjmx": "available"}}'),
            ResponseEmul(200, 'only one string')
        ]
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            'modules': {
                'recorder': {
                    'token': '123'}}})
        obj.settings = obj.engine.config.get('modules').get('recorder')

        obj.prepare()
        self.assertEqual(obj.proxy, 'host1:port1')
        obj.engine.provisioning.executors = [SeleniumExecutor()]
        obj.startup()
        obj.shutdown()
        obj.post_process()
        res_file = obj.engine.artifacts_dir + '/generated.jmx'
        with open(res_file) as fd:
            lines = fd.readlines()
        self.assertEqual(len(lines), 1)
        self.assertEqual(lines[0].strip(), 'only one string')

    def test_existing_proxy(self):
        obj = Proxy2JMXEmul()
        obj.api_delay = 1
        obj.responses = [
            ResponseEmul(200, '{"result" : {"port": "port1", "host": "host1", "status": "active"}}'),
            ResponseEmul(200, ''),  # stopRecording
            ResponseEmul(200, '')   # clearRecording
        ]
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            'modules': {
                'recorder': {
                    'token': '123'}}})
        obj.settings = obj.engine.config.get('modules').get('recorder')

        obj.prepare()
        self.assertEqual(obj.proxy, 'host1:port1')


class TestZipFolder(BZTestCase):
    def test_pack_and_send_to_blazemeter(self):
        obj = CloudProvisioning()
        obj.engine = EngineEmul()

        obj.engine.config.merge({
            "execution": {
                "executor": "selenium",
                "concurrency": 5500,
                "locations": {
                    "us-east-1": 1,
                    "us-west": 2},
                "scenario": {
                    "script": __dir__() + "/../selenium/java_package"}},
            "modules": {
                "selenium": "bzt.modules.selenium.SeleniumExecutor",
                "cloud": "bzt.modules.blazemeter.CloudProvisioning"},
            "provisioning": "cloud"
        })

        obj.parameters = obj.engine.config['execution']
        obj.settings["token"] = "FakeToken"
        obj.client = client = BlazeMeterClientEmul(obj.log)
        client.results.append(self.__get_user_info())  # user
        client.results.append({"result": []})  # tests
        client.results.append({"result": {"id": id(client)}})  # create test
        client.results.append({"files": []})  # create test
        client.results.append({})  # upload files
        client.results.append({"result": {"id": id(obj)}})  # start
        client.results.append({"result": {"id": id(obj)}})  # get master
        client.results.append({"result": []})  # get master sessions
        client.results.append({})  # terminate

        obj.prepare()

        unpack_cfgs = obj.engine.config.get(Service.SERV)
        self.assertEqual(len(unpack_cfgs), 1)
        self.assertEqual(unpack_cfgs[0]['module'], Unpacker.UNPACK)
        self.assertEqual(unpack_cfgs[0][Unpacker.FILES], ['java_package.zip'])
        self.assertTrue(zipfile.is_zipfile(obj.engine.artifacts_dir + '/java_package.zip'))

    @staticmethod
    def __get_user_info():
        with open(__dir__() + "/../json/blazemeter-api-user.json") as fhd:
            return json.loads(fhd.read())

    def test_receive_and_unpack_on_worker(self):
        obj = Unpacker()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            "execution": {
                "executor": "selenium",
                "concurrency": 5500,
                "scenario": {
                    "script": "java_package.zip"}},
            "modules": {
                "selenium": "bzt.modules.selenium.SeleniumExecutor",
                "cloud": "bzt.modules.blazemeter.CloudProvisioning"},
            "provisioning": "local"
        })
        obj.engine.file_search_paths = [obj.engine.artifacts_dir]

        obj.parameters["files"] = ["java_package.zip"]

        # create archive and put it in artifact dir
        source = __dir__() + "/../selenium/java_package"
        zip_name = obj.engine.create_artifact('java_package', '.zip')
        with zipfile.ZipFile(zip_name, 'w', zipfile.ZIP_STORED) as zip_file:
            for filename in get_files_recursive(source):
                zip_file.write(filename, filename[len(os.path.dirname(source)):])

        obj.prepare()

        # check unpacked tree
        destination = obj.engine.artifacts_dir + '/java_package'
        result_tree = set(filename[len(destination):] for filename in get_files_recursive(destination))
        original_tree = set(filename[len(source):] for filename in get_files_recursive(source))
        self.assertEqual(result_tree, original_tree)

    def test_no_work_prov(self):
        obj = Unpacker()
        obj.engine = EngineEmul()
        obj.engine.config[Provisioning.PROV] = 'cloud'
        obj.parameters.merge({Unpacker.FILES: ['notexists.zip']})
        obj.prepare()
