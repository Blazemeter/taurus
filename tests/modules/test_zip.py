import zipfile
import json
import os


from bzt.modules.provisioning import Local
from bzt.engine import ScenarioExecutor
from tests.mocks import EngineEmul, ModuleMock
from bzt.modules.blazemeter import CloudProvisioning, BlazeMeterClientEmul
from tests import BZTestCase, __dir__
from bzt.utils import get_files_recursive


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

        self.assertEqual(obj.engine.config.get('packed'), ['java_package.zip'])
        self.assertTrue(zipfile.is_zipfile(obj.engine.artifacts_dir + '/java_package.zip'))

    @staticmethod
    def __get_user_info():
        with open(__dir__() + "/../json/blazemeter-api-user.json") as fhd:
            return json.loads(fhd.read())

    def test_receive_and_unpack_on_worker(self):
        obj = Local()
        obj.engine = EngineEmul()
        obj.engine.config.merge({
            "execution": {},
            "settings": {
                "default-executor": "mock"
            },
            "modules": {
                "mock": ModuleMock.__module__ + "." + ModuleMock.__name__
            },
            "provisioning": "mock"
        })
        obj.parameters = obj.engine.config['execution']

        # create archive and put it in artifact dir
        source = __dir__() + "/../selenium/java_package"
        zip_name = obj.engine.create_artifact('java_package', '.zip')
        with zipfile.ZipFile(zip_name, 'w', zipfile.ZIP_STORED) as zip_file:
            for filename in get_files_recursive(source):
                zip_file.write(filename, filename[len(os.path.dirname(source)):])
        obj.engine.config['packed'] = ['java_package.zip']

        obj.prepare()

        self.assertFalse(os.path.isfile(obj.engine.artifacts_dir + '/java_package.zip'))  # is archive removed?

        # check unpacked tree
        destination = obj.engine.artifacts_dir + '/java_package'
        result_tree = set(filename[len(destination):] for filename in get_files_recursive(destination))
        original_tree = set(filename[len(source):] for filename in get_files_recursive(source))
        self.assertEqual(result_tree, original_tree)


