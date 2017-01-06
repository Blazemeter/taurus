import time

import json

from bzt import TaurusNetworkError
from bzt.utils import MultiPartForm, to_json


class BlazeMeterClient(object):
    """ Service client class """

    def __init__(self, parent_logger):
        self.user_id = None
        self.test_id = None
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.token = None
        self.session_id = None
        self.master_id = None
        self.timeout = 10
        self.delete_files_before_test = False

    def upload_collection_resources(self, resource_files, draft_id):
        url = self.address + "/api/v4/web/elfinder/%s" % draft_id
        body = MultiPartForm()
        body.add_field("cmd", "upload")
        body.add_field("target", "s1_Lw")
        body.add_field('folder', 'drafts')

        for rfile in resource_files:
            body.add_file('upload[]', rfile)

        hdr = {"Content-Type": str(body.get_content_type())}
        resp = self._request(url, body.form_as_bytes(), headers=hdr)
        if "error" in resp:
            self.log.debug('Response: %s', resp)
            raise TaurusNetworkError("Can't upload resource files")

    def get_collections(self):
        resp = self._request(self.address + "/api/v4/collections")
        return resp['result']

    def import_config(self, config):
        url = self.address + "/api/v4/collections/taurusimport"
        resp = self._request(url, data=to_json(config), headers={"Content-Type": "application/json"}, method="POST")
        return resp['result']

    def update_collection(self, collection_id, coll):
        url = self.address + "/api/v4/collections/%s" % collection_id
        self._request(url, data=to_json(coll), headers={"Content-Type": "application/json"}, method="POST")

    def find_collection(self, collection_name, project_id):
        collections = self.get_collections()
        for collection in collections:
            self.log.debug("Collection: %s", collection)
            if "name" in collection and collection['name'] == collection_name:
                if not project_id or project_id == collection['projectId']:
                    self.log.debug("Matched: %s", collection)
                    return collection

    def find_test(self, test_name, project_id):
        tests = self.get_tests(test_name)
        for test in tests:
            self.log.debug("Test: %s", test)
            if "name" in test and test['name'] == test_name:
                if test['configuration']['type'] == "taurus":
                    if not project_id or project_id == test['projectId']:
                        self.log.debug("Matched: %s", test)
                        return test

    def launch_cloud_collection(self, collection_id):
        self.log.info("Initiating cloud test with %s ...", self.address)
        # NOTE: delayedStart=true means that BM will not start test until all instances are ready
        # if omitted - instances will start once ready (not simultaneously),
        # which may cause inconsistent data in aggregate report.
        url = self.address + "/api/v4/collections/%s/start?delayedStart=true" % collection_id
        resp = self._request(url, method="POST")
        self.log.debug("Response: %s", resp['result'])
        self.master_id = resp['result']['id']
        self.results_url = self.address + '/app/#/masters/%s' % self.master_id
        return self.results_url

    def force_start_master(self):
        self.log.info("All servers are ready, starting cloud test")
        url = self.address + "/api/v4/masters/%s/forceStart" % self.master_id
        self._request(url, method="POST")

    def stop_collection(self, collection_id):
        self.log.info("Shutting down cloud test...")
        url = self.address + "/api/v4/collections/%s/stop" % collection_id
        self._request(url)

    def create_collection(self, name, taurus_config, resource_files, proj_id):
        self.log.debug("Creating collection")
        if resource_files:
            draft_id = "taurus_%s" % int(time.time())
            self.upload_collection_resources(resource_files, draft_id)
            taurus_config.merge({"dataFiles": {"draftId": draft_id}})

        collection_draft = self.import_config(taurus_config)
        collection_draft['name'] = name

        self.log.debug("Creating new test collection: %s", name)
        collection_draft['name'] = name
        collection_draft['projectId'] = proj_id
        url = self.address + "/api/v4/collections"
        headers = {"Content-Type": "application/json"}
        resp = self._request(url, data=to_json(collection_draft), headers=headers, method="POST")
        collection_id = resp['result']['id']

        self.log.debug("Using collection ID: %s", collection_id)
        return collection_id

    def setup_collection(self, collection_id, name, taurus_config, resource_files, proj_id):
        self.log.debug("Setting up collection")
        if resource_files:
            draft_id = "taurus_%s" % int(time.time())
            self.upload_collection_resources(resource_files, draft_id)
            taurus_config.merge({"dataFiles": {"draftId": draft_id}})

        collection_draft = self.import_config(taurus_config)
        collection_draft['name'] = name
        collection_draft['projectId'] = proj_id

        self.update_collection(collection_id, collection_draft)

    def create_test(self, name, configuration, proj_id):
        self.log.debug("Creating new test")
        url = self.address + '/api/v4/tests'
        data = {"name": name, "projectId": proj_id, "configuration": configuration}
        hdr = {"Content-Type": " application/json"}
        resp = self._request(url, json.dumps(data), headers=hdr)
        test_id = resp['result']['id']
        self.log.debug("Using test ID: %s", test_id)
        return test_id
