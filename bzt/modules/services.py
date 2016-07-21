"""
Implementations for small services

Copyright 2015 BlazeMeter Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
"""

import copy
import os
import zipfile
import json
import requests
import time

from bzt.engine import Provisioning
from bzt.engine import Service
from bzt.modules.selenium import SeleniumExecutor
from bzt.utils import replace_in_config


class Unpacker(Service):
    UNPACK = 'unpacker'
    FILES = 'files'

    def __init__(self):
        super(Unpacker, self).__init__()
        self.files = []

    def prepare(self):
        prov = self.engine.config.get(Provisioning.PROV)
        runat = self.parameters.get("run-at", "local")
        if prov != runat:
            self.log.debug("Not running unpacker because of non-matching prov: %s != %s", prov, runat)
            return

        packed_list = copy.deepcopy(self.parameters.get(Unpacker.FILES, self.files))
        unpacked_list = []
        for archive in packed_list:
            full_archive_path = self.engine.find_file(archive)
            self.log.debug('Unpacking %s', archive)
            with zipfile.ZipFile(full_archive_path) as zip_file:
                zip_file.extractall(self.engine.artifacts_dir)

            archive = os.path.basename(archive)
            unpacked_list.append(archive[:-4])  # TODO: replace with top-level archive content

        replace_in_config(self.engine.config, packed_list, unpacked_list, log=self.log)


class Recorder(Service):
    API_URL = 'https://a.blazemeter.com/api/latest/mitmproxies'

    def __init__(self):
        super(Recorder, self).__init__()
        self.proxy = None
        self.headers = {}

    def prepare(self):
        super(Recorder, self).prepare()
        token = self.parameters.get('token')
        if not token:
            token = self.engine.config.get('modules').get('blazemeter').get('token')

        if not token:
            token = self.engine.config.get('modules').get('cloud').get('token')

        if not token:
            raise ValueError("You need token for recording")

        self.headers = {"X-Api-Key": token}
        self.log.debug('Create Blazemeter proxy')
        while True:
            req = requests.delete(self.API_URL, headers=self.headers)
            if req.status_code == 404:
                break
            if req.status_code != 200:
                json_content = json.loads(req.content)
                raise RuntimeError('%s', json_content['error']['message'])
            time.sleep(1)

        req = requests.post(self.API_URL, headers=self.headers)
        json_content = json.loads(req.content)
        if req.status_code != 200:
            raise RuntimeError('%s', json_content['error']['message'])

        host = json_content['result']['host']
        port = json_content['result']['port']

        self.proxy = '%s:%s' % (host, port)

    def startup(self):
        super(Recorder, self).startup()
        for executor in self.engine.provisioning.executors:
            if isinstance(executor, SeleniumExecutor):
                executor.additional_env['http_proxy'] = "http://%s/" % self.proxy
                executor.additional_env['https_proxy'] = "http://%s/" % self.proxy

        self.log.debug('Start BlazeMeter recorder')
        req = requests.post(self.API_URL + '/clearRecording', headers=self.headers)
        if req.status_code != 200:
            json_content = json.loads(req.content)
            raise RuntimeError('%s', json_content['error']['message'])

        req = requests.post(self.API_URL + '/startRecording', headers=self.headers)
        if req.status_code != 200:
            json_content = json.loads(req.content)
            raise RuntimeError('%s', json_content['error']['message'])

    def shutdown(self):
        super(Recorder, self).shutdown()
        self.log.debug("Stop BlazeMeter recorder")
        req = requests.post(self.API_URL + '/stopRecording', headers=self.headers)
        if req.status_code != 200:
            json_content = json.loads(req.content)
            raise RuntimeError('%s', json_content['error']['message'])

    def post_process(self):
        super(Recorder, self).post_process()
        self.log.debug("Prepare JMX file")
        while True:
            req = requests.get(self.API_URL, headers=self.headers)
            json_content = json.loads(req.content)
            if req.status_code != 200:
                raise RuntimeError('%s', json_content['error']['message'])
            if json_content['result']['smartjmx'] == "available":
                break
            time.sleep(1)

        req = requests.get(self.API_URL + '/jmx?smart=true', headers=self.headers)
        jmx_file = self.engine.create_artifact('generated', '.jmx')
        with open(jmx_file, 'w') as _file:
            _file.writelines(req.content)

        self.log.debug("JMX saved into %s", jmx_file)
