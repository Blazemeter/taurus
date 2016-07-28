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


class Proxy2JMX(Service):
    def __init__(self):
        super(Proxy2JMX, self).__init__()
        self.proxy = None
        self.headers = {}
        self.api_delay = 5
        self.address = 'https://a.blazemeter.com/api/latest/mitmproxies'

    def api_request(self, path='', method='GET', check=True):
        if method == 'GET':
            req = requests.get(self.address + path, headers=self.headers)
        elif method == 'POST':
            req = requests.post(self.address + path, headers=self.headers)
        else:
            raise RuntimeError('Unsupported method: %s', method)

        if check and req.status_code != 200:
            json_content = json.loads(req.content)
            raise RuntimeError('%s', json_content['error']['message'])
        return req

    def prepare(self):
        super(Proxy2JMX, self).prepare()
        self.address = self.settings.get('address', self.address)
        token = self.settings.get('token')
        if not token:
            token = self.engine.config.get('modules').get('blazemeter').get('token')

        if not token:
            raise ValueError("You need token for recording")

        self.headers = {"X-Api-Key": token}

        req = self.api_request(check=False)

        if req.status_code == 404:
            self.log.info('Proxy not found, create it')
            req = self.api_request(method='POST')
            json_content = json.loads(req.content)
        elif req.status_code == 200:
            self.log.info('Proxy found')
            json_content = json.loads(req.content)
            if json_content['result']['status'] == 'active':
                self.log.info('Proxy is active, stop it')
                self.api_request('/stopRecording', 'POST')
        else:
            json_content = json.loads(req.content)
            raise RuntimeError('%s', json_content['error']['message'])

        self.api_request('/clearRecording', 'POST')

        host = json_content['result']['host']
        port = json_content['result']['port']

        self.proxy = '%s:%s' % (host, port)

    def startup(self):
        super(Proxy2JMX, self).startup()
        for executor in self.engine.provisioning.executors:
            if isinstance(executor, SeleniumExecutor):
                executor.additional_env['http_proxy'] = "http://%s/" % self.proxy
                executor.additional_env['https_proxy'] = "http://%s/" % self.proxy

        self.log.info('Starting BlazeMeter recorder...')

        self.api_request('/startRecording', 'POST')

    def shutdown(self):
        super(Proxy2JMX, self).shutdown()
        self.log.info("Stop BlazeMeter recorder")
        self.api_request('/stopRecording', 'POST')

    def post_process(self):
        super(Proxy2JMX, self).post_process()
        self.log.info("Waiting for JMX")
        while True:
            req = self.api_request()
            json_content = json.loads(req.content)
            if json_content['result']['smartjmx'] == "available":
                break
            time.sleep(self.api_delay)

        req = self.api_request('/jmx?smart=true')
        jmx_file = self.engine.create_artifact('generated', '.jmx')
        with open(jmx_file, 'w') as _file:
            _file.writelines(req.content)

        self.log.info("JMX saved into %s", jmx_file)
