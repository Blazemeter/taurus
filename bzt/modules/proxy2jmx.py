"""
Implementations for proxy2jmx service

Copyright 2016 BlazeMeter Inc.

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

import json
import time

import requests

from bzt.engine import Service
from bzt.modules.selenium import SeleniumExecutor


class Proxy2JMX(Service):
    def __init__(self):
        super(Proxy2JMX, self).__init__()
        self.proxy = None
        self.headers = {}
        self.api_delay = 5
        self.address = 'https://a.blazemeter.com/api/latest/mitmproxies'
        self.label = 'generated'

    def api_request(self, path='', method='GET', check=True):
        if method == 'GET':
            req = requests.get(self.address + path, headers=self.headers)
        elif method == 'POST':
            req = requests.post(self.address + path, headers=self.headers)
        else:
            raise RuntimeError('Unsupported method: %s' % method)

        if check and req.status_code != 200:
            json_content = json.loads(req.content)
            raise RuntimeError('API request failed: %s' % json_content['error']['message'])
        return req

    def __get_proxy(self):
        req = self.api_request(check=False)

        if req.status_code == 404:
            self.log.info('Creating new recording proxy...')
            req = self.api_request(method='POST')
            json_content = json.loads(req.content)
        elif req.status_code == 200:
            self.log.info('Using existing recording proxy...')
            json_content = json.loads(req.content)
            if json_content['result']['status'] == 'active':
                self.log.info('Proxy is active, stop it')
                self.api_request('/stopRecording', 'POST')
        else:
            json_content = json.loads(req.content)
            raise RuntimeError('API request failed: %s' % json_content['error']['message'])

        self.api_request('/clearRecording', 'POST')

        host = json_content['result']['host']
        port = json_content['result']['port']

        return 'http://%s:%s' % (host, port)

    def prepare(self):
        super(Proxy2JMX, self).prepare()
        self.address = self.settings.get('address', self.address)
        token = self.settings.get('token')
        if not token:
            token = self.engine.config.get('modules').get('blazemeter').get('token')

        if not token:
            raise ValueError("You must provide your API token to use Proxy Recorder")

        self.headers = {"X-Api-Key": token}
        self.proxy = self.__get_proxy()

    def startup(self):
        super(Proxy2JMX, self).startup()
        for executor in self.engine.provisioning.executors:
            if isinstance(executor, SeleniumExecutor):
                executor.additional_env['http_proxy'] = self.proxy
                executor.additional_env['https_proxy'] = self.proxy
                if executor.label:
                    self.label = executor.label

        self.log.info('Starting BlazeMeter recorder...')

        self.api_request('/startRecording', 'POST')

    def shutdown(self):
        super(Proxy2JMX, self).shutdown()
        self.log.info("Stopping BlazeMeter recorder...")
        self.api_request('/stopRecording', 'POST')

    def post_process(self):
        super(Proxy2JMX, self).post_process()
        if self.engine.stopping_reason and not isinstance(self.engine.stopping_reason, KeyboardInterrupt):
            self.log.info("Will not pick converted JMX due to exception: %s", self.engine.stopping_reason)
            return

        self.log.info("Waiting for proxy to generate JMX...")
        while True:
            req = self.api_request()
            json_content = json.loads(req.content)
            if json_content['result']['smartjmx'] == "available":
                break
            time.sleep(self.api_delay)

        req = self.api_request('/jmx?smart=true')
        jmx_file = self.engine.create_artifact(self.label, '.jmx')
        with open(jmx_file, 'w') as _file:
            _file.writelines(req.content)

        self.log.info("JMX saved into %s", jmx_file)
