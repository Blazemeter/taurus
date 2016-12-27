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
import sys
import time

import requests

from bzt import TaurusConfigError, TaurusNetworkError, TaurusInternalException
from bzt.engine import Service
from bzt.modules.selenium import AbstractSeleniumExecutor


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
            raise TaurusInternalException('Unsupported API request method: %s' % method)

        if check and req.status_code != 200:
            json_content = json.loads(req.content)
            raise TaurusNetworkError('API request failed: %s' % json_content['error']['message'])
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
            raise TaurusNetworkError('API request failed: %s' % json_content['error']['message'])

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
            msg = "You must provide your API token in settings of " \
                  "'proxy2jmx' or 'blazemeter' modules to use Proxy Recorder"
            raise TaurusConfigError(msg)

        self.headers = {"X-Api-Key": token}
        self.proxy = self.__get_proxy()

    def startup(self):
        super(Proxy2JMX, self).startup()
        self.log.info('Starting BlazeMeter recorder...')

        labels = []
        is_linux = 'linux' in sys.platform.lower()
        if is_linux:
            self.log.info('Set proxy for selenium: %s', self.proxy)
        else:
            msg = "Your system doesn't support settings of proxy by Taurus way, " \
                  "please set HTTP and HTTPS proxy to %s manually" % self.proxy
            self.log.warning(msg)

        for executor in self.engine.provisioning.executors:
            if isinstance(executor, AbstractSeleniumExecutor):
                if executor.label:
                    labels.append(executor.label)
                executor.add_env({'http_proxy': self.proxy,  # set vars anyway for case
                                  'https_proxy': self.proxy,  # linux system can't say correct name
                                  'HTTP_PROXY': self.proxy,
                                  'HTTPS_PROXY': self.proxy,
                                  "CHROMIUM_USER_FLAGS": "--proxy-server=%s" % self.proxy,  # for Linux chrome
                                  'XDG_CURRENT_DESKTOP': None,  # (it might be in Docker, etc.)
                                  'DESKTOP_SESSION': None,
                                  'GNOME_DESKTOP_SESSION_ID': None,
                                  'KDE_FULL_SESSION': None})
        if len(labels) == 1:
            self.label += '_' + labels[0]

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
        if 'HTTPSampler' not in req.content:
            self.log.warning("There aren't requests recorded by proxy2jmx, check your proxy configuration")
