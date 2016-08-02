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
import os

import requests

from bzt.six import request as http_request
from bzt.engine import Service
from bzt.utils import RequiredTool, is_windows, shell_exec, BetterDict
from bzt.modules.selenium import SeleniumExecutor


class Proxy2JMX(Service):
    CERT_DOWNLOAD_LINK = "http://bz/cert/"

    def __init__(self):
        super(Proxy2JMX, self).__init__()
        self.proxy = None
        self.headers = {}
        self.api_delay = 5
        self.certutil = None
        self.certificates = ['pem', 'p12']
        self.address = 'https://a.blazemeter.com/api/latest/mitmproxies'

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

    def __get_certificates(self):
        downloader = http_request.FancyURLopener(proxies={"http":  self.proxy})

        for cert in self.certificates:
            link = Proxy2JMX.CERT_DOWNLOAD_LINK + cert
            _file = self.engine.create_artifact('mitmproxy-ca-cert', '.'+cert)
            try:
                self.log.info("Downloading %s from %s", cert, Proxy2JMX.CERT_DOWNLOAD_LINK+cert)
                downloader.retrieve(link, _file)
            except BaseException:
                self.log.error("Error while downloading %s", cert)
                raise

    def __certutul(self, nss_dir, cert_file, cert_nick):
        pass
        # TODO: check NSS db

        # add cert if not found
        params = [self.certutil.tool_path, '-d', nss_dir, '-A', '-t', 'TC', '-i', cert_file, '-n', cert_nick]
        subprocess = shell_exec(params)
        output = subprocess.communicate()
        self.log.debug("%s output: %s", self.certutil.tool_name, output)
        if subprocess.returncode == 0:
            self.log.debug('Installation %s into %s succeeded' % (cert_nick, nss_dir))
        else:
            self.log.debug('Installation %s into %s failed' % (cert_nick, nss_dir))

        # TODO: remove if it was added by taurus

    def __chrome_setup(self, home_dir):
        self.log.debug('Chrome certificate setup')
        if not is_windows():
            nssdb_path = os.path.join(home_dir, '.pki', 'nssdb')
            cert_file = os.path.join(self.engine.artifacts_dir, 'mitmproxy-ca-cert.pem')
            self.__certutul(nssdb_path, cert_file, 'mitmproxy')

    def __firefox_setup(self, home_dir):
        self.log.debug('Firefox certificate setup')
        if not is_windows():
            if not home_dir:
                self.log.debug('Home directory not found')
                return
            if '.mozilla' not in os.listdir(home_dir):
                self.log.debug('Mozilla directory not found')
                return
            if 'firefox' not in os.listdir(os.path.join(home_dir, '.mozilla')):
                self.log.debug('Firefox directory not found')
                return
            for _file in os.listdir(os.path.join(home_dir, '.mozilla', 'firefox')):
                if '.default' in _file:
                    break
            else:
                self.log.debug('Firefox profile not found')
                return
            nssdb_path = os.path.join(home_dir, '.mozilla', 'firefox', _file)
            cert_file = os.path.join(self.engine.artifacts_dir, 'mitmproxy-ca-cert.pem')
            self.__certutul(nssdb_path, cert_file, 'mitmproxy')

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

        self.certutil = Certutil(self.log)
        if not self.certutil.check_if_installed():
            self.log.info('Certutil not found, certificate setup is impossible')
            return

        self.__get_certificates()
        environ = BetterDict()
        environ.merge(dict(os.environ))
        self.__chrome_setup(environ.get('HOME'))
        self.__firefox_setup(environ.get('HOME'))

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
        self.log.info("Stopping BlazeMeter recorder...")
        self.api_request('/stopRecording', 'POST')

    def post_process(self):
        super(Proxy2JMX, self).post_process()
        self.log.info("Waiting for proxy to generate JMX...")
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


class Certutil(RequiredTool):
    def __init__(self, parent_logger):
        super(Certutil, self).__init__("Certutil", "")
        self.tool_path = 'certutil'
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        self.log.debug('Checking Certutil: %s' % self.tool_path)
        try:
            shell_exec([self.tool_path, '-h'])
        except OSError:
            return False
        return True
