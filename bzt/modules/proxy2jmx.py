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

import os
import shutil
from os.path import join, isfile

from bzt import TaurusConfigError, TaurusInternalException
from bzt.bza import BZAProxy
from bzt.engine import Service, Singletone
from bzt.modules import SubprocessedExecutor
from bzt.modules.selenium import AbstractSeleniumExecutor
from bzt.utils import is_windows, is_linux, get_full_path


class Proxy2JMX(Service, Singletone):
    def __init__(self):
        super(Proxy2JMX, self).__init__()
        self.proxy_addr = None
        self.proxy = BZAProxy()
        self.headers = {}
        self.label = 'generated'

    def prepare(self):
        super(Proxy2JMX, self).prepare()
        token = self.settings.get('token')
        if not token:
            token = self.engine.config.get('modules').get('blazemeter').get('token')

        if not token:
            msg = "You must provide your API token in settings of " \
                  "'proxy2jmx' or 'blazemeter' modules to use Proxy Recorder"
            raise TaurusConfigError(msg)
        self.proxy.token = token

        bm_api_addr = self.engine.config.get('modules').get('blazemeter').get('address', self.proxy.address)
        api_addr = self.settings.get('address', bm_api_addr)
        self.proxy.address = api_addr

        # todo: handle network exceptions (ssl, ...) in next call
        self.proxy_addr = self.proxy.get_addr()

    def startup(self):
        super(Proxy2JMX, self).startup()
        self.log.info('Starting BlazeMeter recorder...')

        labels = []
        additional_env = {}
        if is_linux():
            self.log.info('Set proxy for selenium: %s', self.proxy_addr)
            additional_env.update({'http_proxy': self.proxy_addr,  # set vars anyway for case
                                   'https_proxy': self.proxy_addr,  # linux system can't say correct name
                                   'HTTP_PROXY': self.proxy_addr,
                                   'HTTPS_PROXY': self.proxy_addr,
                                   "CHROMIUM_USER_FLAGS": "--proxy-server=%s" % self.proxy_addr,  # for Linux chrome
                                   'XDG_CURRENT_DESKTOP': None,  # (it might be in Docker, etc.)
                                   'DESKTOP_SESSION': None,
                                   'GNOME_DESKTOP_SESSION_ID': None,
                                   'KDE_FULL_SESSION': None})
        elif is_windows():
            self.log.info('Prepare chrome loader')
            chrome_path = self._get_chrome_path()
            if chrome_path:
                self._prepare_chrome_loader()
                new_path = join(self.engine.artifacts_dir, 'chrome-loader') + os.pathsep + os.getenv('PATH', '')
                additional_env.update({
                    'path_to_chrome': chrome_path,
                    'additional_chrome_params': '--proxy-server="%s"' % self.proxy_addr,
                    'chrome_loader_log': self.engine.create_artifact('chrome-loader', '.log'),
                    'path': new_path
                })
            else:
                self.log.warning('Chrome not found')

        else:  # probably we are in MacOS
            self.log.warning("Your system doesn't support settings of proxy by Taurus way")

        for executor in self.engine.provisioning.executors:
            if isinstance(executor, AbstractSeleniumExecutor):
                if executor.label:
                    labels.append(executor.label)
                executor.env.set(additional_env)

            if isinstance(executor, SubprocessedExecutor):
                if executor.label:
                    labels.append(executor.label)
                executor.env.set(additional_env)

        if len(labels) == 1:
            self.label += '_' + labels[0]

        self.proxy.start()

    @staticmethod
    def _get_chrome_path():
        # logic from chromedriver for determination of path to real Chrome
        chrome_path = None

        steps1 = (os.getenv('LOCALAPPDATA', ''),  # DIR_LOCAL_APP_DATA
                  os.getenv('PROGRAMFILES', ''),  # DIR_PROGRAM_FILES
                  os.getenv('PROGRAMFILES(X86)', ''))  # DIR_PROGRAM_FILESX86
        steps2 = ('Google\\Chrome\\Application\\', 'Chromium\\Application')
        for step1 in steps1:
            for step2 in steps2:
                path = join(step1, step2, 'chrome.exe')
                if isfile(path):
                    if not chrome_path:  # the path found first should be chosen.
                        chrome_path = path

        return chrome_path

    def _prepare_chrome_loader(self):
        loader_dir = join(self.engine.artifacts_dir, 'chrome-loader')
        os.mkdir(loader_dir)

        # find chromedriver.exe and copy it into artifacts/chrome-loader
        for _dir in os.getenv('PATH').split(os.pathsep):
            path = join(_dir, 'chromedriver.exe')
            if isfile(path):
                if path.lower().startswith(os.getenv('WINDIR')):
                    msg = 'Wrong chromedriver location: %s, look at ' % path
                    msg += 'http://gettaurus.org/docs/Proxy2JMX/#Microsoft-Windows for help'
                    self.log.warning(msg)
                shutil.copy2(path, loader_dir)
                break
        else:
            self.log.warning('cromedriver.exe not found in directories described in PATH')
            return

        # copy chrome-loader.exe from resources into artifacts/chrome-loader/chrome.exe
        old_file = join(get_full_path(__file__, step_up=2), 'resources', 'chrome-loader.exe')
        new_file = join(loader_dir, 'chrome.exe')
        try:
            shutil.copy2(old_file, new_file)
        except IOError as exc:
            raise TaurusInternalException("Can't copy loader: %s" % exc)

    def shutdown(self):
        super(Proxy2JMX, self).shutdown()
        self.log.info("Stopping BlazeMeter recorder...")
        self.proxy.stop()

    def post_process(self):
        super(Proxy2JMX, self).post_process()
        if self.engine.stopping_reason and not isinstance(self.engine.stopping_reason, KeyboardInterrupt):
            self.log.info("Will not pick converted JMX due to exception: %s", self.engine.stopping_reason)
            return

        # self.log.info("Downloading JSON...")
        # jmx_text = self.proxy.get_json()
        # jmx_file = self.engine.create_artifact(self.label, '.recording.json')
        # with open(jmx_file, 'w') as _file:
        #    _file.writelines(jmx_text)
        # self.log.info("JSON saved into %s", jmx_file)

        self.log.info("Downloading simple JMX...")
        jmx_text = self.proxy.get_jmx()
        jmx_file = self.engine.create_artifact(self.label, '.simple.jmx')
        with open(jmx_file, 'w') as _file:
            _file.writelines(jmx_text)
        self.log.info("Simple JMX saved into %s", jmx_file)

        self.log.info("Waiting for proxy to generate SmartJMX...")
        jmx_text = self.proxy.get_smart_jmx()
        jmx_file = self.engine.create_artifact(self.label, '.smart.jmx')
        with open(jmx_file, 'w') as _file:
            _file.writelines(jmx_text)
        self.log.info("Smart JMX saved into %s", jmx_file)

        if 'HTTPSampler' not in jmx_text:
            self.log.warning("There aren't requests recorded by proxy2jmx, check your proxy configuration")

        # log of chrome-loader not found under windows
        if is_windows() and not os.path.isfile(join(self.engine.artifacts_dir, 'chrome-loader.log')):
            msg = "Problems with chrome tuning are encountered, "
            msg += "take look at http://http://gettaurus.org/docs/Proxy2JMX/ for help"
            self.log.warning(msg)


def inject_loader(install_dir):
    """ move loader from installation dir to bzt resources
    (typically C:\Program Files\Taurus\chrome-loader.exe ->
    C:\Program Files\Common Files\Python\2.7\lib\site-packages\bzt\resources\chrome-loader.exe) """
    res_dir = join(get_full_path(__file__, step_up=2), "resources")
    loader = 'chrome-loader.exe'
    try:
        if not isfile(join(res_dir, loader)):
            print('Move %s from "%s" to "%s"...' % (loader, install_dir, res_dir))
            shutil.move(join(install_dir, loader), res_dir)
            print('Movement done')
    except:
        print('Movement failed')
        raise


def remove_loader():
    """ remove loader from bzt resources for correct bzt removal "
    (typically C:\Program Files\Common Files\Python\2.7\lib\site-packages\bzt\resources\chrome-loader.exe) """
    res_dir = join(get_full_path(__file__, step_up=2), "resources")
    loader = 'chrome-loader.exe'
    try:
        if isfile(join(res_dir, loader)):
            print('Remove %s from "%s"...' % (loader, res_dir))
            os.remove(join(res_dir, loader))
            print('Removal done')
    except:
        print('Removal failed')
        raise
