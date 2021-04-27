"""
Module for reporting into http://www.blazemeter.com/ service

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
import re
import time
import traceback
from collections import namedtuple
from functools import wraps
from ssl import SSLError
from urllib.error import URLError

from requests.exceptions import ReadTimeout

from bzt import TaurusNetworkError

NETWORK_PROBLEMS = (IOError, URLError, SSLError, ReadTimeout, TaurusNetworkError)


def send_with_retry(method):
    @wraps(method)
    def _impl(self, *args, **kwargs):
        try:
            method(self, *args, **kwargs)
        except (IOError, TaurusNetworkError):
            self.log.debug("Error sending data: %s", traceback.format_exc())
            self.log.warning("Failed to send data, will retry in %s sec...", self._user.timeout)
            try:
                time.sleep(self._user.timeout)
                method(self, *args, **kwargs)
                self.log.info("Succeeded with retry")
            except NETWORK_PROBLEMS:
                self.log.error("Fatal error sending data: %s", traceback.format_exc())
                self.log.warning("Will skip failed data and continue running")

    return _impl


def get_with_retry(method):
    @wraps(method)
    def _impl(self, *args, **kwargs):
        while True:
            try:
                return method(self, *args, **kwargs)
            except NETWORK_PROBLEMS:
                self.log.debug("Error making request: %s", traceback.format_exc())
                self.log.warning("Failed to make request, will retry in %s sec...", self.user.timeout)
                time.sleep(self.user.timeout)

    return _impl


def parse_blazemeter_test_link(link):
    """
    https://a.blazemeter.com/app/#/accounts/97961/workspaces/89846/projects/229969/tests/5823512

    :param link:
    :return:
    """
    if not isinstance(link, str):
        return None

    regex = r'https://a.blazemeter.com/app/#/accounts/(\d+)/workspaces/(\d+)/projects/(\d+)/tests/(\d+)(?:/\w+)?'
    match = re.match(regex, link)
    if match is None:
        return None

    TestParams = namedtuple('TestParams', 'account_id,workspace_id,project_id,test_id')
    return TestParams(*[int(x) for x in match.groups()])
