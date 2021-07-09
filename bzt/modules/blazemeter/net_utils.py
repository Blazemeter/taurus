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
from collections import namedtuple
from ssl import SSLError
from urllib.error import URLError

from requests.exceptions import ReadTimeout

from bzt import TaurusNetworkError

NETWORK_PROBLEMS = (IOError, URLError, SSLError, ReadTimeout, TaurusNetworkError)


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
