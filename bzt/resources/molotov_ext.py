"""
Molotov plugin for recording test results for Taurus.

Copyright 2017 BlazeMeter Inc.

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

import molotov


report_file = open(os.environ["MOLOTOV_TAURUS_REPORT"], 'wb')


# @molotov.events()
# async def print_request(event, **info):
#     print('print_request', event, info)
#     if event == 'sending_request':
#         print("=>")


@molotov.events()
async def print_response(event, **info):
    if event == 'response_received':
        import pudb; pudb.set_trace()
        print('<=')

