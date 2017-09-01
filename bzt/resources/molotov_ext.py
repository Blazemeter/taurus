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
import csv
import os
import time

import molotov


report_file = open(os.environ["MOLOTOV_TAURUS_REPORT"], 'w')
csv_fields = ['timestamp', 'elapsed', 'label', 'responseCode', 'responseMessage']
report_writer = csv.DictWriter(report_file, fieldnames=csv_fields)
report_writer.writeheader()

samples = dict()


class Sample:
    def __init__(self, timestamp=None, elapsed=None, label=None, response_code=None, response_message=None):
        self.timestamp = timestamp
        self.elapsed = elapsed
        self.label = label
        self.response_code = response_code
        self.response_message = response_message

    def to_dict(self):
        return {
            'timestamp': self.timestamp,
            'elapsed': self.elapsed,
            'label': self.label,
            'responseCode': self.response_code,
            'responseMessage': self.response_message,
        }


@molotov.events()
async def print_request(event, **info):
    if event == 'sending_request':
        request = info['request']
        samples[id(request)] = Sample(timestamp=time.time(), label=str(request.url))


@molotov.events()
async def print_response(event, **info):
    if event == 'response_received':
        response = info['response']
        request = info['request']
        if id(request) in samples:
            sample = samples.pop(id(request))
            sample.elapsed = round(time.time() - sample.timestamp, 3)
            sample.timestamp = int(sample.timestamp * 100)
            sample.response_code = response.status
            sample.response_message = response.reason
            report_writer.writerow(sample.to_dict())
            report_file.flush()
        else:
            print("WARNING: unmatched response and request: %s %s" % (response, request))