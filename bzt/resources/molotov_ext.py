"""
Molotov plugin for recording test results for Taurus. Python 3.5+.

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
import json
import os
import time

import molotov


report_file = open(os.environ["MOLOTOV_TAURUS_REPORT"], 'w')
samples = dict()
scenarios = dict()


def write_report_item(item):
    report_file.write(json.dumps(item) + "\n")
    report_file.flush()


@molotov.events()
async def handle_request(event, **info):
    if event == 'sending_request':
        request = info['request']
        samples[id(request)] = {
            "ts": time.time(),
            "type": "request",
            "label": str(request.url),
            "elapsed": None,
            "responseCode": None,
            "responseMessage": None,
        }
    elif event == 'response_received':
        response = info['response']
        request = info['request']
        if id(request) in samples:
            sample = samples.pop(id(request))
            sample["elapsed"] = round(time.time() - sample["ts"], 3)
            sample["responseCode"] = str(response.status)
            sample["responseMessage"] = response.reason
            write_report_item(sample)
        else:
            print("WARNING: unmatched response and request: %s %s" % (response, request))


@molotov.events()
async def print_concurrency(event, **info):
    if event == 'current_workers':
        workers = info['workers']
        write_report_item({"ts": time.time(), "type": "workers", "value": workers})


@molotov.events()
async def print_test_case(event, **info):
    if event == 'scenario_start':
        scenario = info['scenario']
        ident = (info['wid'], scenario['name'])
        scenarios[ident] = {"ts": time.time(), "wid": "worker-" + str(info['wid'])}
    elif event == 'scenario_success':
        scenario = info['scenario']
        ident = (info['wid'], scenario['name'])
        if ident not in scenarios:
            print("WARNING: unmatched scenario %s:%s" % (info['wid'], scenario['name']))
            return
        sample = scenarios.pop(ident)
        sample.update({
            "type": "scenario_success",
            "name": scenario['name'],
            "duration": round(time.time() - sample["ts"], 3),
        })
        write_report_item(sample)
    elif event == 'scenario_failure':
        scenario = info['scenario']
        exception = info['exception']
        ident = (info['wid'], scenario['name'])
        if ident not in scenarios:
            print("WARNING: unmatched scenario %s:%s" % (info['wid'], scenario['name']))
            return
        sample = scenarios.pop(ident)
        sample.update({
            "type": "scenario_failure",
            "name": scenario['name'],
            "duration": round(time.time() - sample["ts"], 3),
            "exception": exception.__class__.__name__,
            "errorMessage": str(exception),
        })
        write_report_item(sample)


@molotov.global_teardown()
def close_file():
    if not report_file.closed:
        report_file.close()
