#! /usr/bin/env python2
import csv
import json
import os
import time
from requests.exceptions import HTTPError
from locust import main, events, runners


def getrec(request_type, name, response_time, response_length, exc=None):
    rcode = '200' if exc is None else '500'
    rmsg = 'OK' if exc is None else '%s' % exc
    if isinstance(exc, HTTPError):
        rcode = exc.message[:exc.message.index(' ')]
        rmsg = exc.message[exc.message.index(':') + 2:]

    return {
        'timeStamp': "%d" % (time.time() * 1000),
        'label': name,
        'method': request_type,
        'elapsed': response_time,
        'bytes': response_length,  # NOTE: not sure if the field name is right
        'responseCode': rcode,
        'responseMessage': rmsg,
        'success': 'true' if exc is None else 'false',

        # NOTE: might be resource-consuming
        "allThreads": runners.locust_runner.user_count if runners.locust_runner else 0,
        "Latency": 0
    }


def execute():
    if os.getenv("SLAVES_LDJSON"):
        fname = os.getenv("SLAVES_LDJSON")
        is_csv = False
    elif os.getenv("JTL"):
        fname = os.getenv("JTL")
        is_csv = True
    else:
        raise ValueError("Please specify JTL or SLAVES_LDJSON environment variable")

    with open(fname, 'wt') as fhd:
        if is_csv:
            writer = csv.DictWriter(fhd, getrec(None, None, None, None).keys())
            writer.writeheader()
            fhd.flush()
        else:
            writer = None  # FIXME: bad code design, have zero object for it

        def on_request_success(request_type, name, response_time, response_length):
            writer.writerow(getrec(request_type, name, response_time, response_length))
            fhd.flush()

        def on_request_failure(request_type, name, response_time, exception):
            writer.writerow(getrec(request_type, name, response_time, 0, exception))
            fhd.flush()

        def on_slave_report(client_id, data):
            if data['stats'] or data['errors']:
                data['client_id'] = client_id
                fhd.write("%s\n" % json.dumps(data))
                fhd.flush()

        events.request_success += on_request_success
        events.request_failure += on_request_failure
        events.slave_report += on_slave_report

        main.main()
        fhd.flush()


if __name__ == '__main__':
    execute()
