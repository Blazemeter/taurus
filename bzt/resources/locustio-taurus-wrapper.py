#! /usr/bin/env python2
import csv
import os
import time
from requests.exceptions import HTTPError

from locust import main, events, runners

fname = os.environ.get("JTL")
if not fname:
    raise ValueError("Please specify JTL environment variable")


def getrec(request_type, name, response_time, response_length, exc=None):
    rc = '200' if exc is None else '500'
    rm = 'OK' if exc is None else '%s' % exc
    if isinstance(exc, HTTPError):
        rc = exc.message[:exc.message.index(' ')]
        rm = exc.message[exc.message.index(':') + 2:]

    return {
        'timeStamp': "%d" % (time.time() * 1000),
        'label': name,
        'method': request_type,
        'elapsed': response_time,
        'bytes': response_length,  # NOTE: not sure if the field name is right
        'responseCode': rc,
        'responseMessage': rm,
        'success': 'true' if exc is None else 'false',

        # NOTE: might be resource-consuming
        "allThreads": runners.locust_runner.user_count if runners.locust_runner else 0,
        "Latency": 0
    }


if __name__ == '__main__':
    with open(fname, 'wt') as fhd:
        writer = csv.DictWriter(fhd, getrec(None, None, None, None).keys())
        writer.writeheader()
        fhd.flush()


        def on_request_success(request_type, name, response_time, response_length):
            writer.writerow(getrec(request_type, name, response_time, response_length))
            fhd.flush()


        def on_request_failure(request_type, name, response_time, exception):
            writer.writerow(getrec(request_type, name, response_time, 0, exception))
            fhd.flush()


        events.request_success += on_request_success
        events.request_failure += on_request_failure

        main.main()
        fhd.flush()
