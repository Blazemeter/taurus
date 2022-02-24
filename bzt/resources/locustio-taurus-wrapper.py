#! /usr/bin/env python
import csv
import json
import os
import sys
import time
from collections import OrderedDict

from locust import main, events
from locust.exception import StopUser
from requests.exceptions import HTTPError

from bzt.utils import guess_csv_dialect


class LocustStarter(object):
    def __init__(self):
        super(LocustStarter, self).__init__()
        self.fhd = None
        self.writer = None
        self.runner = None
        self.locust_start_time = None
        self.locust_stop_time = None

        self.locust_duration = sys.maxsize
        self.num_requests = sys.maxsize

        if os.getenv("LOCUST_DURATION"):
            self.locust_duration = float(os.getenv("LOCUST_DURATION"))

        if os.getenv("LOCUST_NUMREQUESTS"):
            self.num_requests = float(os.getenv("LOCUST_NUMREQUESTS"))

    def __check_limits(self):
        if self.locust_start_time is None:
            self.locust_start_time = time.time()

        # Only raise an exception if the actual test is running
        if self.locust_stop_time is None:
            if time.time() - self.locust_start_time >= self.locust_duration:
                raise StopUser('Duration limit reached')

            if self.num_requests <= 0:
                raise StopUser('Request limit reached')

    def __getrec(self, request_type, name, response_time, response_length, exc=None):
        rcode = '200' if exc is None else '500'
        rmsg = 'OK' if exc is None else '%s' % exc
        if isinstance(exc, HTTPError):
            exc_message = str(exc)
            rcode = exc_message[:exc_message.index(' ')]
            rmsg = exc_message[exc_message.index(':') + 2:]
        if isinstance(response_time, float):
            response_time = int(round(response_time))

        return OrderedDict([
            ('allThreads', self.runner.user_count if self.runner else 0),
            ('timeStamp', "%d" % (time.time() * 1000)),
            ('label', name),
            ('method', request_type),
            ('elapsed', response_time),
            ('bytes', response_length),
            ('responseCode', rcode),
            ('responseMessage', rmsg),
            ('success', 'true' if exc is None else 'false'),
            ('Latency', 0),
        ])

    def __on_init(self, **args):
        if 'runner' in args:
            self.runner = args['runner']

    def __on_request_success(self, request_type, name, response_time, response_length, **args):
        self.num_requests -= 1
        self.writer.writerow(self.__getrec(request_type, name, response_time, response_length))
        self.fhd.flush()
        self.__check_limits()

    def __on_request_failure(self, request_type, name, response_time, exception, response_length=0, **args):
        self.num_requests -= 1
        self.writer.writerow(self.__getrec(request_type, name, response_time, response_length, exception))
        self.fhd.flush()
        self.__check_limits()

    def __on_exception(self, locust_instance, exception, tb, **args):
        del locust_instance, tb
        self.__on_request_failure('', '', 0, exception)

    def __on_worker_report(self, client_id, data, **args):
        if data['stats'] or data['errors']:
            for item in data['stats']:
                self.num_requests -= item['num_requests']

            data['client_id'] = client_id
            self.fhd.write("%s\n" % json.dumps(data))
            self.fhd.flush()
        self.__check_limits()

    def __on_quit(self, **args):
        self.locust_stop_time = time.time()

    def execute(self):
        events.init.add_listener(self.__on_init)
        events.user_error.add_listener(self.__on_exception)
        events.worker_report.add_listener(self.__on_worker_report)
        events.quitting.add_listener(self.__on_quit)

        if os.getenv("JTL"):    # regular locust worker
            fname = os.getenv("JTL")
            self.fhd = open(fname, 'wt')
            fieldnames = list(self.__getrec(None, None, None, None).keys())
            dialect = guess_csv_dialect(",".join(fieldnames))
            self.writer = csv.DictWriter(self.fhd, fieldnames=fieldnames, dialect=dialect)
            self.writer.writeheader()
            events.request.add_listener(self.__on_request_success)
            events.request.add_listener(self.__on_request_failure)
        elif os.getenv("WORKERS_LDJSON"):   # master of distributed mode
            fname = os.getenv("WORKERS_LDJSON")
            self.fhd = open(fname, 'wt')
            self.writer = None
        else:
            raise ValueError("Please specify JTL or WORKERS_LDJSON environment variable")

        main.main()

        self.fhd.close()


if __name__ == '__main__':
    locust_starter = LocustStarter()
    locust_starter.execute()
