#! /usr/bin/env python2
import csv
import json
import os
import time

from locust import main, events, runners
from locust.exception import StopLocust
from requests.exceptions import HTTPError


class LocustStarter(object):
    def __init__(self):
        super(LocustStarter, self).__init__()
        self.fhd = None
        self.writer = None
        if os.getenv("LOCUST_DURATION"):
            self.locust_start_time = time.time()
            self.locust_duration = float(os.getenv("LOCUST_DURATION"))
        else:
            self.locust_start_time = None
            self.locust_duration = None

    def __check_duration(self):
        if self.locust_duration is not None:
            if time.time() - self.locust_start_time >= self.locust_duration:
                raise StopLocust('Duration limit reached')

    @staticmethod
    def __getrec(request_type, name, response_time, response_length, exc=None):
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

    def __on_request_success(self, request_type, name, response_time, response_length):
        self.writer.writerow(self.__getrec(request_type, name, response_time, response_length))
        self.fhd.flush()
        self.__check_duration()

    def __on_request_failure(self, request_type, name, response_time, exception):
        self.writer.writerow(self.__getrec(request_type, name, response_time, 0, exception))
        self.fhd.flush()
        self.__check_duration()

    def __on_slave_report(self, client_id, data):
        if data['stats'] or data['errors']:
            data['client_id'] = client_id
            self.fhd.write("%s\n" % json.dumps(data))
            self.fhd.flush()

    def execute(self):
        if os.getenv("SLAVES_LDJSON"):
            fname = os.getenv("SLAVES_LDJSON")
            is_csv = False
        elif os.getenv("JTL"):
            fname = os.getenv("JTL")
            is_csv = True
        else:
            raise ValueError("Please specify JTL or SLAVES_LDJSON environment variable")

        with open(fname, 'wt') as self.fhd:
            if is_csv:
                self.writer = csv.DictWriter(self.fhd, self.__getrec(None, None, None, None).keys())
                self.writer.writeheader()
                self.fhd.flush()
            else:
                self.writer = None  # FIXME: bad code design, have zero object for it

            events.request_success += self.__on_request_success
            events.request_failure += self.__on_request_failure
            events.slave_report += self.__on_slave_report

            main.main()
            self.fhd.flush()


if __name__ == '__main__':
    locust_starter = LocustStarter()
    locust_starter.execute()
