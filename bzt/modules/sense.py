"""
Basics of reporting capabilities

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
import json
import os
import time

import requests

import bzt
from bzt.engine import Reporter
from bzt.modules.monitoring import Monitoring, MonitoringListener
from bzt.modules.aggregator import AggregatorListener, ResultsProvider
from bzt.six import viewvalues
from bzt.utils import open_browser


class BlazeMeterSenseReporter(Reporter, AggregatorListener, MonitoringListener):
    def __init__(self):
        super(BlazeMeterSenseReporter, self).__init__()
        self.results_file = None
        self.monitoring_file = None
        self.results_writer = None
        self.monitoring_writer = None
        self.sense = BlazeMeterSenseClient(self.log)
        self.project_key = None
        self.test_color = None
        self.test_title = None
        self.online_buffer = []
        self.online_enabled = False
        self.online_initiated = False

    def prepare(self):
        self.project_key = self.settings.get("project", "Taurus")
        self.test_title = self.settings.get("test-title", "Test")
        self.test_color = self.settings.get("test-color", "")
        self.online_enabled = self.settings.get("online-enabled", True)

        token = self.settings.get('token', '')
        if not token:
            raise ValueError('BlazeMeter Sense token is not specified')
        address = self.settings.get('address', 'https://sense.blazemeter.com/')

        self.sense.configure(token, address)

        if isinstance(self.engine.aggregator, ResultsProvider):
            self.engine.aggregator.add_listener(self)
        for service in self.engine.services:
            if isinstance(service, Monitoring):
                # what if there're many monitoring services?
                service.add_listener(self)

        self.results_file = self.engine.create_artifact('results', '.ldjson')
        self.monitoring_file = self.engine.create_artifact('monitoring', '.ldjson')

    def startup(self):
        self.results_writer = LDJSONWriter(self.results_file)
        self.monitoring_writer = LDJSONWriter(self.monitoring_file)
        if self.online_enabled:
            url = self.sense.start_online(self.project_key, self.test_title)
            self.online_initiated = True
            if url is not None and self.settings.get('browser-open', True):
                open_browser(url)

    def aggregated_second(self, data_point):
        self.results_writer.write(data_point)
        if self.online_enabled:
            self.online_buffer.append(data_point)
            if len(self.online_buffer) >= 5:
                try:
                    self.sense.send_online_data(self.online_buffer)
                except BaseException as exc:
                    self.log.warning("Couldn't send online data to Sense: %s", exc)
                self.online_buffer = []

    def monitoring_data(self, data_point):
        self.monitoring_writer.write(data_point)

    def shutdown(self):
        self.close_fds()
        redirect_link = self.send_test_results()
        if self.online_initiated:
            self.sense.end_online(redirect_link)

    def close_fds(self):
        if self.results_writer is not None:
            self.results_writer.close_fds()
        if self.monitoring_writer is not None:
            self.monitoring_writer.close_fds()

    def send_test_results(self):
        queue_id = self.sense.send_results(self.project_key, self.results_file, self.monitoring_file)
        if not queue_id:
            return
        if self.test_title or self.test_color:
            test_id = self.sense.get_test_by_upload(queue_id)
            if self.test_color:
                self.sense.set_color_flag(test_id, self.test_color)
            if self.test_title:
                self.sense.set_test_title(test_id, self.test_title)
            redirect_link = self.sense.address + "gui/" + test_id + "/"
        else:
            redirect_link = self.sense.address + "api/file/status/" + queue_id + "/?redirect=true"
        self.log.info("BlazeMeter Sense upload succeeded, view the report at %s", redirect_link)
        return redirect_link


class LDJSONWriter(object):
    def __init__(self, filename):
        self.filename = filename
        self.fds = None

    def open_fds(self):
        self.fds = open(self.filename, 'w')

    def write(self, data):
        if not self.fds:
            self.open_fds()
        self.fds.write("%s\n" % json.dumps(data))
        self.fds.flush()

    def close_fds(self):
        if self.fds:
            self.fds.close()
            self.fds = None

    def __del__(self):
        self.close_fds()


class BlazeMeterSenseClient(object):
    STATUS_DONE = 4

    def __init__(self, parent_logger):
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.token = None
        self.address = None
        self.session = requests.Session()
        self.session.headers.update({'User-Agent': 'Taurus %s Sense Uploader' % bzt.VERSION})

    def configure(self, token, address):
        self.token = token
        self.address = address

    def start_online(self, project, title):
        self.log.info("Starting BlazeMeter Sense test")
        data = {'projectKey': project, 'token': self.token, 'title': title}

        url = self.address + "api/active/receiver/start/"
        response = self.session.post(url, data=data)
        if response.status_code != 201:
            self.log.warn("Failed to start Sense test: %s", response.status_code)
            self.log.debug("Failed to start Sense test: %s", response.text)
            return None

        online_id = response.json()
        return self.address + "gui/active/" + online_id['OnlineID'] + '/'

    def end_online(self, redirect_link):
        self.log.debug("Ending Sense online test")
        url = self.address + "api/active/receiver/stop/"
        response = self.session.post(url, data={'redirect': redirect_link})
        if response.status_code != 205:
            self.log.warn("Failed to end Sense test: %s", response.status_code)
            self.log.debug("Failed to end Sense test: %s", response.text)

    def send_online_data(self, data_buffer):
        self.log.debug("Sending online data")
        payload = {'data': json.dumps(data_buffer)}
        url = self.address + "api/active/receiver/data/"
        response = self.session.post(url, data=payload)
        if response.status_code != 202:
            self.log.warn("Failed to send online data: %s", response.status_code)

    def get_test_by_upload(self, queue_id):
        self.log.info("Waiting for BlazeMeter Sense to process file...")

        while True:
            time.sleep(1)  # NOTE: really? we're blocking entire app here
            status = self.get_upload_status(queue_id)
            if status.get('UserError'):
                raise RuntimeWarning("Sense processing error: " + status['UserError'])

            if int(status['status']) == self.STATUS_DONE:
                return status['TestID']

    def get_upload_status(self, queue_id):
        self.log.debug("Requesting file status: %s", queue_id)

        url = self.address + "api/file/status/" + queue_id + "/"
        params = {'format': 'json'}
        form = {'token': self.token}

        response = self.session.post(url, params=params, data=form)
        if response.status_code != 200:
            self.log.debug("Upload status check failed: %s", response.text)
            raise RuntimeWarning("BlazeMeter Sense upload failed, response code %s" % response.status_code)

        res = response.json()
        self.log.debug("Upload status info: %s", res)
        return res[0]

    def set_color_flag(self, test_id, color):
        self.log.debug("Setting Sense color flag: %s", test_id)
        url = self.address + "api/test/edit/color/" + test_id + "/"
        params = {'format': 'json', 'color': color}
        form = {'token': self.token}

        response = self.session.post(url, params=params, data=form)
        if response.status_code != 204:
            self.log.debug("Full BlazeMeter Sense response: %s", response.text)
            raise RuntimeWarning("BlazeMeter Sense request failed, response code %s" % response.status_code)

    def set_test_title(self, test_id, title):
        self.log.debug("Setting Sense test title: %s", test_id)
        url = self.address + "api/test/edit/title/" + test_id + "/"
        params = {'format': 'json', 'title': title}
        form = {'token': self.token}

        response = self.session.post(url, params=params, data=form)
        if response.status_code != 204:
            self.log.debug("Full BlazeMeter Sense response: %s", response.text)
            raise RuntimeWarning("BlazeMeter Sense request failed, response code %s" % response.status_code)

    def send_results(self, project, result_file, monitoring_file):
        self.log.info("Uploading results to BlazeMeter Sense")

        if not project:
            self.log.info("Uploading to default project")

        if not result_file or not os.path.exists(result_file) or not os.path.getsize(result_file):
            self.log.warning("Empty results file, skipping BlazeMeter Sense uploading: %s", result_file)
            return None

        return self.__send_checked_results(project, result_file, monitoring_file)

    def __send_checked_results(self, project, result_file, perfmon_file):
        form = {
            'projectKey': project,
            'token': self.token,
        }
        params = {'format': 'json'}
        url = self.address + "api/file/upload/"

        files = {}
        try:
            files['jtl_file'] = open(result_file, 'rb')
            if not perfmon_file or not os.path.exists(perfmon_file) or not os.path.getsize(perfmon_file):
                self.log.warning("Monitoring file not exists, skipping")
            else:
                files['perfmon_0'] = open(perfmon_file, 'rb')
            response = self.session.post(url, params=params, data=form, files=files)
        finally:
            for fds in viewvalues(files):
                fds.close()

        if response.status_code != 200:
            self.log.debug("Full BlazeMeter Sense response: %s", response.text)
            raise RuntimeWarning("BlazeMeter Sense upload failed, response code %s" % response.status_code)

        res = response.json()
        queue_id = res[0]['QueueID']
        return queue_id


