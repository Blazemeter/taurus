"""
Module for reporting into http://loadosophia.org/ service
"""
import cookielib
import gzip
import json
import os
import urllib
import urllib2
import StringIO
import time
import datetime

from bzt.modules import Reporter, AggregatorListener
from bzt.modules.jmeter import JMeterExecutor
from bzt.modules.aggregator import DataPoint, KPISet
from bzt.utils import MultiPartForm


class Loadosophia(Reporter, AggregatorListener):
    """
    Reporter class
    """

    def __init__(self):
        super(Loadosophia, self).__init__()
        self.client = LoadosophiaClient(self.log)
        self.project = None
        self.title = None
        self.color = None

    def prepare(self):
        """
        Read options for uploading, check that they're sane
        """
        super(Loadosophia, self).prepare()
        self.client.address = self.settings.get("address", "https://loadosophia.org/")
        token = self.settings.get("token", "")
        if not token:
            msg = "Loadosophia.org uploading disabled, "
            msg += "please set token option to enable it, "
            msg += "get token at https://loadosophia.org/gui/settings/"
            self.log.warning(msg)
        self.client.token = token

        if not self.client.address:
            msg = "Loadosophia.org uploading disabled, "
            msg += "please set address option to enable it"
            self.log.warning(msg)

        self.project = self.parameters.get("project", "")
        self.title = self.parameters.get("name", "")
        self.color = self.parameters.get("color", "")

    def startup(self):
        """
        Initiate online test
        """
        super(Loadosophia, self).startup()
        try:
            url = self.client.start_online(self.project, self.title)
            self.log.info("Started online test: %s", url)
        except KeyboardInterrupt:
            raise
        except BaseException, exc:
            self.log.warn("Failed to start online: %s", exc)

    def post_process(self):
        """
        Upload results if possible
        """
        super(Loadosophia, self).post_process()
        if not self.project:
            msg = "Uploading to default project, "
            msg += "please set project option to change this"
            self.log.info(msg)

        try:
            self.client.end_online()
        except KeyboardInterrupt:
            raise
        except BaseException, exc:
            self.log.warn("Failed to finish online: %s", exc)

        for executor in self.engine.provisioning.executors:
            if isinstance(executor, JMeterExecutor):
                queue_id = self.client.send_results(self.project, executor.kpi_jtl)

                if queue_id and (self.title or self.color):
                    test_id = self.client.get_test_by_upload(queue_id)
                    if self.color:
                        self.client.set_color_flag(test_id, self.color)
                    if self.title:
                        self.client.set_test_title(test_id, self.title)

                if queue_id:
                    self.log.info("Loadosophia.org upload succeeded, report link: %s", self.client.results_url)

    def aggregated_second(self, data):
        """
        Send online data
        :param data: DataPoint
        :return:
        """
        # TODO: make it send by 5 sec bunches or configurable
        if self.client.online_started:
            self.client.send_online_data([data])


class LoadosophiaClient(object):
    """ Loadosophia service client class """

    STATUS_DONE = 4

    def __init__(self, parent_logger):
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.token = None
        self.address = None
        self.file_prefix = ''
        self.results_url = None
        self.cookie_jar = cookielib.CookieJar()
        self.online_started = False

    def _get_request(self, url):
        return urllib2.Request(url)

    def _get_response(self, request):
        return urllib2.urlopen(request)

    def _get_opener_response(self, data, url):
        opener = urllib2.build_opener(urllib2.HTTPCookieProcessor(self.cookie_jar))
        response = opener.open(url, data)
        return response

    def send_results(self, project, result_file, monitoring_files=()):
        """ Send files to loadosophia
        :type monitoring_files: tuple
        :type result_file: str
        :type project: str
        """
        if not self.token:
            self.log.debug("Skip send results")
        else:
            self.log.info("Uploading to Loadosophia.org: %s %s %s", project, result_file, monitoring_files)
            if not result_file or not os.path.exists(result_file) or not os.path.getsize(result_file):
                msg = "Empty results file, skip Loadosophia.org uploading: %s"
                self.log.warning(msg, result_file)
            else:
                return self.__send_checked_results(project, result_file, monitoring_files)

    def __send_checked_results(self, project, result_file, monitoring_files):
        """ internal wrapper to send request """
        # Create the form with simple fields
        form = MultiPartForm()
        form.add_field('projectKey', project)
        form.add_field('token', self.token)

        # Add main file
        form.add_file_as_string('jtl_file', self.file_prefix + os.path.basename(result_file) + ".gz",
                                self.__get_gzipped_file(result_file))

        index = 0
        for mon_file in monitoring_files:
            if not mon_file or not os.path.exists(mon_file) or not os.path.getsize(mon_file):
                self.log.warning("Skipped mon file: %s", mon_file)
                continue
            form.add_file_as_string('perfmon_' + str(index),
                                    self.file_prefix + os.path.basename(mon_file) + ".gz",
                                    self.__get_gzipped_file(mon_file))
            index += 1

        # Build the request
        url = self.address + "api/file/upload/?format=json"
        self.log.debug("POST %s", url)
        request = self._get_request(url)
        request.add_header('User-Agent', 'Loadosophia Uploader Module')
        body = str(form)
        request.add_header('Content-Type', form.get_content_type())
        request.add_header('Content-Length', len(body))
        request.add_data(body)

        response = self._get_response(request)
        if response.getcode() != 200:
            self.log.debug("Full loadosophia.org response: %s", response.read())
            msg = "Loadosophia.org upload failed, response code %s "
            msg += "instead of 200, see log for full response text"
            raise RuntimeError(msg % response.getcode())

        resp_str = response.read()
        try:
            res = json.loads(resp_str)
        except Exception, exc:
            self.log.debug("Failed to load json from str: %s", resp_str)
            raise exc
        self.results_url = self.address + 'api/file/status/' + res[0][
            'QueueID'] + '/?redirect=true'
        return res[0]['QueueID']

    @staticmethod
    def __get_gzipped_file(result_file):
        """ gzip file """
        out = StringIO.StringIO()
        fhandle = gzip.GzipFile(fileobj=out, mode='w')
        fhandle.write(open(result_file).read())
        fhandle.close()
        return out.getvalue()

    def get_test_by_upload(self, queue_id):
        """

        :param queue_id:
        :return: :raise HTTPError:
        """
        self.log.info("Waiting for Loadosophia.org to process file...")

        while True:
            time.sleep(1)
            status = self.get_upload_status(queue_id)
            if status['UserError']:
                raise RuntimeError(
                    "Loadosophia processing error: " + status['UserError'])

            if int(status['status']) == self.STATUS_DONE:
                self.results_url = self.address + 'gui/' + status[
                    'TestID'] + '/'
                return status['TestID']

    def get_upload_status(self, queue_id):
        """
        Returns the status for uploaded file

        :type queue_id: str
        :raise RuntimeError:
        """
        self.log.debug("Requesting file status: %s", queue_id)
        form = MultiPartForm()
        form.add_field('token', self.token)

        url = self.address + "api/file/status/" + queue_id + "/?format=json"
        self.log.debug("GET %s", url)
        request = self._get_request(url)
        request.add_header('User-Agent', 'Loadosophia Uploader Module')
        body = str(form)
        request.add_header('Content-Type', form.get_content_type())
        request.add_header('Content-Length', len(body))
        request.add_data(body)

        response = self._get_response(request)
        if response.getcode() != 200:
            self.log.debug("Full loadosophia.org response: %s", response.read())
            msg = "Loadosophia.org request failed, response code %s "
            msg += "instead of 200, see log for full response text"
            raise RuntimeError(msg % response.getcode())

        res = json.loads(response.read())
        self.log.debug("Status info: %s", res)
        return res[0]

    def set_color_flag(self, test_id, color):
        """
        Sets color flag for processed test

        :param test_id:
        :param color:
        :raise RuntimeError:
        """
        form = MultiPartForm()
        form.add_field('token', self.token)

        url = self.address + "api/test/edit/color/" + test_id
        url += "/?format=json&color=" + color
        self.log.debug("POST %s", url)
        request = self._get_request(url)
        request.add_header('User-Agent', 'Loadosophia Uploader Module')
        body = str(form)
        request.add_header('Content-Type', form.get_content_type())
        request.add_header('Content-Length', len(body))
        request.add_data(body)

        response = self._get_response(request)
        if response.getcode() != 204:
            self.log.debug("Full loadosophia.org response: %s", response.read())
            msg = "Loadosophia.org request failed, response code %s"
            msg += " instead of 204, see log for full response text"
            raise RuntimeError(msg % response.getcode())

    def set_test_title(self, test_id, title):
        """
        Set test title for processed test

        :param test_id:
        :param title:
        :raise RuntimeError:
        """
        self.log.debug("Set test title: %s", title)
        form = MultiPartForm()
        form.add_field('token', self.token)

        url = self.address + "api/test/edit/title/" + test_id + "/?format=json&"
        url += urllib.urlencode({"title": title})
        self.log.debug("POST %s", url)
        request = self._get_request(url)
        request.add_header('User-Agent', 'Loadosophia Uploader Module')
        body = str(form)
        request.add_header('Content-Type', form.get_content_type())
        request.add_header('Content-Length', len(body))
        request.add_data(body)

        response = self._get_response(request)
        if response.getcode() != 204:
            self.log.debug("Full loadosophia.org response: %s", response.read())
            msg = "Loadosophia.org request failed, response code %s "
            msg += "instead of 204, see log for full response text"
            raise RuntimeError(msg % response.getcode())

    def start_online(self, project, title=None):
        """
        Start online test

        :type project: str
        :type title: str
        :return:
        """
        self.log.info("Initiating Loadosophia.org active test...")
        data = urllib.urlencode({'projectKey': project, 'token': self.token, 'title': title})

        url = self.address + "api/active/receiver/start/"
        self.log.debug("GET %s", url)

        response = self._get_opener_response(data, url)
        if response.getcode() != 201:
            self.log.warn("Failed to start active test: %s", response.getcode())
            self.log.debug("Failed to start active test: %s", response.read())
            self.cookie_jar.clear_session_cookies()

        online_id = json.loads(response.read())
        self.online_started = True
        return self.address + "gui/active/" + online_id['OnlineID'] + '/'

    def end_online(self):
        """
        Finish online test
        """
        if not self.online_started:
            self.log.debug("Online not started, so not stopping")
        else:
            self.log.debug("Ending Loadosophia online test")
            url = self.address + "api/active/receiver/stop/"
            self.log.debug("Request: %s", url)
            response = self._get_opener_response(None, url)
            if response.getcode() != 205:
                self.log.warn("Failed to end active test: %s", response.getcode())
                self.log.debug("Failed to end active test: %s", response.read())
        self.cookie_jar.clear_session_cookies()

    def send_online_data(self, data_buffer):
        """
        Sends online data

        :type data_buffer: list[bzt.modules.aggregator.DataPoint]
        """
        data = []
        for sec in data_buffer:
            item = sec[DataPoint.CURRENT]['']
            perc = item[KPISet.PERCENTILES]
            for key, val in item[KPISet.PERCENTILES].iteritems():
                item[KPISet.PERCENTILES][key] = int(round(1000 * val))
            json_item = {
                "ts": str(datetime.datetime.fromtimestamp(sec[DataPoint.TIMESTAMP])),
                "threads": item[KPISet.CONCURRENCY],
                "rps": item[KPISet.SAMPLE_COUNT],
                "planned_rps": None,
                "avg_rt": int(round(1000 * item[KPISet.AVG_RESP_TIME])),
                "quantiles": perc,
                "rc": item[KPISet.RESP_CODES],
                "net": item[KPISet.ERRORS]
            }
            data.append(json_item)

        self.log.debug("Sending online data: %s", json.dumps(data))
        data_str = urllib.urlencode({'data': json.dumps(data)})

        url = self.address + "api/active/receiver/data/"
        self.log.debug("POST %s", url)
        response = self._get_opener_response(data_str, url)
        if response.getcode() != 202:
            self.log.warn("Failed to push data: %s", response.getcode())
