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
import copy
import logging
import os
import sys
import time
import traceback
import zipfile
from collections import Counter
from time import sleep
from urllib.error import URLError

import requests
from terminaltables import SingleTable, AsciiTable
from urwid import Pile, Text

from bzt import AutomatedShutdown, TaurusConfigError, TaurusException, TaurusNetworkError, NormalShutdown
from bzt.bza import User, Session, BZA_TEST_DATA_RECEIVED, ENDED
from bzt.engine import Reporter, Provisioning, Configuration, Service, SETTINGS, ScenarioExecutor, EXEC
from bzt.modules.aggregator import DataPoint, KPISet, ResultsProvider
from bzt.modules.console import PrioritizedWidget
from bzt.modules.functional import FunctionalResultsReader, FunctionalSample
from bzt.modules.monitoring import LocalClient
from bzt.modules._selenium import SeleniumExecutor
from bzt.modules.services import Unpacker
from bzt.requests_model import has_variable_pattern
from bzt.utils import iteritems, open_browser, BetterDict, ExceptionalDownloader, ProgressBarContext
from bzt.utils import to_json, dehumanize_time, get_full_path, get_files_recursive, replace_in_config
from bzt.modules.blazemeter.blazemeter_reporter import BlazeMeterUploader
from bzt.modules.blazemeter.cloud_test import FUNC_API_TEST_TYPE, FUNC_GUI_TEST_TYPE, TAURUS_TEST_TYPE
from bzt.modules.blazemeter.project_finder import ProjectFinder
from bzt.modules.blazemeter.const import DEDICATED_IPS


CLOUD_CONFIG_BLACK_LIST = {
    "settings": {
        "artifacts-dir": True,
        "aggregator": True,
        "proxy": True,
        "check-updates": True
    },
    "included-configs": True,
    "cli": True,
    "cli-aliases": True,
    "install-id": True,
    "version": True,
    "modules": {
        "jmeter": {
            "path": True,
            "protocol-handlers": True
        },
        "ab": {
            "path": True
        },
        "gatling": {
            "path": True
        },
        "junit": {
            "path": True
        },
        "molotov": {
            "path": True
        },
        "siege": {
            "path": True
        },
        "testng": {
            "path": True
        },
        "tsung": {
            "path": True
        },
        "console": {
            "disable": True,
        },
        "blazemeter": {
            "address": True,
            "data-address": True,
            "token": True,
        },
        "cloud": {
            "address": True,
            "data-address": True,
            "token": True,
        },
    },
    "provisioning": True,

}


class MasterProvisioning(Provisioning):
    def get_rfiles(self):
        rfiles = []
        additional_files = []
        for executor in self.executors:
            executor_rfiles = executor.get_resource_files()
            config = to_json(self.engine.config.get('execution'))
            config += to_json(self.engine.config.get('scenarios'))
            config += to_json(executor.settings)
            for rfile in executor_rfiles:
                if has_variable_pattern(rfile):
                    continue

                if not os.path.exists(self.engine.find_file(rfile)):
                    raise TaurusConfigError("%s: resource file '%s' not found" % (executor, rfile))
                if to_json(rfile) not in config:
                    additional_files.append(rfile)
            rfiles += executor_rfiles

        if additional_files:
            raise TaurusConfigError("Following files can't be handled in cloud: %s" % additional_files)

        rfiles = list(set(rfiles))
        rfiles = [x for x in rfiles if not has_variable_pattern(x)]
        self.log.debug("All resource files are: %s", rfiles)
        return rfiles

    def _fix_filenames(self, old_names):
        # check for concurrent base names
        old_full_names = [self.engine.find_file(x) for x in old_names]
        rbases = [os.path.basename(get_full_path(rfile)) for rfile in old_full_names]
        rpaths = [get_full_path(rfile, step_up=1) for rfile in old_full_names]
        while rbases:
            base, path = rbases.pop(), rpaths.pop()
            if base in rbases:
                index = rbases.index(base)
                if path != rpaths[index]:
                    msg = 'Resource "%s" occurs more than one time, rename to avoid data loss'
                    raise TaurusConfigError(msg % base)

        old_full_names = self.__pack_dirs(old_full_names)
        new_base_names = [os.path.basename(f) for f in old_full_names]
        self.log.debug('Replace file names in config: %s with %s', old_names, new_base_names)
        replace_in_config(self.engine.config, old_names, new_base_names, log=self.log)
        old_full_names = list(set(old_full_names))
        return old_full_names

    def __pack_dirs(self, source_list):
        result_list = []  # files for upload
        packed_list = []  # files for unpacking

        for source in source_list:
            source = get_full_path(source)
            if os.path.isfile(source):
                result_list.append(source)
            else:  # source is dir
                self.log.debug("Compress directory '%s'", source)
                base_dir_name = os.path.basename(source)
                zip_name = self.engine.create_artifact(base_dir_name, '.zip')
                relative_prefix_len = len(os.path.dirname(source))
                with zipfile.ZipFile(zip_name, 'w') as zip_file:
                    for _file in get_files_recursive(source):
                        zip_file.write(_file, _file[relative_prefix_len:])
                result_list.append(zip_name)
                packed_list.append(base_dir_name + '.zip')

        if packed_list:
            services = self.engine.config.get(Service.SERV, [], force_set=True)
            unpacker = BetterDict.from_dict({'module': Unpacker.UNPACK, Unpacker.FILES: packed_list, 'run-at': 'local'})
            services.append(unpacker)

        return result_list


class CloudProvisioning(MasterProvisioning):
    """
    :type user: bzt.bza.User
    :type router: BaseCloudTest
    :type _workspaces: bzt.bza.BZAObjectsList[bzt.bza.Workspace]
    """

    def __init__(self):
        super(CloudProvisioning, self).__init__()
        self.results_url = None
        self.results_reader = None
        self.user = User()
        self.__last_master_status = None
        self.browser_open = 'start'
        self.widget = None
        self.detach = False
        self.router = None
        self.test_ended = False
        self.check_interval = 5.0
        self._last_check_time = None
        self.public_report = False
        self.report_name = None
        self._workspaces = []
        self.launch_existing_test = None
        self.disallow_empty_execution = False

    @staticmethod
    def merge_with_blazemeter_config(module):
        if 'blazemeter' not in module.engine.config.get('modules'):
            module.log.debug("Module 'blazemeter' wasn't found in base config")
            return
        bm_mod = module.engine.instantiate_module('blazemeter')
        bm_settings = copy.deepcopy(bm_mod.settings)
        bm_settings.update(module.settings)
        module.settings = bm_settings

    def _validate_passfail(self):
        reporting = self.engine.config.get(Reporter.REP)
        validate_passfail = any(reporter.get('module') == 'passfail' for reporter in reporting)
        if validate_passfail:
            if self.router._test.started_passfail_validation():
                self.log.info("Please keep in mind that validation can take time.")
                timeout = 100
                for i in range(timeout):
                    if self.router._test.get_passfail_validation():
                        return
                    self.log.debug(f"Unsuccessful Passfail validation attempt [{i + 1}]. Retrying...")
                    sleep(1)
                self.log.error("Unable get Passfail validation!")
            else:
                self.log.error("Unable to validate Passfail configuration!")

    def prepare(self):
        CloudProvisioning.merge_with_blazemeter_config(self)
        CloudProvisioning.configure_client(self)
        self._workspaces = self.user.accounts().workspaces()
        if not self._workspaces:
            raise TaurusNetworkError("Your account has no active workspaces, please contact BlazeMeter support")

        self.__dump_locations_if_needed()

        super(CloudProvisioning, self).prepare()
        self.browser_open = self.settings.get("browser-open", self.browser_open)
        self.detach = self.settings.get("detach", self.detach)
        self.check_interval = dehumanize_time(self.settings.get("check-interval", self.check_interval))
        self.public_report = self.settings.get("public-report", self.public_report)
        is_execution_empty = not self.engine.config.get("execution")
        self.launch_existing_test = self.settings.get("launch-existing-test", is_execution_empty, force_set=True)

        finder = ProjectFinder(self.parameters, self.settings, self.user, self._workspaces, self.log)
        finder.default_test_name = "Taurus Cloud Test"

        test_type = self.settings.get("test-type")  # user test type. should we mention it in doc?
        if not test_type:
            func_mode = self.engine.is_functional_mode()
            gui_mode = func_mode and (
                    (len(self.executors) == 1) and
                    isinstance(self.executors[0], SeleniumExecutor))

            if func_mode:
                if gui_mode:
                    test_type = FUNC_GUI_TEST_TYPE
                else:
                    test_type = FUNC_API_TEST_TYPE
            else:
                test_type = TAURUS_TEST_TYPE

        finder.test_type = test_type

        self.router = finder.get_test_router()

        if not self.launch_existing_test:
            self.router.prepare_locations(self.executors, self.engine.config)

            res_files = self.get_rfiles()
            files_for_cloud = self._fix_filenames(res_files)
            self._filter_reporting("blazemeter")    # must not be sent to cloud
            config_for_cloud = self.prepare_cloud_config()
            config_for_cloud.dump(self.engine.create_artifact("cloud", ""))
            del_files = self.settings.get("delete-test-files", True)
            self.router.resolve_test(config_for_cloud, files_for_cloud, del_files)

        self.router.sanitize_test()
        self._validate_passfail()

        self.report_name = self.settings.get("report-name", self.report_name)
        if self.report_name == 'ask' and sys.stdin.isatty():
            self.report_name = input("Please enter report-name: ")

        self.widget = self.get_widget()
        self._filter_reporting("passfail")  # must be sent to claud for conversion

        if self.engine.is_functional_mode():
            self.results_reader = FunctionalBZAReader(self.log)
            self.engine.aggregator.add_underling(self.results_reader)
        else:
            self.results_reader = ResultsFromBZA()
            self.results_reader.log = self.log
            self.engine.aggregator.add_underling(self.results_reader)

    @staticmethod
    def _get_other_modules(config):
        used_classes = LocalClient.__name__, BlazeMeterUploader.__name__
        used_modules = []

        for module in config.get("modules"):
            class_name = config.get("modules").get(module).get("class")
            if class_name and (class_name.split('.')[-1] in used_classes):
                used_modules.append(module)
        return used_modules

    def _get_executors(self):
        executors = []
        for executor in self.executors:
            executors.append(executor.execution.get("executor"))
            if isinstance(executor, SeleniumExecutor):
                executors.append(executor.runner.execution.get("executor"))

        return executors

    def _filter_unused_modules(self, config, provisioning):
        services = [service.get("module") for service in config.get(Service.SERV)]
        reporters = [reporter.get("module") for reporter in config.get(Reporter.REP)]
        consolidator = config.get(SETTINGS).get("aggregator")

        used_modules = self._get_executors() + self._get_other_modules(config)
        used_modules += services + reporters + [consolidator, provisioning]

        modules = set(config.get("modules").keys())
        for module in modules:
            if config.get("modules")[module].get("send-to-blazemeter"):
                continue
            if module not in used_modules:
                del config.get("modules")[module]
            elif config.get("modules")[module].get("class"):
                del config.get("modules")[module]["class"]

    def prepare_cloud_config(self):
        # expand concurrency and throughput
        for executor in self.executors:
            executor.get_load()

        config = copy.deepcopy(self.engine.config)

        provisioning = config.get(Provisioning.PROV)
        self._filter_unused_modules(config, provisioning)

        config['local-bzt-version'] = config.get('version', 'N/A')

        config.filter(CLOUD_CONFIG_BLACK_LIST, black_list=True)

        for execution in config[EXEC]:
            if execution.get("files") == []:
                del execution["files"]

            for param in (ScenarioExecutor.CONCURR, ScenarioExecutor.THRPT):
                param_value = execution.get(param).get(provisioning, None)
                if param_value is None:
                    del execution[param]
                else:
                    execution[param] = param_value

        if self.router.dedicated_ips:
            config[DEDICATED_IPS] = True

        assert isinstance(config, Configuration)
        return config

    def __dump_locations_if_needed(self):
        if self.settings.get("dump-locations", False):
            locations = {}
            for loc in self._workspaces.locations(include_private=True):
                locations[loc['id']] = loc

            data = [("ID", "Name")]
            for location_id in sorted(locations):
                location = locations[location_id]
                data.append((location_id, location['title']))
            table = SingleTable(data) if sys.stdout and sys.stdout.isatty() else AsciiTable(data)
            self.log.warning("Dumping available locations instead of running the test:\n%s", table.table)
            raise NormalShutdown("Done listing locations")

    def _filter_reporting(self, mod_name):
        reporting = self.engine.config.get(Reporter.REP, [])
        new_reporting = []
        for index, reporter in enumerate(reporting):
            exc = TaurusConfigError("'module' attribute not found in %s" % reporter)
            cls = reporter.get('module', exc)
            if cls == mod_name:
                self.log.debug(f"Remove {mod_name} for Cloud provisioning: {reporter}")
            else:
                new_reporting.append(reporter)

        self.engine.config[Reporter.REP] = new_reporting

    @staticmethod
    def configure_client(module):
        module.user.log = module.log
        module.user.logger_limit = module.settings.get("request-logging-limit", module.user.logger_limit)
        module.user.address = module.settings.get("address", module.user.address)
        module.user.token = module.settings.get("token", module.user.token)
        module.user.timeout = dehumanize_time(module.settings.get("timeout", module.user.timeout))
        if isinstance(module.user.http_session, requests.Session):
            module.log.debug("Installing http client")
            module.user.http_session = module.engine.get_http_client()
            module.user.http_request = module.user.http_session.request
        if not module.user.token:
            raise TaurusConfigError("You must provide API token to use cloud provisioning")

    def startup(self):
        super(CloudProvisioning, self).startup()
        self.results_url = self.router.launch_test()
        self.log.info("Started cloud test: %s", self.results_url)
        if self.results_url:
            if self.browser_open in ('start', 'both'):
                open_browser(self.results_url)

        if self.user.token and self.public_report:
            public_link = self.router.master.make_report_public()
            self.log.info("Public report link: %s", public_link)

        if self.report_name:
            self.router.master.set({"name": str(self.report_name)})

    def _should_skip_check(self):
        now = time.time()
        if self._last_check_time is None:
            return False
        elif now >= self._last_check_time + self.check_interval:
            return False
        else:
            return True

    def check(self):
        if self.detach:
            self.log.warning('Detaching Taurus from started test...')
            return True

        if self._should_skip_check():
            self.log.debug("Skipping cloud status check")
            return False

        self._last_check_time = time.time()

        master = self._check_master_status()
        status = master.get('status')
        progress = master.get('progress')   # number value of status, see BZA API

        if status != self.__last_master_status:
            self.__last_master_status = status
            self.log.info("Cloud test status: %s", status)

        if self.results_reader and progress and progress >= BZA_TEST_DATA_RECEIVED:
            self.results_reader.master = self.router.master

        if progress == ENDED:
            self.log.info("Test was stopped in the cloud: %s", status)
            self.test_ended = True
            return True

        self.router.start_if_ready()

        self.widget.update()
        return super(CloudProvisioning, self).check()

    def _check_master_status(self):
        return self.router.get_master_status()

    def post_process(self):
        self.log.warning('Part of result data might be missed here due to BM API specifics')

        if not self.detach and self.router and not self.test_ended:
            self.router.stop_test()

        if self.results_url:
            if self.browser_open in ('end', 'both'):
                open_browser(self.results_url)

        if self.router and self.router.master:
            full = self.router.master.get_full()
            if 'note' in full and full['note']:
                self.log.warning("Cloud test has probably failed with message: %s", full['note'])

            for session in full.get('sessions', ()):
                for error in session.get("errors", ()):
                    raise TaurusException(to_json(error))

            if "hasThresholds" in full and full["hasThresholds"]:
                thresholds = self.router.master.get_thresholds()
                for item in thresholds.get('data', []):
                    if item.get('success', None) is False:
                        reason = None
                        for assertion in item.get('assertions', []):
                            if assertion.get('success', None) is False:
                                criterion = assertion.get('field', '')
                                label = assertion.get('label', '')
                                reason = "Cloud failure criterion %r (on label %r) was met" % (criterion, label)
                                break
                        if reason is None:
                            reason = "Cloud tests failed because failure criteria were met"
                        self.log.warning(reason)
                        raise AutomatedShutdown(reason)

            # if we have captured HARs, let's download them
            for service in self.engine.config.get(Service.SERV, []):
                mod = service.get('module', TaurusConfigError("No 'module' specified for service"))
                assert isinstance(mod, str), mod
                module = self.engine.instantiate_module(mod)
                if isinstance(module, ServiceStubCaptureHAR):
                    self._download_logs()
                    break

            if "functionalSummary" in full:
                summary = full["functionalSummary"]
                if summary is None or summary.get("isFailed", False):
                    raise AutomatedShutdown("Cloud tests failed")

    def _download_logs(self):
        for session in self.router.master.sessions():
            assert isinstance(session, Session)
            for log in session.get_logs():
                self.log.info("Downloading %s from the cloud", log['filename'])
                cloud_dir = os.path.join(self.engine.artifacts_dir, 'cloud-artifacts')
                if not os.path.exists(cloud_dir):
                    os.makedirs(cloud_dir)
                dest = os.path.join(cloud_dir, log['filename'])
                dwn = ExceptionalDownloader(self.engine.get_http_client())
                with ProgressBarContext() as pbar:
                    try:
                        dwn.get(log['dataUrl'], dest, reporthook=pbar.download_callback)
                    except BaseException:
                        self.log.debug("Error is: %s", traceback.format_exc())
                        self.log.warning("Failed to download from %s", log['dataUrl'])
                        continue

                    if log['filename'].startswith('artifacts') and log['filename'].endswith('.zip'):
                        with zipfile.ZipFile(dest) as zipf:
                            for name in zipf.namelist():
                                ext = name.split('.')[-1].lower()
                                if ext in ('har', 'jpg', 'js', 'html', 'css'):
                                    self.log.debug("Extracting %s to %s", name, cloud_dir)
                                    zipf.extract(name, cloud_dir)

    def get_widget(self):
        if not self.widget:
            self.widget = CloudProvWidget(self.router)
        return self.widget


class ResultsFromBZA(ResultsProvider):
    """
    :type master: bzt.bza.Master
    """

    def __init__(self, master=None):
        super(ResultsFromBZA, self).__init__()
        self.master = master
        self.min_ts = 0
        self.log = logging.getLogger('')
        self.prev_errors = BetterDict()
        self.cur_errors = BetterDict()
        self.handle_errors = True

    def _get_err_diff(self):
        # find diff of self.prev_errors and self.cur_errors
        diff = {}
        for label in self.cur_errors:
            if label not in self.prev_errors:
                diff[label] = self.cur_errors[label]
                continue

            for msg in self.cur_errors[label]:
                if msg not in self.prev_errors[label]:
                    prev_count = 0
                else:
                    prev_count = self.prev_errors[label][msg]['count']

                delta = self.cur_errors[label][msg]['count'] - prev_count
                if delta > 0:
                    if label not in diff:
                        diff[label] = {}
                    diff[label][msg] = {'count': delta, 'rc': self.cur_errors[label][msg]['rc']}

        return {k: diff[k] for k in diff if diff[k]}  # clean from empty items

    def _calculate_datapoints(self, final_pass=False):
        if self.master is None:
            return

        data, aggr_raw = self.query_data()
        aggr = {}
        for label in aggr_raw:
            aggr[label['labelName']] = label

        for label in data:
            if label.get('kpis') and not final_pass:
                label['kpis'].pop(-1)  # never take last second since it could be incomplete

        timestamps = []
        for label in data:
            if label.get('label') == 'ALL':
                timestamps.extend([kpi['ts'] for kpi in label.get('kpis', [])])

        self.handle_errors = True

        for tstmp in timestamps:
            point = DataPoint(tstmp)
            point[DataPoint.SOURCE_ID] = self.master['id']
            self.__generate_kpisets(aggr, data, point, tstmp)

            if self.handle_errors:
                self.handle_errors = False
                self.cur_errors = self.__get_errors_from_bza()
                err_diff = self._get_err_diff()
                if err_diff:
                    self.__add_err_diff(point, err_diff)
                    self.prev_errors = self.cur_errors

            point.recalculate()

            self.min_ts = point[DataPoint.TIMESTAMP] + 1
            yield point

    def __add_err_diff(self, point, err_diff):
        for label in err_diff:
            point_label = '' if label == 'ALL' else label
            if point_label not in point[DataPoint.CURRENT]:
                self.log.warning("Got inconsistent kpi/error data for label: %s", point_label)
                kpiset = KPISet()
                point[DataPoint.CURRENT][point_label] = kpiset
                kpiset[KPISet.SAMPLE_COUNT] = sum([item['count'] for item in err_diff[label].values()])
            else:
                kpiset = point[DataPoint.CURRENT][point_label]

            kpiset[KPISet.ERRORS] = self.__get_kpi_errors(err_diff[label])
            kpiset[KPISet.FAILURES] = sum([x['cnt'] for x in kpiset[KPISet.ERRORS]])
            kpiset[KPISet.SAMPLE_COUNT] = kpiset[KPISet.SUCCESSES] + kpiset[KPISet.FAILURES]
            assert kpiset[KPISet.SAMPLE_COUNT] > 0, point_label

    def __generate_kpisets(self, aggr, data, point, tstmp):
        for label in data:
            for kpi in label.get('kpis', []):
                if kpi['ts'] != tstmp:
                    continue
                label_str = label.get('label')
                if label_str is None or label_str not in aggr:
                    self.log.warning("Skipping inconsistent data from API for label: %s", label_str)
                    continue

                if kpi['n'] <= 0:
                    self.log.warning("Skipping empty KPI item got from API: %s", kpi)
                    continue

                kpiset = self.__get_kpiset(aggr, kpi, label_str)
                point[DataPoint.CURRENT]['' if label_str == 'ALL' else label_str] = kpiset

    def __get_errors_from_bza(self):
        #
        # This method reads error report from BZA
        #
        # internal errors format:
        # <request_label>:
        #   <error_message>:
        #     'count': <count of errors>
        #     'rc': <response code>
        #
        result = {}
        try:
            errors = self.master.get_errors()
        except (URLError, TaurusNetworkError):
            self.log.warning("Failed to get errors, will retry in %s seconds...", self.master.timeout)
            self.log.debug("Full exception: %s", traceback.format_exc())
            time.sleep(self.master.timeout)
            errors = self.master.get_errors()
            self.log.info("Succeeded with retry")

        for e_record in errors:
            _id = e_record["_id"]
            if _id == "ALL":
                _id = ""
            result[_id] = {}
            for error in e_record['errors']:
                result[_id][error['m']] = {'count': error['count'], 'rc': error['rc']}
            for assertion in e_record['assertions']:
                result[_id][assertion['failureMessage']] = {'count': assertion['failures'], 'rc': assertion['name']}
        return result

    def __get_kpi_errors(self, errors):
        result = []
        for msg in errors:
            kpi_error = KPISet.error_item_skel(
                error=msg,
                ret_c=errors[msg]['rc'],
                cnt=errors[msg]['count'],
                errtype=KPISet.ERRTYPE_ERROR,
                urls=Counter(), tag=None)
            result.append(kpi_error)
        return result

    def __get_kpiset(self, aggr, kpi, label):
        kpiset = KPISet()
        kpiset[KPISet.FAILURES] = kpi['ec']
        kpiset[KPISet.CONCURRENCY] = kpi['na']
        kpiset[KPISet.SAMPLE_COUNT] = kpi['n']
        assert kpi['n'] > 0 and kpi['n'] >= kpi['ec']
        kpiset[KPISet.SUCCESSES] = kpi['n'] - kpi['ec']
        kpiset.sum_rt += kpi['t_avg'] * kpi['n'] / 1000.0
        kpiset.sum_lt += kpi['lt_avg'] * kpi['n'] / 1000.0
        perc_map = {'90line': 90.0, "95line": 95.0, "99line": 99.0}
        for field, level in iteritems(perc_map):
            kpiset[KPISet.PERCENTILES][str(level)] = aggr[label][field] / 1000.0
        return kpiset

    def query_data(self):
        try:
            data = self.master.get_kpis(self.min_ts)
        except (URLError, TaurusNetworkError):
            self.log.warning("Failed to get result KPIs, will retry in %s seconds...", self.master.timeout)
            self.log.debug("Full exception: %s", traceback.format_exc())
            time.sleep(self.master.timeout)
            data = self.master.get_kpis(self.min_ts)
            self.log.info("Succeeded with retry")

        try:
            aggr = self.master.get_aggregate_report()
        except (URLError, TaurusNetworkError):
            self.log.warning("Failed to get aggregate results, will retry in %s seconds...", self.master.timeout)
            self.log.debug("Full exception: %s", traceback.format_exc())
            time.sleep(self.master.timeout)
            aggr = self.master.get_aggregate_report()
            self.log.info("Succeeded with retry")

        return data, aggr


class FunctionalBZAReader(FunctionalResultsReader):
    def __init__(self, parent_log, master=None):
        super(FunctionalBZAReader, self).__init__()
        self.master = master
        self.log = parent_log.getChild(self.__class__.__name__)

    @staticmethod
    def extract_samples_from_group(group, group_summary):
        group_name = group_summary.get("name") or "Tests"
        for sample in group["samples"]:
            status = "PASSED"
            if sample["error"]:
                status = "FAILED"
            error_msg = ""
            error_trace = ""
            assertions = sample.get("assertions")
            if assertions:
                for assertion in assertions:
                    if assertion.get("isFailed"):
                        error_msg = assertion.get("errorMessage")
                        status = "BROKEN"

            rtm = sample.get("responseTime") or 0.0
            yield FunctionalSample(
                test_case=sample["label"],
                test_suite=group_name,
                status=status,
                start_time=int(sample["created"]),
                duration=rtm / 1000.0,
                error_msg=error_msg,
                error_trace=error_trace,
                extras={},
                subsamples=[],
            )

    def read(self, last_pass=False):
        if self.master is None:
            return

        if last_pass:
            try:
                groups = self.master.get_functional_report_groups()
            except (URLError, TaurusNetworkError):
                self.log.warning("Failed to get test groups, will retry in %s seconds...", self.master.timeout)
                self.log.debug("Full exception: %s", traceback.format_exc())
                time.sleep(self.master.timeout)
                groups = self.master.get_functional_report_groups()
                self.log.info("Succeeded with retry")

            for group_summary in groups:
                group_id = group_summary['groupId']
                try:
                    group = self.master.get_functional_report_group(group_id)
                except (URLError, TaurusNetworkError):
                    self.log.warning("Failed to get test group, will retry in %s seconds...", self.master.timeout)
                    self.log.debug("Full exception: %s", traceback.format_exc())
                    time.sleep(self.master.timeout)
                    group = self.master.get_functional_report_group(group_id)
                    self.log.info("Succeeded with retry")

                for sample in self.extract_samples_from_group(group, group_summary):
                    yield sample


class CloudProvWidget(Pile, PrioritizedWidget):
    def __init__(self, test):
        """
        :type test: BaseCloudTest
        """
        self.test = test
        self.text = Text("")
        super(CloudProvWidget, self).__init__([self.text])
        PrioritizedWidget.__init__(self)

    def update(self):
        txt = self.test.get_test_status_text()
        if txt:
            self.text.set_text(txt)


class ServiceStubScreenshoter(Service):
    def startup(self):
        if not isinstance(self.engine.provisioning, CloudProvisioning):
            self.log.warning("Stub for service 'screenshoter', use cloud provisioning to have it working")


class ServiceStubCaptureHAR(Service):
    def startup(self):
        if not isinstance(self.engine.provisioning, CloudProvisioning):
            self.log.warning("Stub for service 'capturehar', use cloud provisioning to have it working")