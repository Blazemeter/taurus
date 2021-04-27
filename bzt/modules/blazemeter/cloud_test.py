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
from abc import abstractmethod

import yaml

from bzt import TaurusConfigError
from bzt.bza import Workspace
from bzt.utils import iteritems, BetterDict
from bzt.modules.blazemeter.const import LOC

TAURUS_TEST_TYPE = "taurus"
FUNC_API_TEST_TYPE = "functionalApi"
FUNC_GUI_TEST_TYPE = "functionalGui"


class BaseCloudTest(object):
    """
    :type _user: bzt.bza.User
    :type _project: bzt.bza.Project
    :type _test: bzt.bza.Test
    :type master: bzt.bza.Master
    :type cloud_mode: str
    """

    def __init__(self, user, test, project, test_name, default_location, launch_existing_test, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.default_location = default_location
        self._test_name = test_name
        self._last_status = None
        self._sessions = None
        self._started = False
        self._user = user
        self._project = project
        self._test = test
        self.launch_existing_test = launch_existing_test
        self.master = None
        self._workspaces = None
        self.cloud_mode = None
        self.dedicated_ips = False
        self.test_type = None
        self.send_report_email = False

    @abstractmethod
    def prepare_locations(self, executors, engine_config):
        pass

    @abstractmethod
    def resolve_test(self, taurus_config, rfiles, delete_old_files=False):
        pass

    @abstractmethod
    def launch_test(self):
        """launch cloud test"""
        pass

    @abstractmethod
    def sanitize_test(self):
        """Sanitize cloud test"""
        pass

    @abstractmethod
    def start_if_ready(self):
        """start cloud test if all engines are ready"""
        pass

    @abstractmethod
    def get_test_status_text(self):
        pass

    @abstractmethod
    def stop_test(self):
        pass

    def get_master_status(self):
        self._last_status = self.master.get_status()
        return self._last_status


class CloudTaurusTest(BaseCloudTest):
    def prepare_locations(self, executors, engine_config):
        available_locations = {}
        is_taurus4 = True
        workspace = Workspace(self._project, {'id': self._project['workspaceId']})
        for loc in workspace.locations(include_private=is_taurus4):
            available_locations[loc['id']] = loc

        if LOC in engine_config and not is_taurus4:
            self.log.warning("Deprecated test API doesn't support global locations")

        for executor in executors:
            if LOC in executor.execution \
                    and isinstance(executor.execution[LOC], dict):
                exec_locations = executor.execution[LOC]
                self._check_locations(exec_locations, available_locations)
            elif LOC in engine_config and is_taurus4:
                self._check_locations(engine_config[LOC], available_locations)
            else:
                default_loc = self._get_default_location(available_locations)
                executor.execution[LOC] = BetterDict.from_dict({default_loc: 1})

            executor.get_load()  # we need it to resolve load settings into full form

    def _get_default_location(self, available_locations):
        if self.default_location and self.default_location in available_locations:
            return self.default_location

        self.log.debug("Default location %s not found", self.default_location)

        for location_id in sorted(available_locations):
            location = available_locations[location_id]
            if location['sandbox'] and not location.get('purposes', {}).get('functional', False):
                return location_id

        if available_locations:
            location_id = sorted(available_locations.keys())[0]
            self.log.warning("Using first location as default: %s", location_id)
            return location_id

        self.log.warning("List of supported locations for you is: %s", sorted(available_locations.keys()))
        raise TaurusConfigError("No sandbox or default location available, please specify locations manually")

    def _check_locations(self, locations, available_locations):
        for location in locations:
            if location not in available_locations:
                self.log.warning("List of supported locations for you is: %s", sorted(available_locations.keys()))
                raise TaurusConfigError("Invalid location requested: %s" % location)

    def resolve_test(self, taurus_config, rfiles, delete_old_files=False):
        if self.launch_existing_test:
            return

        if self._test is not None:
            test_type = self._test.get("configuration").get("type")
            if test_type != self.test_type:
                self.log.debug("Can't reuse test type %r as Taurus test, will create new one", test_type)
                self._test = None

        if self._test is None:
            test_config = {
                "type": self.test_type,
                "plugins": {
                    "taurus": {
                        "filename": ""  # without this line it does not work
                    }
                }
            }

            self._test = self._project.create_test(self._test_name, test_config)

        if delete_old_files:
            self._test.delete_files()

        taurus_config = yaml.safe_dump(taurus_config, default_flow_style=False, explicit_start=True, canonical=False)
        self._test.upload_files(taurus_config, rfiles)
        self._test.update_props({'configuration': {'executionType': self.cloud_mode}})
        self._test.update_props({
            "shouldSendReportEmail": self.send_report_email
        })

    def sanitize_test(self):
        self._test.update_props({'overrideExecutions': []})
        self._test.update_props({'configuration': {'scriptType': 'taurus'}})

    def launch_test(self):
        self.log.info("Initiating cloud test with %s ...", self._test.address)
        self.master = self._test.start(as_functional=self.test_type in (FUNC_API_TEST_TYPE, FUNC_GUI_TEST_TYPE))
        return self.master.address + '/app/#/masters/%s' % self.master['id']

    def start_if_ready(self):
        self._started = True

    def stop_test(self):
        if self.master:
            self.log.info("Ending cloud test...")
            if not self._last_status:
                self.get_master_status()

            if self._last_status["progress"] >= 100:
                self.master.stop()
            else:
                self.master.terminate()

    def get_test_status_text(self):
        if not self._sessions:
            self._sessions = self.master.sessions()
            if not self._sessions:
                return

        mapping = BetterDict()  # dict(executor -> dict(scenario -> dict(location -> servers count)))
        for session in self._sessions:
            try:
                name_split = [part.strip() for part in session['name'].split('/')]
                location = session['configuration']['location']
                count = session['configuration']['serversCount']
                ex_item = mapping.get(name_split[0], force_set=True)

                if len(name_split) > 1:
                    name = name_split[1]
                else:
                    name = "N/A"

                ex_item.get(name, force_set=True)[location] = count
            except KeyError:
                self._sessions = None

        txt = "%s #%s\n" % (self._test['name'], self.master['id'])
        for executor, scenarios in iteritems(mapping):
            txt += " %s" % executor
            for scenario, locations in iteritems(scenarios):
                txt += " %s:\n" % scenario
                for location, count in iteritems(locations):
                    txt += "  Agents in %s: %s\n" % (location, count)

        return txt
