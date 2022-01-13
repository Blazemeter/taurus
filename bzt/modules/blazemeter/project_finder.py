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

from bzt import TaurusConfigError
from bzt.bza import Test
from bzt.modules.blazemeter.cloud_test import CloudTaurusTest
from bzt.modules.blazemeter.net_utils import parse_blazemeter_test_link


class ProjectFinder(object):
    def __init__(self, parameters, settings, user, workspaces, parent_log):
        super(ProjectFinder, self).__init__()
        self.default_test_name = "Taurus Test"
        self.parameters = parameters
        self.settings = settings
        self.log = parent_log.getChild(self.__class__.__name__)
        self.user = user
        self.workspaces = workspaces
        self.test_type = None

    def _find_project(self, proj_name):
        """
        :rtype: bzt.bza.Project
        """
        if isinstance(proj_name, (int, float)):  # project id
            proj_id = int(proj_name)
            self.log.debug("Treating project name as ID: %s", proj_id)
            project = self.workspaces.projects(ident=proj_id).first()
            if not project:
                raise TaurusConfigError("BlazeMeter project not found by ID: %s" % proj_id)
        elif proj_name:
            project = self.workspaces.projects(name=proj_name).first()
        else:
            project = None

        return project

    def _ws_proj_switch(self, project):
        if project:
            return project
        else:
            return self.workspaces

    def resolve_external_test(self):
        proj_name = self.parameters.get("project", self.settings.get("project"))
        test_name = self.parameters.get("test", self.settings.get("test", self.default_test_name))

        project = self._find_project(proj_name)
        if not project and proj_name:
            project = self.workspaces.first().create_project(proj_name)

        test = self._ws_proj_switch(project).tests(name=test_name, test_type='external').first()

        if not test:
            if not project:
                info = self.user.fetch()
                project = self.workspaces.projects(ident=info['defaultProject']['id']).first()
                if not project:
                    project = self.workspaces.first().create_project("Taurus Tests Project")

            test = project.create_test(test_name, {"type": "external"})

        return test

    def resolve_account(self, account_name):
        account = None

        if isinstance(account_name, (int, float)):
            acc_id = int(account_name)
            self.log.debug("Treating account name as ID: %s", acc_id)
            account = self.user.accounts(ident=acc_id).first()
            if not account:
                raise TaurusConfigError("BlazeMeter account not found by ID: %s" % acc_id)
        elif account_name:
            account = self.user.accounts(name=account_name).first()
            if not account:
                raise TaurusConfigError("BlazeMeter account not found by name: %s" % account_name)

        if account:
            return account

        self.user.fetch()
        account = self.user.accounts(ident=self.user['defaultProject']['accountId']).first()
        self.log.debug("Using default account: %s", account)
        return account

    def resolve_workspace(self, account, workspace_name):
        workspace = None

        if isinstance(workspace_name, (int, float)):
            workspace_id = int(workspace_name)
            self.log.debug("Treating workspace name as ID: %s", workspace_id)
            workspace = account.workspaces(ident=workspace_id).first()
            if not workspace:
                raise TaurusConfigError("BlazeMeter workspace not found by ID: %s" % workspace_id)
        elif workspace_name is not None:
            workspace = account.workspaces(name=workspace_name).first()
            if not workspace:
                raise TaurusConfigError("BlazeMeter workspace not found: %s" % workspace_name)

        if workspace is None:
            workspace = account.workspaces(ident=self.user['defaultProject']['workspaceId']).first()
            self.log.debug("Using first workspace: %s" % workspace)

        return workspace

    def resolve_project(self, workspace, project_name):
        if isinstance(project_name, (int, float)):  # project id
            project_id = int(project_name)
            self.log.debug("Treating project name as ID: %s", project_id)
            project = workspace.projects(ident=project_id).first()
            if not project:
                raise TaurusConfigError("BlazeMeter project not found by ID: %s" % project_id)
        elif project_name:
            project = workspace.projects(name=project_name).first()
        else:
            project = None

        if not project:
            project = self._create_project_or_use_default(workspace, project_name)

        return project

    def _create_project_or_use_default(self, workspace, proj_name):
        if proj_name:
            return workspace.create_project(proj_name)
        else:
            info = self.user.fetch()
            self.log.debug("Looking for default project: %s", info['defaultProject']['id'])
            project = self.workspaces.projects(ident=info['defaultProject']['id']).first()
            if not project:
                project = workspace.create_project("Taurus Tests Project")
            return project

    def resolve_test(self, project, test_name, test_type):
        is_int = isinstance(test_name, (int, float))
        is_digit = isinstance(test_name, str) and test_name.isdigit()

        if is_int or is_digit:
            test_id = int(test_name)
            self.log.debug("Treating project name as ID: %s", test_id)
            test = project.tests(ident=test_id, test_type=test_type).first()
            if not test:
                raise TaurusConfigError("BlazeMeter test not found by ID: %s" % test_id)
        elif test_name is not None:
            test = project.tests(name=test_name, test_type=test_type).first()
        else:
            test = None

        return test

    def get_test_router(self):
        default_location = self.settings.get("default-location", None)
        account_name = self.parameters.get("account", self.settings.get("account", None))
        workspace_name = self.parameters.get("workspace", self.settings.get("workspace", None))
        project_name = self.parameters.get("project", self.settings.get("project"))
        test_name = self.parameters.get("test", self.settings.get("test", self.default_test_name))
        launch_existing_test = self.settings.get("launch-existing-test", False)

        # if we're to launch existing test - don't use test_type filter (look for any type)
        filter_test_type = None if launch_existing_test else self.test_type

        test_spec = parse_blazemeter_test_link(test_name)
        self.log.debug("Parsed test link: %s", test_spec)
        if test_spec is not None:
            account, workspace, project, test = self.user.test_by_ids(test_spec.account_id, test_spec.workspace_id,
                                                                      test_spec.project_id, test_spec.test_id,
                                                                      test_type=filter_test_type)
            if not test:
                raise TaurusConfigError("Test not found: %s", test_name)
            self.log.info("Found test by link: %s", test_name)
        else:
            account = self.resolve_account(account_name)
            workspace = self.resolve_workspace(account, workspace_name)
            project = self.resolve_project(workspace, project_name)
            test = self.resolve_test(project, test_name, test_type=filter_test_type)

        if isinstance(test, Test):
            test_class = CloudTaurusTest
        else:   # test not found
            if launch_existing_test:
                raise TaurusConfigError("Can't find test for launching: %r" % test_name)

            test_class = CloudTaurusTest

        router = test_class(self.user, test, project, test_name, default_location, launch_existing_test, self.log)
        router._workspaces = self.workspaces
        router.cloud_mode = self.settings.get("cloud-mode", None)
        router.dedicated_ips = self.settings.get("dedicated-ips", False)
        router.test_type = self.test_type
        router.send_report_email = self.settings.get("send-report-email", False)
        return router
