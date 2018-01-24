"""
Main BZT classes

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
import logging
import os
import yaml
import uuid

class Commands(object):

    def __getattribute__(self, name):
        attr = object.__getattribute__(self, name)
        if hasattr(attr, '__call__'):
            def newfunc(*args, **kwargs):
                result = attr(*args, **kwargs)
                if not attr.__name__[0] == "_":
                    self._save_settings()
                return result
            return newfunc
        else:
            return attr

    def __init__(self, parent_logger):
        """

        :type parent_logger: logging.Logger
        """
        self.settings_file = os.path.expanduser(os.path.join('~', ".bzt-commands"))
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.settings = {}
        self._load_settings()
        if "user_uuid" not in self.settings:
            self.settings["user_uuid"] = str(uuid.uuid1())
        if "remote" not in self.settings:
            self.settings["remote"] = {}

    def _load_settings(self):
        if not os.path.isfile(self.settings_file):
            self._save_settings()
        with open(self.settings_file, 'r+') as bzt_remote_file:
            self.settings = yaml.load(bzt_remote_file.read())
        if not self.settings:
            self.settings = {}

    def _save_settings(self):
        with open(self.settings_file, 'w+') as bzt_remote_file:
            yaml.dump(self.settings, bzt_remote_file)

    def remote_catalog(self):
        self.log.info("Catalog")

    def remote_on(self):
        self.log.info("Setting Remote On")
        self.settings["remote"]["enabled"] = True

    def remote_off(self):
        self.log.info("Setting Remote Off")
        self.settings["remote"]["enabled"] = False

    def remote_attach(self, service_id):
        self.log.info("Remote Attach: %s" % service_id)

    def remote_detach(self, attach_id):
        self.log.info("Remote Detach: %s" % attach_id)

    def remote_list(self):
        self.log.info("Remote List")
