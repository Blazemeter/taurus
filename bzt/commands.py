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
# import os
import yaml
import uuid

from terminaltables import AsciiTable
# import json

import sys
import os

from bzt.remote import Remote

if os.name == 'nt':
    # import msvcrt
    import ctypes

if os.name == 'nt':
    class _CursorInfo(ctypes.Structure):
        _fields_ = [("size", ctypes.c_int),
                    ("visible", ctypes.c_byte)]


class Commands(object):

    def __getattribute__(self, name):
        attr = object.__getattribute__(self, name)
        if hasattr(attr, '__call__'):
            def newfunc(*args, **kwargs):
                if not attr.__name__[0] == "_":
                    self._hide_cursor()
                result = attr(*args, **kwargs)
                if not attr.__name__[0] == "_":
                    self._save_settings()
                    self._show_cursor()
                return result

            return newfunc
        else:
            return attr

    def __init__(self, parent_logger):
        """

        :type parent_logger: logging.Logger
        """

        self.indent = 2
        self.indent_str = ' ' * self.indent

        self.settings_file = os.path.expanduser(os.path.join('~', ".bzt-commands"))
        self.log = parent_logger.getChild(self.__class__.__name__)

        self.settings = {}
        self._load_settings()
        if "user_uuid" not in self.settings:
            self.settings["user_uuid"] = str(uuid.uuid1())
        if "remote" not in self.settings:
            self.settings["remote"] = {}

        # TODO: Hidded the on/off option, by defautl enabled
        self.settings["remote"]["enabled"] = True

        self.remote = Remote(parent_logger, self.settings["user_uuid"])

    @staticmethod
    def _hide_cursor():
        if os.name == 'nt':
            ci = _CursorInfo()
            handle = ctypes.windll.kernel32.GetStdHandle(-11)
            ctypes.windll.kernel32.GetConsoleCursorInfo(handle, ctypes.byref(ci))
            ci.visible = False
            ctypes.windll.kernel32.SetConsoleCursorInfo(handle, ctypes.byref(ci))
        elif os.name == 'posix':
            sys.stdout.write("\033[?25l")
            sys.stdout.flush()

    @staticmethod
    def _show_cursor():
        if os.name == 'nt':
            ci = _CursorInfo()
            handle = ctypes.windll.kernel32.GetStdHandle(-11)
            ctypes.windll.kernel32.GetConsoleCursorInfo(handle, ctypes.byref(ci))
            ci.visible = True
            ctypes.windll.kernel32.SetConsoleCursorInfo(handle, ctypes.byref(ci))
        elif os.name == 'posix':
            sys.stdout.write("\033[?25h")
            sys.stdout.flush()

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

    @staticmethod
    def _get_table(header, data, title=""):
        table_headers = header["headers"]
        table_headers_desc = header["descriptions"]
        table_data = []

        table_header = []
        header_index = 0
        justify_columns = {}
        for header in table_headers:
            table_header.append(table_headers_desc[header].split(":")[0])
            justify_columns[header_index] = table_headers_desc[header].split(":")[1]
            header_index += 1
        table_data.append(table_header)

        for element in data:
            table_item = []
            for header in table_headers:
                table_item.append(element[header])
            table_data.append(table_item)

        table_instance = AsciiTable(table_data, title)

        table_instance.justify_columns = justify_columns

        return table_instance.table.splitlines()

    def remote_catalog(self):

        self.log.info('')
        self.log.info(self.indent_str + "Catalog")

        elements = self.remote.get_catalog()

        # print(elements)

        header = {
            "headers": ["ServiceID", "ServiceDesc", "ServiceType"],
            "descriptions": {"ServiceID": "service_id:left", "ServiceDesc": "Description:left",
                             "ServiceType": "Type:center"}
        }

        for line in self._get_table(header, elements):
            self.log.info(self.indent_str + line)

        self.log.info(self.indent_str + "Done.")

    def remote_on(self):
        self.log.info(self.indent_str + "Setting Remote On")
        self.settings["remote"]["enabled"] = True

    def remote_off(self):
        self.log.info(self.indent_str + "Setting Remote Off")
        self.settings["remote"]["enabled"] = False

    def remote_attach(self, service_ids):
        self.log.info(self.indent_str + "Remote Attach:")

        attached_ids = self.remote.attach_services(service_ids)
        attached = self.remote.list_attached(attached_ids)
        self._list_attached(attached)

        self.log.info(self.indent_str + "Done.")

    def remote_detach(self, attach_ids):
        self.log.info(self.indent_str + "Remote Detach:")
        if len(attach_ids) == 1 and attach_ids[0] == "*all":
            attached = self.remote.list_attached()
            attach_list = []
            for attach in attached:
                attach_list.append(attach["attach_id"])
            attach_ids = attach_list

        for attach_id in attach_ids:
            print(attach_id)
            self.remote.detach_service(attach_id)

        self.log.info(self.indent_str + "Done.")

    def remote_inspect(self, attach_id):
        self.log.info(self.indent_str + "Remote Inspect:" + attach_id)

        self.log.info(self.indent_str + "Done.")

    def _list_attached(self, attached):
        elements = []
        for attach in attached:
            if "service_info" in attach and "selenium" in attach["service_info"]:
                service_remote = attach["service_info"]["selenium"]["info"]["remote"]
            else:
                service_remote = "..."
            element = {
                "service_id": attach["service_id"],
                "attach_id": attach["attach_id"],
                "machine_state": attach["machine_state"],
                "service_state": attach["service_state"],

                "service_remote": service_remote,
            }
            elements.append(element)

        header = {
            "headers": ["service_id", "attach_id", "machine_state", "service_state", "service_remote"],
            "descriptions": {"service_id": "service_id:left", "attach_id": "attach_id:left",
                             "machine_state": "Machine State:center", "service_state": "Service State:center",
                             "service_remote": "Remote:left"}
        }

        for line in self._get_table(header, elements):
            self.log.info(self.indent_str + line)

    def remote_list(self):
        self.log.info(self.indent_str + "Remote List")
        attached = self.remote.list_attached()

        self._list_attached(attached)

        self.log.info(self.indent_str + "Done.")
