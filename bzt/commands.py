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
import requests
from terminaltables import AsciiTable, SingleTable
import json

import sys
import os

if os.name == 'nt':
    import msvcrt
    import ctypes


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
        self.settings_file = os.path.expanduser(os.path.join('~', ".bzt-commands"))
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.base_service = "https://kip7rvk9ih.execute-api.us-east-1.amazonaws.com/dev/"
        self.settings = {}
        self._load_settings()
        if "user_uuid" not in self.settings:
            self.settings["user_uuid"] = str(uuid.uuid1())
        if "remote" not in self.settings:
            self.settings["remote"] = {}

    def _hide_cursor(self):
        if os.name == 'nt':
            ci = _CursorInfo()
            handle = ctypes.windll.kernel32.GetStdHandle(-11)
            ctypes.windll.kernel32.GetConsoleCursorInfo(handle, ctypes.byref(ci))
            ci.visible = False
            ctypes.windll.kernel32.SetConsoleCursorInfo(handle, ctypes.byref(ci))
        elif os.name == 'posix':
            sys.stdout.write("\033[?25l")
            sys.stdout.flush()

    def _show_cursor(self):
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

    def _get_table(self, header, data, title="", indent=0):
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
        indent = 2
        indent_str = ' ' * indent

        self.log.info('')
        self.log.info(indent_str + "Catalog")

        r = requests.post(self.base_service + "list")
        #print(r.status_code)
        elements = r.json()

        #print(elements)

        header = {
            "headers":["ServiceID", "ServiceDesc", "ServiceType"],
            "descriptions":{"ServiceID": "service_id:left", "ServiceDesc": "Description:left", "ServiceType": "Type:center"}
        }

        for line in self._get_table(header, elements):
            self.log.info(indent_str + line)

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
