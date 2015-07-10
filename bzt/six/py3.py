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
import io
import operator
import collections
import urllib

import tkinter
import configparser

string_types = str,
integer_types = int,
class_types = type,
text_type = str
binary_type = bytes

configparser = configparser
tkinter = tkinter
tkfont = tkinter.font
UserDict = collections.UserDict

StringIO = io.StringIO
BytesIO = io.BytesIO

parse = urllib.parse
request = urllib.request
"""
urlopen = urllib.request.urlopen
urlencode = __import__("urllib.parse", fromlist="urlencode").urlencode
urlsplit = __import__("urllib.parse", fromlist="urlsplit").urlsplit
urlparse = __import__("urllib.parse", fromlist="urlparse").urlparse
build_opener = __import__("urllib.request", fromlist="urlopen").build_opener
install_opener = __import__("urllib.request", fromlist="install_opener").install_opener

Request = __import__("urllib.request", fromlist="Request").Request
ProxyHandler = __import__("", fromlist="ProxyHandler").ProxyHandler
HTTPError = __import__("urllib.error", fromlist="HTTPError").HTTPError
FancyURLopener = __import__("urllib.request", fromlist="FancyURLopener").FancyURLopener
URLopener = __import__("urllib.request", fromlist="URLopener").URLopener
"""


def b(string):
    return string.encode("latin-1")


def u(string):
    return string


def iteritems(dictionary, **kw):
    return iter(dictionary.items(**kw))


viewvalues = operator.methodcaller("values")
