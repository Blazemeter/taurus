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
# pylint: skip-file
import io
import sys
import operator
import collections
import traceback
import urllib
import socketserver
import configparser

from http import server, cookiejar


string_types = str,
integer_types = int,
numeric_types = (int, float, complex)
class_types = type,
text_type = str
binary_type = bytes
file_type = io.IOBase

configparser = configparser
UserDict = collections.UserDict

StringIO = io.StringIO
BytesIO = io.BytesIO

cookielib=cookiejar
request = urllib.request
parse = urllib.parse
urlopen = request.urlopen
urlencode = parse.urlencode
build_opener = request.build_opener
install_opener = request.install_opener
ProxyHandler = request.ProxyHandler
Request = request.Request
HTTPError = urllib.error.HTTPError
URLError = urllib.error.URLError
BaseHTTPServer = server
socketserver = socketserver
SimpleHTTPRequestHandler = BaseHTTPServer.SimpleHTTPRequestHandler
url2pathname = urllib.request.url2pathname

viewvalues = operator.methodcaller("values")
r_input = input


def iteritems(dictionary, **kw):
    return iter(dictionary.items(**kw))


def b(string):
    return string.encode("latin-1")


def u(string):
    return string


def get_stacktrace(exc):
    return ''.join(traceback.format_tb(exc.__traceback__)).rstrip()


def reraise(exc_info, exc=None):
    _type, message, stacktrace = exc_info
    if exc is None:
        exc = _type(message)
    exc.__traceback__ = stacktrace
    raise exc


def stream_decode(string):
    if not isinstance(string, text_type):
        return string.decode()
    else:
        return string


def deunicode(string, errors="strict"):
    """
    If string is unicode - convert it to basic string. Otherwise - leave it.
    Does nothing on py3 as there're no basic strings there.
    """
    return string
