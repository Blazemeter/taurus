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

import types
import traceback
import urllib
import urllib2
import urlparse
import operator
import ConfigParser
import UserDict
import StringIO
import BaseHTTPServer
import SocketServer as socketserver

string_types = basestring,
integer_types = (int, long)
numeric_types = (int, long, float, complex)
class_types = (type, types.ClassType)
text_type = unicode
binary_type = str
file_type = file

configparser = ConfigParser
UserDict = UserDict.UserDict
BytesIO = StringIO.StringIO
StringIO = StringIO.StringIO

parse = urlparse
request = urllib
urlopen = urllib2.urlopen
urlencode = urllib.urlencode
build_opener = urllib2.build_opener
install_opener = urllib2.install_opener
ProxyHandler = urllib2.ProxyHandler
Request = urllib2.Request
HTTPError = urllib2.HTTPError
URLError = urllib2.URLError
BaseHTTPServer = BaseHTTPServer
socketserver = socketserver
SimpleHTTPRequestHandler = BaseHTTPServer.BaseHTTPRequestHandler

viewvalues = operator.methodcaller("viewvalues")

r_input = raw_input


def iteritems(dictionary, **kw):
    return iter(dictionary.iteritems(**kw))


def b(string):
    return string


def u(string):
    if not isinstance(string, unicode):
        return unicode(string.replace('\\', '\\\\'), "unicode_escape")
    else:
        return string


def get_stacktrace(exc):
    return traceback.format_exc(exc)


def reraise(exc_info):
    raise exc_info[0], exc_info[1], exc_info[2]


def unicode_decode(string):
    if not isinstance(string, unicode):
        return string.decode('utf-8')
    else:
        return string
