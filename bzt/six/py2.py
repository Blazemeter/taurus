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
import types
import urllib
import urllib2
import urlparse
import operator

string_types = basestring,
integer_types = (int, long)
class_types = (type, types.ClassType)
text_type = unicode
binary_type = str


def iteritems(dictionary, **kw):
    return iter(dictionary.iteritems(**kw))


viewvalues = operator.methodcaller("viewvalues")

parse = urlparse
urlopen = urllib2.urlopen
urlencode = urllib.urlencode
urlsplit = urlparse.urlsplit
urlparse = urlparse.urlparse
build_opener = __import__("urllib2", fromlist="build_opener").build_opener
install_opener = __import__("urllib2", fromlist="install_opener").install_opener

Request = __import__("urllib2", fromlist="Request").Request
ProxyHandler = __import__("urllib2", fromlist="ProxyHandler").ProxyHandler
HTTPError = __import__("urllib2", fromlist="HTTPError").HTTPError
FancyURLopener = __import__("urllib", fromlist="FancyURLopener").FancyURLopener
URLopener = __import__("urllib", fromlist="URLopener").URLopener
ConfigParser = __import__("ConfigParser")
Tkinter = __import__("Tkinter")
TkMoved = Tkinter.Tk
Text = Tkinter.Text
tkFont = __import__("tkFont", fromlist="Font").Font
UserDict = __import__("UserDict").UserDict

StringIO = BytesIO = __import__("StringIO", fromlist="StringIO").StringIO


def b(string):
    return string


def u(string):
    return unicode(string.replace(r'\\', r'\\\\'), "unicode_escape")
