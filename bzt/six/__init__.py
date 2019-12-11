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
import operator
import traceback
import socketserver

from http import server

try:
    from lxml import etree
except ImportError:
    try:
        import cElementTree as etree
    except ImportError:
        import elementtree.ElementTree as etree

numeric_types = (int, float, complex)
viewvalues = operator.methodcaller("values")

# server.py
BaseHTTPServer = server
SimpleHTTPRequestHandler = BaseHTTPServer.SimpleHTTPRequestHandler
socketserver = socketserver

def unicode_decode(string, errors="strict"):
    if isinstance(string, bytes):
        return string.decode("utf-8", errors)
    else:
        return string


def communicate(proc):  # todo: replace usage of it with sync_run()
    out, err = proc.communicate()
    out = unicode_decode(out, errors="ignore")
    err = unicode_decode(err, errors="ignore")
    return out, err


def iteritems(dictionary, **kw):
    return iter(dictionary.items(**kw))


def b(string):
    return string.encode("latin-1")


def get_stacktrace(exc):
    return ''.join(traceback.format_tb(exc.__traceback__)).rstrip()


def reraise(exc_info, exc=None):
    _type, message, stacktrace = exc_info
    if exc is None:
        exc = _type(message)
    exc.__traceback__ = stacktrace
    raise exc


def stream_decode(string):
    if not isinstance(string, str):
        return string.decode()
    else:
        return string
