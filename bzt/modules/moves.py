import sys
import types
import operator

PY2 = sys.version_info[0] == 2
PY3 = sys.version_info[0] == 3

if PY2:

    string_types = basestring,
    integer_types = (int, long)
    class_types = (type, types.ClassType)
    text_type = unicode
    binary_type = str

    def iteritems(d, **kw):
        return iter(d.iteritems(**kw))

    from urllib2 import Request
    from urllib2 import urlopen
    from urllib2 import ProxyHandler
    from urllib2 import build_opener
    from urllib2 import install_opener
    from urllib2 import HTTPError
    from urllib import urlencode
    from urlparse import urlsplit
    from urlparse import urlparse
    from urllib import FancyURLopener
    from urllib import URLopener
    import ConfigParser as ConfigParser

    import Tkinter as Tkinter
    from Tkinter import Tk, Text
    from tkFont import Font as tkFont

    from UserDict import UserDict

    viewvalues = operator.methodcaller("viewvalues")

    import StringIO

    StringIO = BytesIO = StringIO.StringIO

    def b(s):
        return s

    def u(s):
        return unicode(s.replace(r'\\', r'\\\\'), "unicode_escape")

else:
    string_types = str,
    integer_types = int,
    class_types = type,
    text_type = str
    binary_type = bytes

    def iteritems(d, **kw):
        return iter(d.items(**kw))

    from urllib.request import Request
    from urllib.request import urlopen
    from urllib.request import ProxyHandler
    from urllib.request import build_opener
    from urllib.request import install_opener
    from urllib.error import HTTPError
    from urllib.parse import urlencode
    from urllib.parse import urlsplit
    from urllib.parse import urlparse
    from urllib.request import FancyURLopener
    from urllib.request import URLopener

    import tkinter as Tkinter
    from tkinter import Tk, Text
    from tkinter.font import Font as tkFont

    import configparser as ConfigParser

    from collections import UserDict

    viewvalues = operator.methodcaller("values")

    import io

    StringIO = io.StringIO
    BytesIO = io.BytesIO

    def b(s):
        return s.encode("latin-1")

    def u(s):
        return s
