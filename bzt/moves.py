# pylint: skip-file
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


    def iteritems(dictionary, **kw):
        return iter(dictionary.iteritems(**kw))


    urlopen = __import__("urllib2", fromlist=("urlopen")).urlopen
    urlencode = __import__("urllib", fromlist=("urlencode")).urlencode
    urlsplit = __import__("urlparse", fromlist=("urlsplit")).urlsplit
    urlparse = __import__("urlparse", fromlist=("urlparse")).urlparse
    build_opener = __import__("urllib2", fromlist=("build_opener")).build_opener
    install_opener = __import__("urllib2", fromlist=("install_opener")).install_opener

    Request = __import__("urllib2", fromlist=("Request")).Request
    ProxyHandler = __import__("urllib2", fromlist=("ProxyHandler")).ProxyHandler
    HTTPError = __import__("urllib2", fromlist=("HTTPError")).HTTPError
    FancyURLopener = __import__("urllib", fromlist=("FancyURLopener")).FancyURLopener
    URLopener = __import__("urllib", fromlist=("URLopener")).URLopener
    ConfigParser = __import__("ConfigParser")
    Tkinter = __import__("Tkinter")
    TkMoved = Tkinter.Tk
    Text = Tkinter.Text
    tkFont = __import__("tkFont", fromlist=("Font")).Font
    UserDict = __import__("UserDict").UserDict

    viewvalues = operator.methodcaller("viewvalues")

    StringIO = BytesIO = __import__("StringIO", fromlist=("StringIO")).StringIO


    def to_bytes(string):
        return string


    def to_unicode(string):
        return unicode(string.replace(r'\\', r'\\\\'), "unicode_escape")

else:
    string_types = str,
    integer_types = int,
    class_types = type,
    text_type = str
    binary_type = bytes


    def iteritems(dictionary, **kw):
        return iter(dictionary.items(**kw))


    urlopen = __import__("urllib.request", fromlist=("urlopen")).urlopen
    urlencode = __import__("urllib.parse", fromlist=("urlencode")).urlencode
    urlsplit = __import__("urllib.parse", fromlist=("urlsplit")).urlsplit
    urlparse = __import__("urllib.parse", fromlist=("urlparse")).urlparse
    build_opener = __import__("urllib.request", fromlist=("urlopen")).build_opener
    install_opener = __import__("urllib.request", fromlist=("install_opener")).install_opener

    Request = __import__("urllib.request", fromlist=("Request")).Request
    ProxyHandler = __import__("urllib.request", fromlist=("ProxyHandler")).ProxyHandler
    HTTPError = __import__("urllib.error", fromlist=("HTTPError")).HTTPError
    FancyURLopener = __import__("urllib.request", fromlist=("FancyURLopener")).FancyURLopener
    URLopener = __import__("urllib.request", fromlist=("URLopener")).URLopener
    Tkinter = __import__("tkinter")
    _Tkinter = __import__("tkinter", fromlist=("Tk", "Text"))
    TkMoved = _Tkinter.Tk
    Text = _Tkinter.Text
    tkFont = __import__("tkinter.font", fromlist=("Font")).Font
    ConfigParser = __import__("configparser")
    UserDict = __import__("collections", fromlist=("UserDict")).UserDict

    viewvalues = operator.methodcaller("values")

    _io = __import__("io", fromlist=("StringIO", "BytesIO"))
    StringIO = _io.StringIO
    BytesIO = _io.BytesIO


    def to_bytes(string):
        return string.encode("latin-1")


    def to_unicode(string):
        return string
