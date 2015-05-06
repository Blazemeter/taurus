"""
Every project needs its trash heap of miscellaneous functions and classes

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
from collections import defaultdict, Counter
import csv
import json
import logging
import os
import platform
import random
import re
import shlex
from subprocess import PIPE
import mimetypes
import itertools
import zipfile
import sys
from psutil import Popen
import six


def run_once(f):
    """
    A decorator to run function only once

    :type f: __builtin__.function
    :return:
    """

    def wrapper(*args, **kwargs):
        """
        :param kwargs:
        :param args:
        """
        if not wrapper.has_run:
            wrapper.has_run = True
            return f(*args, **kwargs)

    wrapper.has_run = False
    return wrapper


def dehumanize_time(str_time):
    """
    Convert value like 1d4h33m12s103ms into seconds

    :param str_time: string to convert
    :return: float value in seconds
    :raise ValueError: in case of unsupported unit
    """
    parser = re.compile('([\d\.]+)([a-zA-Z]*)')
    parts = parser.findall(str(str_time).replace(' ', ''))
    result = 0.0
    for value, unit in parts:
        value = float(value)
        unit = unit.lower()
        if unit == 'ms':
            result += value / 1000.0
            continue
        elif unit == 's' or unit == '':
            result += value
            continue
        elif unit == 'm':
            result += value * 60
            continue
        elif unit == 'h':
            result += value * 60 * 60
            continue
        elif unit == 'd':
            result += value * 60 * 60 * 24
            continue
        else:
            msg = "String contains unsupported unit %s: %s"
            raise ValueError(msg % (unit, str_time))
    return result


class BetterDict(defaultdict):
    """
    Wrapper for defaultdict that able to deep merge other dicts into itself

    :param kwargs:
    """

    def __init__(self, **kwargs):
        super(BetterDict, self).__init__(**kwargs)
        self.log = logging.getLogger(self.__class__.__name__)

    def get(self, key, default=defaultdict):
        """
        Change get with setdefault

        :type key: object
        :type default: object
        """
        if default == defaultdict:
            default = BetterDict()

        value = self.setdefault(key, default)
        if isinstance(value, six.string_types):
            if isinstance(value, str):  # this is a trick for python v2/v3 compatibility
                return value
            else:
                return value.encode()
        else:
            return value

    def merge(self, src):
        """
        Deep merge other dict into current
        '-'  - overwrite operation prefix for dict key

        :type src: dict
        """
        if not isinstance(src, dict):
            raise ValueError("Loaded object is not dict: %s" % src)

        for key, val in six.iteritems(src):
            if len(key) and key[0] == '~':  # overwrite flag
                if key[1:] in self:
                    self.pop(key[1:])
                key = key[1:]
                self.log.debug("Overridden key: %s", key)

            if len(key) and key[0] == '^':  # eliminate flag
                # TODO: improve logic - use val contents to see what to eliminate
                self.pop(key[1:])
                self.log.debug("Removed key: %s", key)
                continue

            if isinstance(val, dict):
                dst = self.get(key)
                if isinstance(dst, BetterDict):
                    dst.merge(val)
                elif isinstance(dst, Counter):
                    self[key] += val
                elif isinstance(dst, dict):
                    raise ValueError("Mix of DictOfDict and dict is forbidden")
                else:
                    self.log.warning("Overwritten key: %s", key)
                    self[key] = val
            elif isinstance(val, list):
                self.__ensure_list_type(val)
                if key not in self:
                    self[key] = []
                if isinstance(self[key], list):
                    self[key].extend(val)
                else:
                    self.log.warning("Overridden key: %s", key)
                    self[key] = val
            else:
                self[key] = val

        return

    def __ensure_list_type(self, values):
        for idx, obj in enumerate(values):
            if isinstance(obj, dict):
                values[idx] = BetterDict()
                values[idx].merge(obj)
            elif isinstance(obj, list):
                self.__ensure_list_type(obj)

    @classmethod
    def traverse(cls, obj, visitor):
        """
        Deep traverse dict with visitor

        :type obj: list or dict or object
        :type visitor: callable
        """
        if isinstance(obj, dict):
            visitor(obj)
            for key, val in six.iteritems(obj):
                cls.traverse(val, visitor)
        elif isinstance(obj, list):
            for val in obj:
                cls.traverse(val, visitor)


def shell_exec(args, cwd=None, stdout=PIPE, stderr=PIPE, stdin=PIPE):
    """
    Wrapper for subprocess starting

    :param stderr:
    :param stdout:
    :param cwd:
    :param stdin:
    :type args: basestring or list
    :return:
    """

    if isinstance(args, six.string_types):
        args = shlex.split(args)
    logging.getLogger(__name__).debug("Executing shell: %s", args)
    if platform.system() == 'Windows':
        return Popen(args, stdout=stdout, stderr=stderr, stdin=stdin, bufsize=0, cwd=cwd)
    else:
        return Popen(args, stdout=stdout, stderr=stderr, stdin=stdin, bufsize=0,
                     preexec_fn=os.setpgrp, close_fds=True, cwd=cwd)


def ensure_is_dict(container, key, default_key=None):
    """
    Ensure that dict item is dict, convert if needed

    :type container: dict or list
    :type key: basestring or int
    :type default_key: basestring
    :return:
    """
    if (isinstance(container, dict) and key not in container) \
            or (isinstance(container, list) and not container[key]):
        if default_key:
            container[key] = BetterDict()
            container[key][default_key] = None
        else:
            container[key] = BetterDict()
    elif not isinstance(container[key], dict):
        if default_key:
            val = container[key]
            container[key] = BetterDict()
            container[key][default_key] = val
        else:
            container[key] = BetterDict()

    return container[key]


def dict_key(dictnr, value):
    """
    Search key by value in dict

    :type dictnr: dict
    :type value: type
    :return: :raise KeyError:
    """
    for key, val in six.iteritems(dictnr):
        if val == value:
            return key
    raise KeyError("Value not found in dict: %s" % value)


class MultiPartForm(object):
    """
    Accumulate the data to be used when posting a form.
    http://blog.doughellmann.com/2009/07/
        pymotw-urllib2-library-for-opening-urls.html

    :type form_fields: list[str,str]
    """

    def __init__(self):
        self.form_fields = []
        self.files = []
        self.boundary = make_boundary()

    def get_content_type(self):
        """ returns content type """
        return 'multipart/form-data; boundary=%s' % self.boundary

    def add_field(self, name, value):
        """
        Add a simple field to the form data.
        :type name: str
        :type value: str
        """
        self.form_fields.append((name, value))

    def add_file_as_string(self, fieldname, filename, body, mimetype=None):
        """ add raw string file
        :type fieldname: str
        :type filename: str
        :type body: str | bytes
        :type mimetype: str
        """
        default = 'application/octet-stream'
        if mimetype is None:
            mimetype = mimetypes.guess_type(filename)[0] or default

        # if isinstance(fieldname, six.u()):
        # fieldname = fieldname.encode()

        # if isinstance(body, str):
        # body = body.encode()

        self.files.append((fieldname, filename, mimetype, body))

    def add_file(self, fieldname, filename, file_handle=None, mimetype=None):
        """Add a file to be uploaded.
        :type mimetype: str
        :type file_handle: file
        :type filename: str
        :type fieldname: str
        """
        if not file_handle:
            with open(filename, 'rb') as fds:
                body = fds.read()

            filename = os.path.basename(filename)
        else:
            body = file_handle.read()
        self.add_file_as_string(fieldname, filename, body, mimetype)

    def __convert_to_list(self):
        """Return a string representing the form, including attached files."""
        # Build a list of lists, each containing "lines" of the
        # request.  Each part is separated by a boundary string.
        # Once the list is built, return a string where each
        # line is separated by '\r\n'.
        parts = []
        part_boundary = '--' + self.boundary

        # Add the form fields
        parts.extend(
            [part_boundary, 'Content-Disposition: form-data; name="%s"' % name, '', value, ]
            for name, value in self.form_fields
        )

        # Add the files to upload
        parts.extend(
            [part_boundary,
             'Content-Disposition: file; name="%s"; filename="%s"' % (field_name, filename),
             'Content-Type: %s' % content_type, '', body, "\r\n"]
            for field_name, filename, content_type, body in self.files
        )

        # Flatten the list and add closing boundary marker,
        # then return CR+LF separated data
        flattened = list(itertools.chain(*parts))
        flattened.append('--' + self.boundary + '--')
        # flattened.append('')
        # return b'\r\n'.join(x.encode() if isinstance(x, str) else x for x in flattened)
        return flattened

    def form_as_bytes(self):
        """
        represents form contents as bytes in python3 or 8-bit str in python2
        """
        result_list = []
        for x in self.__convert_to_list():
            # if (8-bit str (2.7) or bytes (3.x), then no processing, just add, else - encode)
            if isinstance(x, six.binary_type):
                result_list.append(x)
            elif isinstance(x, six.text_type):
                result_list.append(x.encode())
            else:
                raise BaseException

        res_bytes = six.b("\r\n").join(result_list)
        res_bytes += six.b("\r\n")
        return res_bytes
        # return b'\r\n'.join(x.encode() if isinstance(x, str) else x for x in self.__convert_to_list())


def to_json(obj):
    """
    Convert object into indented json

    :param obj:
    :return:
    """

    return json.dumps(obj, indent=True, cls=ComplexEncoder)


class JSONDumpable(object):
    """
    Marker class for json dumpable classes
    """
    pass


class ComplexEncoder(json.JSONEncoder):
    """
    Magic class to help serialize in JSON any object.
    """
    TYPES = [dict, list, tuple, six.text_type, six.string_types, six.integer_types, float, bool, type(None)]

    def default(self, obj):
        """
        Filters out protected and private fields

        :param obj:
        :return:
        """

        if self.__dumpable(obj):
            res = {}
            for key, val in six.iteritems(obj.__dict__):
                if not self.__dumpable(val):
                    # logging.debug("Filtered out: %s.%s", key, val)
                    pass
                elif key.startswith('_'):
                    # logging.debug("Filtered out: %s", key)
                    pass
                else:
                    res[key] = val
            return res
        else:
            return None

    def __dumpable(self, obj):
        """
        Re

        :param obj:
        :rtype: bool
        """
        for atype in self.TYPES + [JSONDumpable]:
            if isinstance(obj, atype):
                return True
        return False

    @classmethod
    def of_basic_type(cls, val):
        """
        Returns true if val is of basic type

        :param val:
        :return:
        """
        for atype in cls.TYPES:
            if isinstance(val, atype):
                return True
        return False


def humanize_time(secs):
    """
    taken from http://testingreflections.com/node/6534

    :param secs:
    :return:
    """
    mins, secs = divmod(secs, 60)
    hours, mins = divmod(mins, 60)
    return '%02d:%02d:%02d' % (hours, mins, secs)


def guess_csv_delimiter(header):
    """ completely arbitrary fn to detect the delimiter

    :type header: str
    :raise ValueError:
    :rtype: str
    """
    possible_delims = "\t;|:,"
    lines = header.split("\n")
    if len(lines) < 2:
        raise ValueError("CSV header must contain at least 1 line")

    dialect = csv.Sniffer().sniff(header, delimiters=possible_delims)
    return dialect.delimiter


def load_class(full_name):
    """
    Load class by its full name like bzt.cli.CLI

    :type full_name: str
    :return:
    :rtype: callable
    """
    module_name = full_name[:full_name.rfind('.')]
    class_name = full_name[full_name.rfind('.') + 1:]
    logging.debug("Importing module: %s", module_name)
    module = __import__(module_name)
    for mod in module_name.split('.')[1:]:
        module = getattr(module, mod)

    logging.debug("Loading class: '%s' from %s", class_name, module)
    return getattr(module, class_name)


def unzip(source_filename, dest_dir, rel_path=None):
    """
    :param source_filename:
    :param dest_dir:
    :param rel_path:
    :return:
    """
    logging.debug("Extracting %s to %s", source_filename, dest_dir)

    with zipfile.ZipFile(source_filename) as zf:
        for member in zf.infolist():
            if rel_path:
                if not member.filename.startswith(rel_path):
                    continue
                else:
                    member.filename = member.filename[len(rel_path) + 1:]

            if not member.filename:
                continue

            # Path traversal defense copied from
            # http://hg.python.org/cpython/file/tip/Lib/http/server.py#l789
            logging.debug("Writing %s%s%s", dest_dir, os.path.sep, member.filename)

            zf.extract(member, dest_dir)


def download_progress_hook(blocknum, blocksize, totalsize):
    """
    displays download progress, invoked by UrlOpener() or it's subclasses.
    mocked as tests.mocks.download_progress_mock
    
    :return:
    
    """
    if not callable(getattr(sys.stdout, 'isatty')):
        return

    if not sys.stdout.isatty():
        return

    readsofar = blocknum * blocksize
    if totalsize > 0:
        percent = readsofar * 100 / totalsize
        # TODO: fix bug when downloaded size < totalsize at 100%.
        # or just skip downloaded output
        s = "\r%5.1f%% %*d of %d" % (
            percent, len(str(totalsize)), readsofar, totalsize)
        sys.stdout.write(s)
        if readsofar >= totalsize:  # near the end
            sys.stderr.write("\n")
    else:
        sys.stdout.write("read %d\n" % (readsofar,))


def make_boundary(text=None):
    _width = len(repr(sys.maxsize - 1))
    _fmt = '%%0%dd' % _width
    token = random.randrange(sys.maxsize)
    boundary = ('=' * 15) + (_fmt % token) + '=='
    if text is None:
        return boundary
    b = boundary
    counter = 0
    while True:
        cre = re.compile('^--' + re.escape(b) + '(--)?$', re.MULTILINE)
        if not cre.search(text):
            break
        b = boundary + '.' + str(counter)
        counter += 1
    return b


def get_configs_dir():
    path = os.getenv("VIRTUAL_ENV", "") \
        if os.getenv("VIRTUAL_ENV", "") \
        else os.path.splitdrive(sys.executable)[0]
    path += os.path.sep + os.path.join("etc", "bzt.d")  # os.path.join does not work for some reason
    return path