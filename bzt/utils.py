# coding=utf-8
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
import codecs
import copy
import csv
import fnmatch
import ipaddress
import itertools
import json
import locale
import logging
import math
import mimetypes
import operator
import os
import platform
import random
import re
import shlex
import shutil
import signal
import subprocess
import sys
import tarfile
import tempfile
import time
import traceback
import webbrowser
import zipfile
from abc import abstractmethod
from collections import defaultdict, Counter
from contextlib import contextmanager
from distutils.version import LooseVersion
from io import IOBase
from ssl import SSLError
from subprocess import CalledProcessError, PIPE, check_output, STDOUT
from urllib import parse
from urllib.error import URLError
from urllib.request import url2pathname
from webbrowser import GenericBrowser

import psutil
import requests
import requests.adapters
from lxml import etree
from progressbar import ProgressBar, Percentage, Bar, ETA
from requests.exceptions import ReadTimeout
from urwid import BaseScreen

from bzt import TaurusInternalException, TaurusNetworkError, ToolError, TaurusConfigError

LOG = logging.getLogger("")
CALL_PROBLEMS = (CalledProcessError, OSError)
NETWORK_PROBLEMS = (IOError, URLError, SSLError, ReadTimeout, TaurusNetworkError)
numeric_types = (int, float, complex)
viewvalues = operator.methodcaller("values")


def unicode_decode(string, errors="strict"):
    if isinstance(string, bytes):
        return string.decode("utf-8", errors)
    else:
        return string


def communicate(proc):
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


def sync_run(args, env=None):
    output = check_output(args, env=env, stderr=STDOUT)
    return stream_decode(output).rstrip()


def temp_file(suffix="", prefix="tmp", dir=None):
    """ Creates temporary file, returns name of it. User is responsible for deleting the file """
    fd, fname = tempfile.mkstemp(suffix=suffix, prefix=prefix, dir=dir)
    os.close(fd)
    return fname


def simple_body_dict(dic):
    """ body dict must have just one level for sending with form params"""
    if isinstance(dic, dict):
        for key in dic:
            if not isinstance(dic[key], (str, numeric_types)):
                return False
        return True
    return False


def get_full_path(path, default=None, step_up=0):
    """
    Function expands '~' and adds cwd to path if it's not absolute (relative)
    Target doesn't have to exist

    :param path:
    :param default:
    :param step_up:
    :return:
    """
    if not path:
        return default

    res = os.path.abspath(os.path.expanduser(path))
    for _ in range(step_up):
        res = os.path.dirname(res)
    return res


BZT_DIR = get_full_path(__file__, step_up=1)
RESOURCES_DIR = os.path.join(BZT_DIR, "resources")


def get_files_recursive(dir_name, exclude_mask=''):
    for root, _, files in os.walk(dir_name):
        for _file in files:
            if not fnmatch.fnmatch(_file, exclude_mask):
                yield os.path.join(root, _file)


def parse_java_version(versions):
    if versions:
        version = versions[0]

        if LooseVersion(version) > LooseVersion("6"):  # start of openjdk naming
            major = re.findall("^([\d]*)", version)
        else:
            major = re.findall("\.([\d]*)", version)

        if major:
            return major[0]


def run_once(func):
    """
    A decorator to run function only once

    :type func: __builtin__.function
    :return:
    """

    def wrapper(*args, **kwargs):
        """
        :param kwargs:
        :param args:
        """
        if not wrapper.has_run:
            wrapper.has_run = True
            return func(*args, **kwargs)

    wrapper.has_run = False
    return wrapper


def replace_in_config(config, samples, substitutes, log=None):
    def file_replacer(value, key, container):
        if value in samples:
            container[key] = substitutes[samples.index(value)]
            if container[key] != value and log:
                log.debug("Replaced %s with %s", value, container[key])

    BetterDict.traverse(config, file_replacer)


def dehumanize_time(str_time):
    """
    Convert value like 1d4h33m12s103ms into seconds

    Also, incidentally translates strings like "inf" into float("inf")

    :param str_time: string to convert
    :return: float value in seconds
    :raise TaurusInternalException: in case of unsupported unit
    """
    if not str_time:
        return 0

    parser = re.compile(r'([\d\.\-infa]+)([a-zA-Z]*)')
    parts = parser.findall(str(str_time).replace(' ', ''))

    if len(parts) == 0:
        msg = "String format not supported: %s"
        raise TaurusInternalException(msg % str_time)

    result = 0.0
    for value, unit in parts:
        try:
            value = float(value)
        except ValueError:
            raise TaurusInternalException("Unsupported float string: %r" % value)
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
            raise TaurusInternalException(msg % (unit, str_time))
    return result


def get_bytes_count(str_bytes):
    if not str_bytes:
        return 0

    parser = re.compile(r'([\d\.]+)([a-zA-Z]*)')
    parts = parser.findall(str(str_bytes).replace(' ', ''))

    if len(parts) != 1:
        msg = "String format not supported: %s"
        raise TaurusConfigError(msg % str_bytes)

    value, unit = parts[0]
    try:
        value = float(value)
    except ValueError:
        raise TaurusConfigError("Unsupported float string: %r" % value)

    unit = unit.lower()
    if unit in ('', 'b'):
        result = value
    elif unit in ('k', 'kb', 'kib'):
        result = value * 1024
    elif unit in ('m', 'mb', 'mib'):
        result = value * 1024 * 1024
    else:
        msg = "String contains unsupported unit %s: %s"
        raise TaurusConfigError(msg % (unit, str_bytes))
    return result


class BetterDict(defaultdict):
    """
    Wrapper for defaultdict that able to deep merge other dicts into itself
    """

    @classmethod
    def from_dict(cls, orig):
        """
        # https://stackoverflow.com/questions/50013768/how-can-i-convert-nested-dictionary-to-defaultdict/50013806
        """
        if isinstance(orig, dict):
            return cls(lambda: None, {k: cls.from_dict(v) for k, v in orig.items()})
        elif isinstance(orig, list):
            return [cls.from_dict(e) for e in orig]
        else:
            return orig

    def get(self, key, default=defaultdict, force_set=False):
        """
        Change get with setdefault

        :param force_set:
        :type key: object
        :type default: object
        """
        if default == defaultdict:
            default = BetterDict()

        if isinstance(default, BaseException) and key not in self:
            raise default

        if force_set:
            value = self.setdefault(key, default)
        else:
            value = defaultdict.get(self, key, default)

        return value

    def merge(self, src):
        """
        Deep merge other dict into current
        :type src: dict
        """

        if not isinstance(src, dict):
            raise TaurusInternalException("Loaded object is not dict [%s]: %s" % (src.__class__, src))

        for key, val in iteritems(src):

            prefix = ""
            if key[0] in ("^", "~", "$"):  # modificator found
                prefix = key[0]
                key = key[1:]

            if prefix == "^":  # eliminate flag
                if key in self:
                    self.pop(key)
                continue
            elif prefix == "~":  # overwrite flag
                if key in self:
                    self.pop(key)

            if isinstance(val, dict):
                self.__add_dict(key, val)
            elif isinstance(val, list):
                self.__add_list(key, val, merge_list_items=(prefix == "$"))
            else:
                self[key] = val

        return self

    def __add_dict(self, key, val):
        dst = self.get(key, force_set=True)
        if isinstance(dst, BetterDict):
            dst.merge(val)
        elif isinstance(dst, Counter):
            self[key] += val
        elif isinstance(dst, dict):
            raise TaurusInternalException("Mix of DictOfDict and dict is forbidden")
        else:
            self[key] = BetterDict.from_dict(val)

    def __add_list(self, key, val, merge_list_items):
        self.__ensure_list_type(val)
        if key not in self:
            self[key] = []
        if not isinstance(self[key], list):
            self[key] = val
            return

        if merge_list_items:
            left = self[key]
            right = val
            for index, righty in enumerate(right):
                if index < len(left):
                    lefty = left[index]
                    if isinstance(lefty, BetterDict) and isinstance(righty, BetterDict):
                        lefty.merge(righty)
                    else:
                        LOG.warning("Overwriting the value of %r when merging configs", key)
                        left[index] = righty
                else:
                    left.insert(index, righty)
        else:
            self[key].extend(val)

    def __ensure_list_type(self, values):
        """
        Ensure that values is a list, convert if needed
        :param values: dict or list
        :return:
        """
        for idx, obj in enumerate(values):
            if isinstance(obj, dict):
                values[idx] = BetterDict.from_dict(obj)
            elif isinstance(obj, list):
                self.__ensure_list_type(obj)

    @classmethod
    def traverse(cls, obj, visitor):
        """
        Deep traverse dict with visitor. If visitor returns any value, don't traverse into

        :type obj: list or dict or object
        :type visitor: callable
        """
        if isinstance(obj, dict):
            for key, val in iteritems(obj):
                if not visitor(val, key, obj):
                    cls.traverse(obj[key], visitor)
        elif isinstance(obj, list):
            for idx, val in enumerate(obj):
                if not visitor(val, idx, obj):
                    cls.traverse(obj[idx], visitor)

    def filter(self, rules, black_list=False):
        keys = set(self.keys())
        for key in keys:
            ikey = "!" + key
            if (key in rules) or (ikey in rules):  # we have rule for this key
                current_black_list = black_list if key in rules else not black_list
                rkey = key if key in rules else ikey

                if isinstance(rules.get(rkey), dict):
                    if isinstance(self.get(key), BetterDict):  # need to go deeper
                        self.get(key).filter(rules[rkey], black_list=current_black_list)
                    elif not current_black_list:
                        del self[key]
                elif current_black_list:
                    del self[key]  # must be blacklisted
            elif not black_list:
                del self[key]  # remove unknown key

            current = self.get(key, None)
            if isinstance(current, (dict, list)) and not current:
                del self[key]  # clean empty

    def __repr__(self):
        return dict(self).__repr__()


def get_uniq_name(directory, prefix, suffix="", forbidden_names=()):
    base = os.path.join(directory, prefix)
    diff = ""
    num = 0
    while os.path.exists(base + diff + suffix) or base + diff + suffix in forbidden_names:
        num += 1
        diff = "-%s" % num

    return base + diff + suffix


class TaurusCalledProcessError(CalledProcessError):
    def __init__(self, *args, **kwargs):
        """ join output and stderr for compatibility """
        output = ""
        if "output" in kwargs:
            output += u"\n>>> {out_start} >>>\n{out}\n<<< {out_end} <<<\n".format(
                out_start="START OF STDOUT", out=kwargs["output"], out_end="END OF STDOUT")

        if "stderr" in kwargs:
            output += u"\n>>> {err_start} >>>\n{err}\n<<< {err_end} <<<\n".format(
                err_start="START OF STDERR", err=kwargs.pop("stderr"), err_end="END OF STDERR")

        if output:
            kwargs["output"] = output

        super(TaurusCalledProcessError, self).__init__(*args, **kwargs)

    def __str__(self):
        base_str = super(TaurusCalledProcessError, self).__str__()

        if self.output:
            base_str += '\n' + self.output

        return base_str


def exec_and_communicate(*args, **kwargs):
    process = shell_exec(*args, **kwargs)
    out, err = communicate(process)

    if process.returncode != 0:
        raise TaurusCalledProcessError(process.returncode, cmd=args[0], output=out, stderr=err)

    return out, err


def shell_exec(args, cwd=None, stdout=PIPE, stderr=PIPE, stdin=PIPE, shell=False, env=None, pgrp=True):
    """
    Wrapper for subprocess starting

    """
    if stdout and not isinstance(stdout, (int, IOBase)):
        LOG.warning("stdout is not IOBase: %s", stdout)
        stdout = None

    if stderr and not isinstance(stderr, (int, IOBase)):
        LOG.warning("stderr is not IOBase: %s", stderr)
        stderr = None

    if isinstance(args, str) and not shell:
        args = shlex.split(args, posix=not is_windows())
    LOG.debug("Executing shell: %s at %s", args, cwd or os.curdir)

    kwargs = {
        "stdout": stdout,
        "stderr": stderr,
        "stdin": stdin,
        "bufsize": 0,
        "cwd": cwd,
        "shell": shell,
        "env": env
    }

    if is_windows():
        if pgrp:
            kwargs["creationflags"] = subprocess.CREATE_NEW_PROCESS_GROUP
        return psutil.Popen(args, **kwargs)
    else:
        kwargs["close_fds"] = True
        if pgrp:
            kwargs["preexec_fn"] = os.setpgrp
        return psutil.Popen(args, **kwargs)


class Environment(object):
    def __init__(self, log=None, parent=None):
        self.data = {}
        self._queue = []

        log = log or LOG
        self.log = log.getChild(self.__class__.__name__)

        if parent:
            self._queue.extend(
                [(self.__getattribute__(method), args, kwargs) for method, args, kwargs in parent.get_queue()])

    def get_queue(self):
        return [(method.__name__, args, kwargs) for method, args, kwargs in self._queue]

    def set(self, *args, **kwargs):
        self._add_to_queue(self._set, *args, **kwargs)

    def add_path(self, *args, **kwargs):
        self._add_to_queue(self._add_path, *args, **kwargs)

    def add_java_param(self, *args, **kwargs):
        self._add_to_queue(self._add_java_param, *args, **kwargs)

    def update(self, *args, **kwargs):
        self._add_to_queue(self._update, *args, **kwargs)

    def _add_to_queue(self, *args, **kwargs):
        self._queue.append((args[0], args[1:], kwargs))

    def _set(self, env):
        """
        :type env: dict
        """
        for key in env:
            key = str(key)
            val = env[key]

            if is_windows():
                key = key.upper()

            if key in self.data:
                if val is None:
                    self.log.debug("Remove '%s' from environment", key)
                    self.data.pop(key)
                else:
                    self.log.debug("Replace '%s' in environment", key)
                    self.data[key] = str(val)
            else:
                self._add({key: val}, '', finish=False)

    def _add_path(self, pair, finish=False):
        self._add(pair, os.pathsep, finish)

    def _add_java_param(self, pair, finish=False):
        self._add(pair, " ", finish)

    def _update(self, env):  # compatibility with taurus-server
        self.set(env)

    def _add(self, pair, separator, finish):
        for key in pair:
            val = pair[key]
            key = str(key)
            if is_windows():
                key = key.upper()

            if val is None:
                self.log.debug("Skip empty variable '%s'", key)
                return

            val = str(val)

            if key in self.data:
                if finish:
                    self.data[key] += separator + val  # add to the end
                else:
                    self.data[key] = val + separator + self.data[key]  # add to the beginning
            else:
                self.data[key] = str(val)

    def get(self, key=None):
        self._apply_queue()

        if key:
            key = str(key)
            if is_windows():
                key = key.upper()

            return self.data.get(key, None)
        else:
            # full environment
            return copy.deepcopy(self.data)

    def _apply_queue(self):
        self.data = {}
        self._set(os.environ)
        for method, args, kwargs in self._queue:
            method(*args, **kwargs)


class FileReader(object):
    SYS_ENCODING = locale.getpreferredencoding()

    def __init__(self, filename="", file_opener=None, parent_logger=None):
        self.fds = None
        if parent_logger:
            self.log = parent_logger.getChild(self.__class__.__name__)
        else:
            self.log = logging.getLogger(self.__class__.__name__)

        if file_opener:
            self.file_opener = file_opener  # external method for opening of file
        else:
            self.file_opener = lambda f: open(f, mode='rb')  # default mode is binary

        # for non-trivial openers filename must be empty (more complicate than just open())
        # it turns all regular file checks off, see is_ready()
        self.name = filename
        self.cp = 'utf-8'  # default code page is utf-8
        self.decoder = codecs.lookup(self.cp).incrementaldecoder()
        self.fallback_decoder = codecs.lookup(self.SYS_ENCODING).incrementaldecoder(errors='ignore')
        self.offset = 0

    def _readlines(self, hint=None):
        # get generator instead of list (in regular readlines())
        length = 0
        for line in self.fds:
            yield line
            if hint and hint > 0:
                length += len(line)
                if length >= hint:
                    return

    def is_ready(self):
        if not self.fds:
            if self.name:
                if not os.path.isfile(self.name):
                    self.log.debug("File not appeared yet: %s", self.name)
                    return False
                if not os.path.getsize(self.name):
                    self.log.debug("File is empty: %s", self.name)
                    return False

                self.log.debug("Opening file: %s", self.name)

            # call opener regardless of the name value as it can use empty name as flag
            self.fds = self.file_opener(self.name)

        if self.fds:
            self.name = self.fds.name
            return True

    def _decode(self, line, last_pass=False):
        try:
            return self.decoder.decode(line, final=last_pass)
        except UnicodeDecodeError:
            self.log.warning("Content encoding of '%s' doesn't match %s", self.name, self.cp)
            self.cp = self.SYS_ENCODING
            self.decoder = self.fallback_decoder
            self.decoder.reset()
            self.log.warning("Proposed code page: %s", self.cp)
            return self.decoder.decode(line, final=last_pass)

    def get_lines(self, size=-1, last_pass=False):
        if self.is_ready():
            if last_pass:
                size = -1
            self.fds.seek(self.offset)
            for line in self._readlines(hint=size):
                self.offset += len(line)
                yield self._decode(line, last_pass)

    def get_line(self):
        line = ""
        if self.is_ready():
            self.fds.seek(self.offset)
            line = self.fds.readline()
            self.offset += len(line)

        return self._decode(line)

    def get_bytes(self, size=-1, last_pass=False, decode=True):
        if self.is_ready():
            if last_pass:
                size = -1
            self.fds.seek(self.offset)
            _bytes = self.fds.read(size)
            self.offset += len(_bytes)
            if decode:
                return self._decode(_bytes, last_pass)
            else:
                return _bytes

    def __del__(self):
        self.close()

    def close(self):
        if self.fds:
            self.fds.close()


def ensure_is_dict(container, key, sub_key):
    """
    Ensure that dict item is dict, convert if needed

    :type container: dict or list
    :type key: basestring or int
    :type sub_key: basestring
    :return:
    """
    if isinstance(container, BetterDict):
        container.get(key, force_set=True)
    elif isinstance(container, dict):  # todo: remove after fixing merge
        container[key] = BetterDict()

    if not isinstance(container[key], dict):  # todo: replace dict with BetterDict after fixing merge
        container[key] = BetterDict.from_dict({sub_key: container[key]})

    return container[key]


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
             'Content-Type: %s' % content_type, '', body]
            for field_name, filename, content_type, body in self.files
        )

        # Flatten the list and add closing boundary marker,
        # then return CR+LF separated data
        flattened = list(itertools.chain(*parts))
        flattened.append('--' + self.boundary + '--')
        return flattened

    def form_as_bytes(self):
        """
        represents form contents as bytes
        """
        result_list = []
        for item in self.__convert_to_list():
            # if (bytes (3.x), then no processing, just add, else - encode)
            if isinstance(item, bytes):
                result_list.append(item)
            elif isinstance(item, str):
                result_list.append(item.encode())
            else:
                raise TaurusInternalException("Unhandled form data type: %s" % type(item))

        res_bytes = b("\r\n").join(result_list)
        res_bytes += b("\r\n")
        return res_bytes


def to_json(obj, indent=True):
    """
    Convert object into indented json

    :param indent: whether to generate indented JSON
    :param obj: object to convert
    :return:
    """
    return json.dumps(obj, indent=indent, cls=ComplexEncoder)


class JSONDumpable(object):
    """
    Marker class for json dumpable classes
    """
    pass


class JSONConvertible(object):
    @abstractmethod
    def __json__(self):
        "Convert class instance into JSON-dumpable structure (e.g. dict)"
        pass


class ComplexEncoder(json.JSONEncoder):
    """
    Magic class to help serialize in JSON any object.
    """
    TYPES = (dict, list, tuple, str, int, float, bool, type(None))

    def default(self, obj):  # pylint: disable=method-hidden
        """
        Filters out protected and private fields

        :param obj:
        :return:
        """

        if self.__dumpable(obj):
            res = {}
            for key, val in iteritems(obj.__dict__):
                if not self.__dumpable(val):
                    # logging.debug("Filtered out: %s.%s", key, val)
                    pass
                elif key.startswith('_'):
                    # logging.debug("Filtered out: %s", key)
                    pass
                else:
                    res[key] = val
            return res
        elif ComplexEncoder.__convertible(obj):
            return obj.__json__()
        else:
            return None

    @classmethod
    def __dumpable(cls, obj):
        """
        Re

        :param obj:
        :rtype: bool
        """
        dumpable_types = tuple(cls.TYPES + (JSONDumpable,))
        return isinstance(obj, dumpable_types)

    @staticmethod
    def __convertible(obj):
        return isinstance(obj, JSONConvertible)

    @classmethod
    def of_basic_type(cls, val):
        """
        Returns true if val is of basic type

        :param val:
        :return:
        """
        return isinstance(val, cls.TYPES)


def humanize_time(secs):
    """
    taken from http://testingreflections.com/node/6534

    :param secs:
    :return:
    """
    mins, secs = divmod(secs, 60)
    hours, mins = divmod(mins, 60)
    return '%02d:%02d:%02d' % (hours, mins, secs)


def guess_csv_dialect(header, force_doublequote=False):
    """ completely arbitrary fn to detect the delimiter

    :param force_doublequote: bool
    :type header: str
    :rtype: csv.Dialect
    """
    possible_delims = ",;\t"
    dialect = csv.Sniffer().sniff(header, delimiters=possible_delims)

    if force_doublequote:
        dialect.doublequote = True
    return dialect


def load_class(full_name):
    """
    Load class by its full name like bzt.cli.CLI

    :type full_name: str
    :return:
    :rtype: callable
    """
    module_name = full_name[:full_name.rfind('.')]
    class_name = full_name[full_name.rfind('.') + 1:]
    LOG.debug("Importing module: %s", module_name)
    module = __import__(module_name)
    for mod in module_name.split('.')[1:]:
        module = getattr(module, mod)

    LOG.debug("Loading class: '%s' from %s", class_name, module)
    return getattr(module, class_name)


def unzip(source_filename, dest_dir, rel_path=None):
    """
    :param source_filename:
    :param dest_dir:
    :param rel_path:
    :return:
    """
    LOG.debug("Extracting %s to %s", source_filename, dest_dir)

    with zipfile.ZipFile(source_filename) as zfd:
        for member in zfd.infolist():
            if rel_path:
                if not member.filename.startswith(rel_path):
                    continue
                else:
                    member.filename = member.filename[len(rel_path) + 1:]

            if not member.filename:
                continue

            # Path traversal defense copied from
            # http://hg.python.org/cpython/file/tip/Lib/http/server.py#l789
            LOG.debug("Writing %s%s%s", dest_dir, os.path.sep, member.filename)

            zfd.extract(member, dest_dir)


def untar(source_filename, dest_dir, rel_path=None):
    with tarfile.open(source_filename, "r|*") as tar:
        for member in tar:
            if member.isfile():
                if member.name is None:
                    continue
                if rel_path is not None and not member.name.startswith(rel_path):
                    continue

                filename = os.path.basename(member.name)
                destination = os.path.join(dest_dir, filename)
                with open(destination, "wb") as output:
                    shutil.copyfileobj(tar.extractfile(member), output, member.size)


def make_boundary(text=None):
    """
    Generate boundary id
    :param text:
    :return:
    """
    _width = len(repr(sys.maxsize - 1))
    _fmt = '%%0%dd' % _width
    token = random.randrange(sys.maxsize)
    boundary = ('=' * 15) + (_fmt % token) + '=='
    if text is None:
        return boundary
    bnd = boundary
    counter = 0
    while True:
        cre = re.compile(r'^--' + re.escape(bnd) + '(--)?$', re.MULTILINE)
        if not cre.search(text):
            break
        bnd = boundary + '.' + str(counter)
        counter += 1
    return bnd


def is_int(str_val):
    """
    Check if str_val is int type
    :param str_val: str
    :return: bool
    """
    if str_val.startswith('-') and str_val[1:].isdigit():
        return True
    elif str_val.isdigit():
        return True
    else:
        return False


def shutdown_process(process_obj, log_obj, send_sigterm=True):
    # unhandled sigterm causes immediate break of the process
    # so it makes sense to turn send_sigterm off for graceful (or delayed) shutdown
    time_limit = 60
    for count in range(time_limit + 1):
        time.sleep(1)

        process_still_works = process_obj and process_obj.poll() is None
        if not process_still_works:
            break

        if count < time_limit and not send_sigterm:
            continue  # time for sigterm, but it isn't supported (unhandled)
        elif count == time_limit and not is_windows():
            kill_signal = signal.SIGKILL  # send KILL to program on linux/mac
        else:
            kill_signal = signal.SIGTERM  # KILL doesn't supported on win, send TERM instead

        msg = "Terminating process PID %s with signal %s (%s tries left)"
        log_obj.info(msg, process_obj.pid, kill_signal, time_limit - count)
        try:
            if is_windows():
                cur_pids = psutil.pids()
                if process_obj.pid in cur_pids:
                    jm_proc = psutil.Process(process_obj.pid)
                    for child_proc in jm_proc.children(recursive=True):
                        log_obj.debug("Terminating child process: %d", child_proc.pid)
                        child_proc.send_signal(kill_signal)
                    os.kill(process_obj.pid, kill_signal)
            else:
                os.killpg(process_obj.pid, kill_signal)
        except OSError as exc:
            log_obj.debug("Failed to terminate process: %s", exc)


class LocalFileAdapter(requests.adapters.BaseAdapter):
    """
    Protocol Adapter to allow HTTPClient to GET file:// URLs
    """

    @staticmethod
    def _chkpath(method, path):
        """Return an HTTP status for the given filesystem path."""
        if method.lower() in ('put', 'delete'):
            return 501, "Not Implemented"
        elif method.lower() not in ('get', 'head'):
            return 405, "Method Not Allowed"
        elif os.path.isdir(path):
            return 400, "Path Not A File"
        elif not os.path.isfile(path):
            return 404, "File Not Found"
        elif not os.access(path, os.R_OK):
            return 403, "Access Denied"
        else:
            return 200, "OK"

    def send(self, req, **kwargs):  # pylint: disable=unused-argument
        """Return the file specified by the given request
        """
        path = os.path.normcase(os.path.normpath(url2pathname(req.path_url)))
        response = requests.Response()

        response.status_code, response.reason = self._chkpath(req.method, path)
        if response.status_code == 200 and req.method.lower() != 'head':
            try:
                response.raw = open(path, 'rb')
            except (OSError, IOError) as err:
                response.status_code = 500
                response.reason = str(err)

        if isinstance(req.url, bytes):
            response.url = req.url.decode('utf-8')
        else:
            response.url = req.url

        response.request = req
        response.connection = self

        return response

    def close(self):
        pass


class HTTPClient(object):
    def __init__(self):
        self.session = requests.Session()
        self.session.mount('file://', LocalFileAdapter())
        self.log = logging.getLogger(self.__class__.__name__)
        self.proxy_settings = None

    def add_proxy_settings(self, proxy_settings):
        if proxy_settings and proxy_settings.get("address"):
            self.proxy_settings = proxy_settings
            proxy_addr = proxy_settings.get("address")
            self.log.info("Using proxy %r", proxy_addr)
            proxy_url = parse.urlsplit(proxy_addr)
            self.log.debug("Using proxy settings: %s", proxy_url)
            username = proxy_settings.get("username")
            pwd = proxy_settings.get("password")
            scheme = proxy_url.scheme if proxy_url.scheme else 'http'
            if username and pwd:
                proxy_uri = "%s://%s:%s@%s" % (scheme, username, pwd, proxy_url.netloc)
            else:
                proxy_uri = "%s://%s" % (scheme, proxy_url.netloc)
            self.session.proxies = {"https": proxy_uri, "http": proxy_uri}

        self.session.verify = proxy_settings.get('ssl-cert', True)
        self.session.cert = proxy_settings.get('ssl-client-cert', None)

    def get_proxy_props(self):
        props = {}

        if not self.proxy_settings or not self.proxy_settings.get("address"):
            return props

        proxy_url = parse.urlsplit(self.proxy_settings.get("address"))
        username = self.proxy_settings.get("username")
        pwd = self.proxy_settings.get("password")
        for protocol in ["http", "https"]:
            props[protocol + '.proxyHost'] = proxy_url.hostname
            props[protocol + '.proxyPort'] = proxy_url.port or 80
            if username and pwd:
                props[protocol + '.proxyUser'] = username
                props[protocol + '.proxyPass'] = pwd

        return props

    @staticmethod
    def _save_file_from_connection(conn, filename, reporthook=None):
        if not conn.ok:
            raise TaurusNetworkError("Connection failed, status code %s" % conn.status_code)
        total = int(conn.headers.get('content-length', 0))
        block_size = 1024
        count = 0
        with open(filename, 'wb') as f:
            for chunk in conn.iter_content(chunk_size=block_size):
                if chunk:
                    f.write(chunk)
                    count += 1
                    if reporthook:
                        reporthook(count, block_size, total)

    def download_file(self, url, filename, reporthook=None, data=None, timeout=None):
        headers = None
        try:
            with self.session.get(url, stream=True, data=data, timeout=timeout) as conn:
                self._save_file_from_connection(conn, filename, reporthook=reporthook)
                headers = conn.headers
        except requests.exceptions.RequestException as exc:
            resp = exc.response
            self.log.debug("File download resulted in exception: %s", traceback.format_exc())
            msg = "Unsuccessful download from %s" % url
            if resp is not None:
                msg += ": %s - %s" % (resp.status_code, resp.reason)
            raise TaurusNetworkError(msg)
        except BaseException:
            self.log.debug("File download resulted in exception: %s", traceback.format_exc())
            raise TaurusNetworkError("Unsuccessful download from %s" % url)

        return filename, headers

    def request(self, method, url, *args, **kwargs):
        self.log.debug('Making HTTP request %s %s', method, url)
        try:
            return self.session.request(method, url, *args, **kwargs)
        except requests.exceptions.RequestException as exc:
            resp = exc.response
            self.log.debug("Request resulted in exception: %s", traceback.format_exc())
            msg = "Request to %s failed" % url
            if resp is not None:
                msg += ": %s - %s" % (resp.status_code, resp.reason)
            raise TaurusNetworkError(msg)


class ExceptionalDownloader(object):
    def __init__(self, http_client):
        """

        :type http_client: HTTPClient
        """
        super(ExceptionalDownloader, self).__init__()
        self.http_client = http_client

    def get(self, url, filename=None, reporthook=None, data=None, suffix="", timeout=30.0):
        if os.getenv("TAURUS_DISABLE_DOWNLOADS", ""):
            raise TaurusInternalException("Downloads are disabled by TAURUS_DISABLE_DOWNLOADS env var")

        try:
            if not filename:
                filename = temp_file(suffix)
            result = self.http_client.download_file(url, filename, reporthook=reporthook, data=data, timeout=timeout)
        except BaseException:
            os.remove(filename)
            raise

        return result


class RequiredTool(object):
    """
    Abstract required tool
    """
    VERSION = None
    DOWNLOAD_LINK = None

    def __init__(self, log=None, tool_path="", download_link="", http_client=None,
                 env=None, version=None, installable=True, mandatory=True, dry_install=False):
        self.http_client = http_client
        self.tool_path = os.path.expanduser(tool_path)
        self.download_link = download_link or self.DOWNLOAD_LINK
        self.mirror_manager = None
        self.mandatory = mandatory
        self.installable = installable
        self.version = str(version) if version else self.VERSION
        self.tool_name = self.__class__.__name__
        self.dry_install = dry_install

        log = log or LOG
        self.log = log.getChild(self.tool_name)

        self.env = env or Environment(self.log)

    def _get_version(self, output):
        return

    def call(self, *args, **kwargs):
        mixed_env = self.env.get()
        mixed_env.update(kwargs.get("env", {}))
        kwargs["env"] = mixed_env
        return exec_and_communicate(*args, **kwargs)

    def check_if_installed(self):
        if os.path.exists(self.tool_path):
            return True
        self.log.debug("File not exists: %s", self.tool_path)
        return False

    def install(self):
        if self.dry_install:
            self.log.info(f"Dry installation for {self.tool_name}")
            return

        if not self.installable:
            msg = "%s isn't found, automatic installation isn't implemented" % self.tool_name
            if self.mandatory:
                raise ToolError(msg)
            else:
                self.log.warning(msg)
                return

        with ProgressBarContext() as pbar:
            if not os.path.exists(os.path.dirname(self.tool_path)):
                os.makedirs(os.path.dirname(self.tool_path))
            downloader = ExceptionalDownloader(self.http_client)
            self.log.info("Downloading %s", self.download_link)
            downloader.get(self.download_link, self.tool_path, reporthook=pbar.download_callback)

            if self.check_if_installed():
                return self.tool_path
            else:
                raise ToolError("Unable to run %s after installation!" % self.tool_name)

    def _download(self, suffix=".zip", use_link=False):
        if use_link:
            links = [self.download_link]
        else:
            links = self.mirror_manager.mirrors()

        downloader = ExceptionalDownloader(self.http_client)
        for link in links:
            self.log.info("Downloading: %s", link)
            with ProgressBarContext() as pbar:
                try:
                    return downloader.get(link, reporthook=pbar.download_callback, suffix=suffix)[0]
                except KeyboardInterrupt:
                    raise
                except BaseException as exc:
                    self.log.error("Error while downloading %s: %s" % (link, exc))
        error_message = "%s download failed: No more links to try" % self.tool_name
        if self.mandatory:
            raise TaurusInternalException(error_message)
        else:
            self.log.warning(error_message)


class JavaVM(RequiredTool):
    def __init__(self, **kwargs):
        super(JavaVM, self).__init__(installable=False, tool_path="java", **kwargs)

    def _get_version(self, output):
        versions = re.findall("version\ \"([_\d\.]*)", output)
        version = parse_java_version(versions)

        if not version:
            self.log.warning("Tool version parsing error: %s", output)

        return version

    def check_if_installed(self):
        cmd = [self.tool_path, '-version']
        self.log.debug("Trying %s: %s", self.tool_name, cmd)
        try:
            out, err = self.call(cmd)
        except CALL_PROBLEMS as exc:
            self.log.debug("Failed to check %s: %s", self.tool_name, exc)
            return False

        self.version = self._get_version(err)

        if err:
            out += err
        self.log.debug("%s output: %s", self.tool_name, out)
        return True


class ProgressBarContext(ProgressBar):
    def __init__(self, maxval=0):
        widgets = [Percentage(), ' ', Bar(marker='=', left='[', right=']'), ' ', ETA()]
        super(ProgressBarContext, self).__init__(widgets=widgets, maxval=maxval, fd=sys.stdout)

    def __enter__(self):
        if not (sys.stdout and sys.stdout.isatty()):
            LOG.debug("No progressbar for non-tty output: %s", sys.stdout)

        self.start()
        return self

    def update(self, value=None):
        if sys.stdout and sys.stdout.isatty():
            super(ProgressBarContext, self).update(value)

    def __exit__(self, exc_type, exc_val, exc_tb):
        del exc_type, exc_val, exc_tb
        if sys.stdout and sys.stdout.isatty():
            self.finish()

    def download_callback(self, block_count, blocksize, totalsize):
        if totalsize > 0:
            self.maxval = totalsize
            progress = block_count * blocksize
            self.update(progress if progress <= totalsize else totalsize)


class IncrementableProgressBar(ProgressBarContext):
    def __init__(self, maxval):
        super(IncrementableProgressBar, self).__init__(maxval=maxval)

    def increment(self):
        incremented = self.currval + 1
        if incremented < self.maxval:
            super(IncrementableProgressBar, self).update(incremented)

    def catchup(self, started_time=None, current_value=None):
        super(IncrementableProgressBar, self).start()
        if started_time:
            self.start_time = started_time
        if current_value and current_value < self.maxval:
            self.update(current_value)


class TclLibrary(RequiredTool):
    ENV_NAME = "TCL_LIBRARY"
    INIT_TCL = "init.tcl"
    FOLDER = "tcl"

    def check_if_installed(self):
        """
        Check if tcl is available
        :return:
        """
        if is_windows():
            self.log.debug("Checking if %s variable is present in environment", TclLibrary.ENV_NAME)
            if not os.environ.get(TclLibrary.ENV_NAME, None):
                self.log.debug("%s environment variable is not present", TclLibrary.ENV_NAME)
                return False
            else:
                self.log.debug("%s environment variable is present", TclLibrary.ENV_NAME)
                return True
        else:
            self.log.debug("We don't need to check tcl library on this platform")
            return True

    @staticmethod
    def _find_tcl_dir():
        lib_dirs = [os.path.dirname(_x) for _x in sys.path if _x.lower().endswith('lib')]
        for lib_dir in lib_dirs:
            base_dir = os.path.join(lib_dir, TclLibrary.FOLDER)
            if os.path.exists(base_dir):
                for root, _, files in os.walk(base_dir):
                    if TclLibrary.INIT_TCL in files:
                        return root

    def _set_env_variable(self, value):
        self.log.debug("Setting environment %s=%s", TclLibrary.ENV_NAME, value)
        os.environ[TclLibrary.ENV_NAME] = value

    def install(self):
        """
        :return:
        """
        tcl_dir = self._find_tcl_dir()
        if tcl_dir:
            self.log.debug("Tcl directory was found: %s", tcl_dir)
            self._set_env_variable(tcl_dir)

        if not self.check_if_installed():
            self.log.warning("No Tcl library was found")


class Node(RequiredTool):
    def __init__(self, **kwargs):
        super(Node, self).__init__(installable=False, **kwargs)

    def check_if_installed(self):
        node_candidates = ["node", "nodejs"]
        for candidate in node_candidates:
            try:
                self.log.debug("Trying '%r' as Node Tool...", candidate)
                out, err = self.call([candidate, '--version'])
            except CALL_PROBLEMS as exc:
                self.log.debug("%r is not installed: %s", candidate, exc)
                continue

            if err:
                out += err
            self.log.debug("%s output: %s", candidate, out)
            self.tool_path = candidate
            return True

        return False


class MirrorsManager(object):
    def __init__(self, http_client, base_link, parent_logger):
        """

        :type base_link: str
        :type http_client: HTTPClient
        """
        self.base_link = base_link
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.http_client = http_client
        self.page_source = None

    @abstractmethod
    def _parse_mirrors(self):
        return []

    def mirrors(self):
        self.log.debug("Retrieving mirrors from page: %s", self.base_link)
        downloader = ExceptionalDownloader(self.http_client)
        try:
            tmp_file = downloader.get(self.base_link)[0]
            with open(tmp_file) as fds:
                self.page_source = fds.read()
        except BaseException:
            self.log.debug("Exception: %s", traceback.format_exc())
            self.log.error("Can't fetch %s", self.base_link)
        return self._parse_mirrors()


@contextmanager
def log_std_streams(logger=None, stdout_level=logging.DEBUG, stderr_level=logging.DEBUG):
    """
    redirect standard output/error to taurus logger
    """
    out_descriptor = os.dup(1)
    err_descriptor = os.dup(2)
    stdout = tempfile.SpooledTemporaryFile(mode='w+')
    stderr = tempfile.SpooledTemporaryFile(mode='w+')
    sys.stdout = stdout
    sys.stderr = stderr
    os.dup2(stdout.fileno(), 1)
    os.dup2(stderr.fileno(), 2)
    try:
        yield
    finally:
        stdout.seek(0)
        stderr.seek(0)
        stdout_str = stdout.read().strip()
        stderr_str = stderr.read().strip()
        stdout.close()
        stderr.close()
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__
        os.dup2(out_descriptor, 1)
        os.dup2(err_descriptor, 2)
        os.close(out_descriptor)
        os.close(err_descriptor)
        if logger:
            if stdout_str:
                logger.log(stdout_level, "STDOUT: " + stdout_str)
            if stderr_str:
                logger.log(stderr_level, "STDERR: " + stderr_str)


def open_browser(url):
    try:
        browser = webbrowser.get()
        if type(browser) != GenericBrowser:  # pylint: disable=unidiomatic-typecheck
            with log_std_streams(logger=LOG):
                webbrowser.open(url)
    except BaseException as exc:
        LOG.warning("Can't open link in browser: %s", exc)


def is_windows():
    return platform.system() == 'Windows'


def is_linux():
    return 'linux' in sys.platform.lower()


def is_mac():
    return 'darwin' in sys.platform.lower()


def platform_bitness():
    return 64 if sys.maxsize > 2 ** 32 else 32


EXE_SUFFIX = ".bat" if is_windows() else ".sh"


class DummyScreen(BaseScreen):
    """
    Null-object for Screen on non-tty output
    """

    def __init__(self, rows=120, cols=40):
        super(DummyScreen, self).__init__()
        self.size = (rows, cols)
        self.ansi_escape = re.compile(r'\x1b[^m]*m')

    def get_cols_rows(self):
        """
        Dummy cols and rows

        :return:
        """
        return self.size

    def draw_screen(self, size, canvas):
        """

        :param size:
        :type canvas: urwid.Canvas
        """
        data = ""
        for char in canvas.content():
            line = ""
            for part in char:
                if isinstance(part[2], str):
                    line += part[2]
                else:
                    line += part[2].decode()
            data += "%s│\n" % line
        data = self.ansi_escape.sub('', data)
        LOG.info("Screen %sx%s chars:\n%s", size[0], size[1], data)


class PythonGenerator(object):
    IMPORTS = ''
    INDENT_STEP = 4

    def __init__(self, scenario):
        self.root = etree.Element("PythonCode")
        self.tree = etree.ElementTree(self.root)
        self.log = scenario.engine.log.getChild(self.__class__.__name__)
        self.scenario = scenario

    def add_imports(self):
        imports = etree.Element("imports")
        imports.text = self.IMPORTS
        return imports

    @abstractmethod
    def build_source_code(self):
        pass

    @staticmethod
    def gen_class_definition(class_name, inherits_from, indent=0):
        def_tmpl = "class {class_name}({inherits_from}):"
        class_def_element = etree.Element("class_definition", indent=str(indent))
        class_def_element.text = def_tmpl.format(class_name=class_name, inherits_from="".join(inherits_from))
        return class_def_element

    @staticmethod
    def gen_method_definition(method_name, params, indent=None):
        if indent is None:
            indent = PythonGenerator.INDENT_STEP

        def_tmpl = "def {method_name}({params}):"
        method_def_element = etree.Element("method_definition", indent=str(indent))
        method_def_element.text = def_tmpl.format(method_name=method_name, params=",".join(params))
        return method_def_element

    @staticmethod
    def gen_decorator_statement(decorator_name, indent=None):
        if indent is None:
            indent = PythonGenerator.INDENT_STEP

        def_tmpl = "@{decorator_name}"
        decorator_element = etree.Element("decorator_statement", indent=str(indent))
        decorator_element.text = def_tmpl.format(decorator_name=decorator_name)
        return decorator_element

    @staticmethod
    def gen_statement(statement, indent=None):
        if indent is None:
            indent = PythonGenerator.INDENT_STEP * 2

        statement_elem = etree.Element("statement", indent=str(indent))
        statement_elem.text = statement
        return statement_elem

    def gen_comment(self, comment, indent=None):
        return self.gen_statement("# %s" % comment, indent=indent)

    def save(self, filename):
        with codecs.open(filename, 'w', encoding='utf-8') as fds:
            for child in self.root.iter():
                if child.text is not None:
                    indent = int(child.get('indent', "0"))
                    fds.write(" " * indent + child.text + "\n")

    def gen_new_line(self, indent=0):
        return self.gen_statement("", indent=indent)


def str_representer(dumper, data):
    """ Representer for PyYAML that dumps multiline strings as | scalars """
    if len(data.splitlines()) > 1:
        return dumper.represent_scalar('tag:yaml.org,2002:str', data, style='|')
    return dumper.represent_scalar('tag:yaml.org,2002:str', data)


def humanize_bytes(byteval):
    # from http://stackoverflow.com/questions/1094841/reusable-library-to-get-human-readable-version-of-file-size/
    #   25613067#25613067
    _suffixes = [' ', 'K', 'M', 'G', 'T', 'P']

    # determine binary order in steps of size 10
    # (coerce to int, // still returns a float)
    order = int(math.log(byteval, 2) / 10.0) if byteval else 0
    # format file size
    # (.4g results in rounded numbers for exact matches and max 3 decimals,
    # should never resort to exponent values)
    return '{:.4g}{}'.format(byteval / (1 << (order * 10)), _suffixes[order])


class LDJSONReader(object):
    def __init__(self, filename, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.file = FileReader(filename=filename,
                               file_opener=lambda f: open(f, 'rb'),
                               parent_logger=self.log)
        self.partial_buffer = ""

    def read(self, last_pass=False):
        lines = self.file.get_lines(size=1024 * 1024, last_pass=last_pass)

        for line in lines:
            if not last_pass and not line.endswith("\n"):
                self.partial_buffer += line
                continue
            line = "%s%s" % (self.partial_buffer, line)
            self.partial_buffer = ""
            yield json.loads(line)


def get_host_ips(filter_loopbacks=True):
    """
    Returns a list of all IP addresses assigned to this host.

    :param filter_loopbacks: filter out loopback addresses
    """
    ips = []
    for _, interfaces in iteritems(psutil.net_if_addrs()):
        for iface in interfaces:
            addr = str(iface.address)
            try:
                ip = ipaddress.ip_address(addr)
                if filter_loopbacks and ip.is_loopback:
                    continue
            except ValueError:
                continue
            ips.append(iface.address)
    return ips


def is_url(url):
    return parse.urlparse(url).scheme in ["https", "http"]


def guess_delimiter(path):
    with open(path) as fhd:
        header = fhd.read(4096)  # 4KB is enough for header
        try:
            delimiter = guess_csv_dialect(header).delimiter
        except BaseException as exc:
            LOG.debug(traceback.format_exc())
            LOG.warning('CSV dialect detection failed (%s), default delimiter selected (",")', exc)
            delimiter = ","  # default value

    return delimiter


def get_assembled_value(configs, key, protect=False):
    """
    Joins values from several configs, "the last is the most important" (strings, lists or dictionaries).

    :param configs: list of dicts with target configs
    :param key: name of target config
    :param protect: use safely, make deepcopy
    """
    target_configs = []
    for config in configs:
        target_config = config.get(key)
        if target_config:
            if protect:
                target_config = copy.deepcopy(target_config)
            target_configs.append(target_config)

    if not target_configs:
        return

    res = target_configs.pop(0)

    if all(isinstance(config, dict) for config in target_configs):
        for config in target_configs:
            res.merge(config)
    elif all(isinstance(config, list) for config in target_configs):
        for config in target_configs:
            res.extend(config)
    elif all(isinstance(config, (numeric_types, str)) for config in target_configs):
        res = target_configs[-1]
    else:
        raise TaurusConfigError("Incorrect type of '%s' found." % key)

    return res


def parse_think_time(think_time, full=False):
    distributions = ["uniform", "gaussian", "poisson"]
    format_str = "^(%s)\(([\wd.]+)[,\s]+([\wd.]+)\)$"
    expr = re.compile(format_str % '|'.join(distributions), re.IGNORECASE)
    res = expr.match(str(think_time))

    if not res:  # constant timer
        return think_time

    if not full:
        return res.group(2).lower()  # make it simple!
    else:
        return [res.group(i + 1).lower() for i in range(3)]


def convert_body_to_string(request):
    if isinstance(request.body, (dict, list, numeric_types)):
        if request.get_header('content-type') == 'application/json' or isinstance(request.body, numeric_types):
            request.body = json.dumps(request.body)
        elif not simple_body_dict(request.body):
            LOG.debug('Header "Content-Type: application/json" is required for body: "%s"', request.body)
            request.body = json.dumps(request.body)


class SoapUIScriptConverter(object):
    NAMESPACES = dict(con="http://eviware.com/soapui/config")

    def __init__(self, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.tree = None
        self.interface = None

    def load(self, path):
        try:
            self.tree = etree.ElementTree()
            self.tree.parse(path)
        except BaseException as exc:
            msg = "XML parsing failed for file %s: %s"
            raise TaurusInternalException(msg % (path, exc))

    def _extract_headers(self, config_elem):
        headers_settings = config_elem.find(
            './/con:settings/con:setting[@id="com.eviware.soapui.impl.wsdl.WsdlRequest@request-headers"]',
            namespaces=self.NAMESPACES)
        if headers_settings is None:
            return None
        headers = etree.fromstring(headers_settings.text)
        if "{" + self.NAMESPACES['con'] + "}" + "entry" == headers.tag:
            entries = [headers]
        else:
            entries = headers.findall(".//con:entry", namespaces=self.NAMESPACES)

        headers = {entry.get('key'): entry.get('value')
                   for entry in entries}
        return headers

    def _extract_assertions(self, config_elem):
        assertions = []
        assertion_tags = config_elem.findall('.//con:assertion', namespaces=self.NAMESPACES)
        for assertion in assertion_tags:
            # TODO: XPath assertions / JSONPath assertions ?
            if assertion.get('type') in ('Simple Contains', 'Simple NotContains'):
                subject = assertion.findtext('./con:configuration/token', namespaces=self.NAMESPACES)
                use_regex = assertion.findtext('./con:configuration/useRegEx', namespaces=self.NAMESPACES)
                negate = assertion.get('type') == 'Simple NotContains'

                assertions.append({"contains": [subject],
                                   "subject": "body",
                                   "regexp": use_regex == "true",
                                   "not": negate,
                                   })
        return assertions

    def _extract_http_request(self, test_step):
        config = test_step.find('./con:config', namespaces=self.NAMESPACES)

        request = {
            "label": test_step.get('name'),
            "url": config.find('.//con:endpoint', namespaces=self.NAMESPACES).text}

        method = config.get('method')

        if method is not None and method != "GET":
            request["method"] = method

        headers = self._extract_headers(config)
        assertions = self._extract_assertions(config)

        if headers:
            request["headers"] = headers

        if assertions:
            request["assert"] = assertions

        body = config.findtext('./con:request', namespaces=self.NAMESPACES)

        if body is not None:
            request["body"] = body

        params = config.findall('./con:parameters/con:parameter', namespaces=self.NAMESPACES)

        if params:
            body = {}
            for param in params:
                key = param.findtext("./con:name", namespaces=self.NAMESPACES)
                value = param.findtext("./con:value", namespaces=self.NAMESPACES)
                body[key] = value
            request["body"] = body

        return request

    def _extract_soap_endpoint(self, interface_name, operation_name):
        interface = self.tree.find("//con:interface[@name='%s']" % interface_name, namespaces=self.NAMESPACES)
        if interface is None:
            self.log.warning("Can't find intreface %s for operation %s, skipping", interface_name, operation_name)
            return None

        interface_endpoint = interface.findtext("./con:endpoints/con:endpoint", namespaces=self.NAMESPACES)

        operation = interface.find(".//con:operation[@name='%s']" % operation_name, namespaces=self.NAMESPACES)
        if operation is None:
            self.log.warning("Can't find operation %s for interface %s, skipping", operation_name, interface_name)
            return None

        operation_endpoint = operation.findtext(".//con:endpoint", namespaces=self.NAMESPACES)

        if operation_endpoint is not None:
            return operation_endpoint
        elif interface_endpoint is not None:
            return interface_endpoint
        else:
            self.log.warning("Can't find endpoint for %s:%s", interface_name, operation_name)
            return None

    def _extract_soap_request(self, test_step):
        label = test_step.get('name')
        config = test_step.find('./con:config', namespaces=self.NAMESPACES)
        body = config.findtext('./con:request/con:request', namespaces=self.NAMESPACES)

        interface = config.findtext('./con:interface', namespaces=self.NAMESPACES)
        operation = config.findtext('./con:operation', namespaces=self.NAMESPACES)
        self.log.debug("Extracting SOAP request, interface=%r, operation=%r", interface, operation)
        endpoint = self._extract_soap_endpoint(interface, operation)

        if endpoint is None:
            return

        request = {
            "url": endpoint,
            "label": label,
            "method": "POST",
            "headers": {
                "Content-Type": "text/xml; charset=utf-8",
            }
        }

        if body:
            request["body"] = body

        return request

    def _calc_base_address(self, test_step):
        config = test_step.find('./con:config', namespaces=self.NAMESPACES)
        service = config.get('service')
        interfaces = self.tree.xpath('//con:interface', namespaces=self.NAMESPACES)
        for interface in interfaces:
            if interface.get("name") == service:
                endpoint = interface.find('.//con:endpoints/con:endpoint', namespaces=self.NAMESPACES)
                if endpoint is not None:
                    service = endpoint.text
                    break
        return service

    def _extract_rest_request(self, test_step):
        config = test_step.find('./con:config', namespaces=self.NAMESPACES)
        method = config.get('method')

        params = self._parse_parent_resources(config)

        url = self._calc_base_address(test_step) + config.get('resourcePath')

        for param_name in copy.copy(list(params.keys())):
            template = "{" + param_name + "}"
            if template in url:
                param_value = params.pop(param_name)
                url = url.replace(template, param_value)

        request = {"url": url, "label": test_step.get('name')}

        if method is not None and method != "GET":
            request["method"] = method

        headers = self._extract_headers(config)
        assertions = self._extract_assertions(config)

        if headers:
            request["headers"] = headers

        if assertions:
            request["assert"] = assertions

        body = {}
        for key, value in iteritems(params):
            body[key] = value

        if body:
            request["body"] = body

        return request

    def _parse_parent_resources(self, config):
        method_name = config.get('methodName')

        for interface in self.interface:
            method_obj = interface.find('.//con:method[@name="%s"]' % method_name, namespaces=self.NAMESPACES)
            if method_obj is not None:
                break

        params = BetterDict()
        if method_obj is not None:
            parent = method_obj.getparent()
            while parent.tag.endswith('resource'):
                for param in parent.findall('./con:parameters/con:parameter', namespaces=self.NAMESPACES):
                    param_name = param.findtext('./con:name', namespaces=self.NAMESPACES)
                    param_value = param.findtext('./con:value', namespaces=self.NAMESPACES)
                    def_value = param.findtext('./con:default', namespaces=self.NAMESPACES)
                    if param_value:
                        params[param_name] = param_value
                    elif def_value:
                        params[param_name] = def_value

                parent = parent.getparent()

        for entry in config.findall('./con:restRequest/con:parameters/con:entry', namespaces=self.NAMESPACES):
            params.merge({entry.get("key"): entry.get("value")})

        return params

    def _extract_properties(self, block, key_prefix=""):
        properties = block.findall('./con:properties/con:property', namespaces=self.NAMESPACES)
        prop_map = {}
        for prop in properties:
            key = key_prefix + prop.findtext('./con:name', namespaces=self.NAMESPACES)
            value = prop.findtext('./con:value', namespaces=self.NAMESPACES)
            prop_map[key] = value
        return prop_map

    def _extract_execution(self, test_case):
        load_exec = {}
        load_test = test_case.find('./con:loadTest', namespaces=self.NAMESPACES)
        if load_test is not None:
            load_exec['concurrency'] = int(load_test.find('./con:threadCount', self.NAMESPACES).text)
            load_exec['hold-for'] = int(load_test.find('./con:testLimit', self.NAMESPACES).text)
        else:
            load_exec['concurrency'] = 1
        return load_exec

    def _validate_transfer(self, source_type, source_step_name, transfer_type, target_step_name):
        source_step = self.tree.find("//con:testStep[@name='%s']" % source_step_name, namespaces=self.NAMESPACES)
        if source_step is None:
            self.log.warning("Can't find source step (%s) for Property Transfer. Skipping", source_step_name)
            return False

        source_step_type = source_step.get("type")
        if source_step_type not in ["httprequest", "restrequest", "request"]:
            self.log.warning("Unsupported source step type for Property Transfer (%s). Skipping", source_step_type)
            return False

        if source_type != "Response":
            self.log.warning("Found Property Transfer with non-response source (%s). Skipping", source_type)
            return False

        if transfer_type not in ["JSONPATH", "XPATH"]:
            self.log.warning("Found Property Transfer with unsupported type (%s). Skipping", transfer_type)
            return False

        target_step = self.tree.find("//con:testStep[@name='%s']" % target_step_name, namespaces=self.NAMESPACES)
        if target_step is None:
            self.log.warning("Can't find target step (%s) for Property Transfer. Skipping", target_step_name)
            return False

        target_step_type = target_step.get("type")
        if target_step_type != "properties":
            self.log.warning("Unsupported target step type for Property Transfer (%s). Skipping", target_step_type)
            return False

        return True

    def _extract_transfer(self, transfer):
        source_type = transfer.findtext('./con:sourceType', namespaces=self.NAMESPACES)
        source_step_name = transfer.findtext('./con:sourceStep', namespaces=self.NAMESPACES)
        query = transfer.findtext('./con:sourcePath', namespaces=self.NAMESPACES)
        transfer_type = transfer.findtext('./con:type', namespaces=self.NAMESPACES)
        target_step_name = transfer.findtext('./con:targetStep', namespaces=self.NAMESPACES)
        target_prop = transfer.findtext('./con:targetType', namespaces=self.NAMESPACES)

        if source_step_name.startswith("#") and source_step_name.endswith("#"):
            source_step_name = source_step_name[1:-1]

        if not self._validate_transfer(source_type, source_step_name, transfer_type, target_step_name):
            return None

        extractor = BetterDict()
        if transfer_type == "JSONPATH":
            extractor.merge({
                'extract-jsonpath': {
                    target_prop: {
                        'jsonpath': query,
                        'default': 'NOT_FOUND',
                    }
                }
            })
        elif transfer_type == "XPATH":
            extractor.merge({
                'extract-xpath': {
                    target_prop: {
                        'xpath': query,
                        'default': 'NOT_FOUND',
                    }
                }
            })
        return {source_step_name: extractor}

    def _extract_property_transfers(self, test_step):
        extractors = BetterDict()  # label -> {extract-xpath: ..., extract-jsonpath: ...}
        transfers = test_step.findall('./con:config/con:transfers', namespaces=self.NAMESPACES)
        if not transfers:
            return None

        for transfer in transfers:
            extracted_transfer = self._extract_transfer(transfer)
            if extracted_transfer is not None:
                extractors.merge(extracted_transfer)

        return extractors

    def _extract_scenario(self, test_case, case_level_props):
        variables = BetterDict.from_dict(case_level_props)
        requests = []

        extractors = BetterDict()

        steps = test_case.findall('.//con:testStep', namespaces=self.NAMESPACES)
        for step in steps:
            request = None
            if step.get("type") == "httprequest":
                request = self._extract_http_request(step)
            elif step.get("type") == "restrequest":
                request = self._extract_rest_request(step)
            elif step.get("type") == "request":
                request = self._extract_soap_request(step)
            elif step.get("type") == "properties":
                config_block = step.find('./con:config', namespaces=self.NAMESPACES)
                if config_block is not None:
                    props = self._extract_properties(config_block)
                    variables.merge(props)
            elif step.get("type") == "transfer":
                extracted_extractors = self._extract_property_transfers(step)  # label -> extractor
                if extracted_extractors:
                    extractors.merge(extracted_extractors)
            elif step.get("type") == "groovy":
                request = self._extract_script(step)

            if request is not None:
                requests.append(request)

        for request in requests:
            label = request["label"]
            if label in extractors:
                request.update(extractors[label])

        scenario = {
            "test-case": test_case.get("name"),
            "requests": requests
        }
        if variables:
            scenario["variables"] = variables

        return scenario

    def _extract_script(self, test_step):
        label = test_step.get("name", "Script")
        script = test_step.find('./con:config/script', namespaces=self.NAMESPACES).text
        if script is not None:
            script = script.strip()
            return {
                "label": label,
                "action": "pause",
                "target": "current-thread",
                "pause-duration": "0ms",
                "jsr223": [{
                    "language": "groovy",
                    "script-text": script,
                }]
            }

    def _extract_test_case(self, test_case, test_suite, suite_level_props):
        case_name = test_case.get("name")
        scenario_name = test_suite.get("name") + "-" + case_name

        case_properties = self._extract_properties(test_case)
        case_properties = {
            "#TestCase#" + key: value
            for key, value in iteritems(case_properties)
        }
        case_level_props = BetterDict.from_dict(suite_level_props)
        case_level_props.merge(case_properties)

        scenario = self._extract_scenario(test_case, case_level_props)
        scenario['test-suite'] = test_suite.get("name")

        return scenario_name, scenario

    def _extract_config(self, project, test_suites, target_test_case=None):
        execution = []
        scenarios = {}

        project_properties = self._extract_properties(project, key_prefix="#Project#")
        project_name = project.get("name")

        interface_exec, interface_scen = self._extract_interface(project_name, self.interface)
        execution.append(interface_exec)
        scenarios.update(interface_scen)

        for suite in test_suites:
            suite_props = BetterDict.from_dict(project_properties)
            suite_props.merge(self._extract_properties(suite, key_prefix="#TestSuite#"))

            test_cases = suite.findall('.//con:testCase', namespaces=self.NAMESPACES)
            for case in test_cases:
                case_name = case.get("name")
                scenario_name, scenario = self._extract_test_case(case, suite, suite_props)

                load_exec = self._extract_execution(case)
                load_exec['scenario'] = scenario_name
                self.log.debug("Extracted execution for scenario %s", scenario_name)

                if not scenario["requests"]:
                    self.log.warning("No requests extracted for scenario %s, skipping it" % scenario_name)
                    continue

                if target_test_case is None or target_test_case == case_name:
                    self.log.debug("Extracted scenario: %s", scenario_name)
                    scenarios[scenario_name] = scenario
                    execution.append(load_exec)

        return {
            "execution": execution,
            "scenarios": scenarios,
        }

    def convert_script(self, script_path, target_test_case=None):
        if not os.path.exists(script_path):
            raise ValueError("SoapUI script %s doesn't exist" % script_path)

        self.load(script_path)

        self.log.debug("Found namespaces: %s", self.NAMESPACES)

        projects = self.tree.xpath('//con:soapui-project', namespaces=self.NAMESPACES)
        self.log.debug("Found projects: %s", projects)
        project = projects[0]

        self.interface = project.findall('.//con:interface', namespaces=self.NAMESPACES)
        self.log.debug("Found interface: %s", self.interface)

        test_suites = project.findall('.//con:testSuite', namespaces=self.NAMESPACES)
        self.log.debug("Found test suites: %s", test_suites)

        config = self._extract_config(project, test_suites, target_test_case=target_test_case)

        if not config["scenarios"]:
            self.log.warning("No scenarios were extracted")

        if not config["execution"]:
            self.log.warning("No load tests were extracted")

        return config

    def _extract_interface(self, project_name, interfaces):
        execution = {
            "concurrency": 1,
            "iterations": 1,
            "hold-for": "10s",
            "scenario": project_name
        }
        scenarios = {}

        interface_requests = []

        for interface in interfaces:
            try:
                endpoint = interface.find('.//con:endpoint', namespaces=self.NAMESPACES).text
                resources = interface.findall('.//con:resource', namespaces=self.NAMESPACES)
                if not resources:
                    interface_requests.append({
                        "url": endpoint
                    })
                    continue
            except AttributeError:
                continue

            for resource in resources:
                path = resource.get("path")
                url = endpoint + path

                methods = resource.findall('.//con:method', namespaces=self.NAMESPACES)
                for method in methods:
                    method_type = method.get("method")

                    requests = method.findall('con:request', namespaces=self.NAMESPACES)
                    for request in requests:
                        request_body = request.find('.//con:request', namespaces=self.NAMESPACES).text
                        interface_requests.append({
                            "body": request_body,
                            "method": method_type,
                            "url": url
                        })

        scenarios.update({project_name: {"requests": interface_requests}})

        return execution, scenarios

    def find_soapui_test_case(self, test_case, scenarios):
        matching_scenarios = [
            (name, scen)
            for name, scen in iteritems(scenarios)
            if scen.get("test-case") == test_case
        ]
        if len(matching_scenarios) == 0:
            sorted_scenarios = sorted((name, scen) for name, scen in iteritems(scenarios))
            scenario_name, scenario = next(iter(sorted_scenarios))
            if test_case is None:
                self.log.warning("No `test-case` specified for SoapUI project, will use '%s'",
                                 scenario.get("test-case"))
            else:
                msg = "No matching test cases found for name '%s', using the '%s'"
                self.log.warning(msg, test_case, scenario.get("test-case"))
        elif len(matching_scenarios) > 1:
            scenario_name, scenario = next(iter(matching_scenarios))
            msg = "Multiple test cases found for name '%s', using case '%s' from suite '%s'"
            self.log.warning(msg, test_case, scenario.get('test-case'), scenario.get('test-suite'))
        else:
            scenario_name, scenario = next(iter(matching_scenarios))
        return scenario_name, scenario
