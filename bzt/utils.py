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
import itertools
import json
import locale
import logging
import mimetypes
import os
import platform
import random
import re
import shlex
import shutil
import signal
import socket
import stat
import sys
import tarfile
import tempfile
import time
import webbrowser
import zipfile
import ipaddress
import psutil

from abc import abstractmethod
from collections import defaultdict, Counter
from contextlib import contextmanager
from math import log
from progressbar import ProgressBar, Percentage, Bar, ETA
from subprocess import CalledProcessError, PIPE, check_output, STDOUT
from urwid import BaseScreen
from webbrowser import GenericBrowser

from bzt import TaurusInternalException, TaurusNetworkError, ToolError
from bzt.six import stream_decode, file_type, etree, parse
from bzt.six import string_types, iteritems, binary_type, text_type, b, integer_types, request

CALL_PROBLEMS = (CalledProcessError, OSError)


def sync_run(args, env=None):
    output = check_output(args, env=env, stderr=STDOUT)
    return stream_decode(output).rstrip()


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


def get_files_recursive(dir_name, exclude_mask=''):
    for root, _, files in os.walk(dir_name):
        for _file in files:
            if not fnmatch.fnmatch(_file, exclude_mask):
                yield os.path.join(root, _file)


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


class BetterDict(defaultdict):
    """
    Wrapper for defaultdict that able to deep merge other dicts into itself
    """

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

        if isinstance(value, string_types):
            if isinstance(value, str):  # this is a trick for python v2/v3 compatibility
                return value
            else:
                return text_type(value)
        else:
            return value

    def merge(self, src):
        """
        Deep merge other dict into current
        '-'  - overwrite operation prefix for dict key

        :type src: dict
        """
        if not isinstance(src, dict):
            raise TaurusInternalException("Loaded object is not dict [%s]: %s" % (src.__class__, src))

        for key, val in iteritems(src):
            merge_list_items = False
            if key.startswith("^"):  # eliminate flag
                # TODO: improve logic - use val contents to see what to eliminate
                if key[1:] in self:
                    self.pop(key[1:])
                continue
            elif key.startswith("~"):  # overwrite flag
                if key[1:] in self:
                    self.pop(key[1:])
                key = key[1:]
            elif key.startswith("$"):
                merge_list_items = True
                key = key[1:]

            if isinstance(val, dict):
                dst = self.get(key, force_set=True)
                if isinstance(dst, BetterDict):
                    dst.merge(val)
                elif isinstance(dst, Counter):
                    self[key] += val
                elif isinstance(dst, dict):
                    raise TaurusInternalException("Mix of DictOfDict and dict is forbidden")
                else:
                    self[key] = val
            elif isinstance(val, list):
                self.__ensure_list_type(val)
                if key not in self:
                    self[key] = []
                if isinstance(self[key], list):
                    if merge_list_items:
                        self.__merge_list_elements(self[key], val, key)
                    else:
                        self[key].extend(val)
                else:
                    self[key] = val
            else:
                self[key] = val

    def __merge_list_elements(self, left, right, key):
        for index, righty in enumerate(right):
            if index < len(left):
                lefty = left[index]
                if isinstance(lefty, BetterDict):
                    if isinstance(righty, BetterDict):
                        lefty.merge(righty)
                        continue
                logging.warning("Overwriting the value of %r when merging configs", key)
                left[index] = righty
            else:
                left.insert(index, righty)

    def __ensure_list_type(self, values):
        """
        Ensure that values is a list, convert if needed
        :param values: dict or list
        :return:
        """
        for idx, obj in enumerate(values):
            if isinstance(obj, dict):
                values[idx] = BetterDict()
                values[idx].merge(obj)
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

    def filter(self, rules):
        keys = set(self.keys())
        for key in keys:
            ikey = "!" + key
            if ikey in rules:
                if isinstance(rules.get(ikey), dict) and isinstance(self.get(key), BetterDict):
                    inverted_rules = {x: True for x in self.get(key).keys() if x not in rules[ikey]}
                    self.get(key).filter(inverted_rules)
                    if not self.get(key):  # clear empty
                        del self[key]
            elif key not in rules:
                del self[key]
            else:
                if isinstance(rules.get(key), dict) and isinstance(self.get(key), BetterDict):
                    self.get(key).filter(rules[key])
                    if not self.get(key):  # clear empty
                        del self[key]


def get_uniq_name(directory, prefix, suffix="", forbidden_names=()):
    base = os.path.join(directory, prefix)
    diff = ""
    num = 0
    while os.path.exists(base + diff + suffix) or base + diff + suffix in forbidden_names:
        num += 1
        diff = "-%s" % num

    return base + diff + suffix


def shell_exec(args, cwd=None, stdout=PIPE, stderr=PIPE, stdin=PIPE, shell=False, env=None):
    """
    Wrapper for subprocess starting

    :param stderr:
    :param stdout:
    :param cwd:
    :param stdin:
    :type args: basestring or list
    :return:
    """
    if stdout and not isinstance(stdout, int) and not isinstance(stdout, file_type):
        logging.warning("stdout is not IOBase: %s", stdout)
        stdout = None

    if stderr and not isinstance(stderr, int) and not isinstance(stderr, file_type):
        logging.warning("stderr is not IOBase: %s", stderr)
        stderr = None

    if isinstance(args, string_types) and not shell:
        args = shlex.split(args, posix=not is_windows())
    logging.getLogger(__name__).debug("Executing shell: %s at %s", args, cwd or os.curdir)

    if is_windows():
        return psutil.Popen(args, stdout=stdout, stderr=stderr, stdin=stdin,
                            bufsize=0, cwd=cwd, shell=shell, env=env)
    else:
        return psutil.Popen(args, stdout=stdout, stderr=stderr, stdin=stdin,
                            bufsize=0, preexec_fn=os.setpgrp, close_fds=True, cwd=cwd, shell=shell, env=env)


class Environment(object):
    def __init__(self, parent_log, data=None):
        self.data = {}
        self.log = parent_log.getChild(self.__class__.__name__)
        if data is not None:
            self.set(data)

    def set(self, env):
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

    def add_path(self, pair, finish=False):
        self._add(pair, os.pathsep, finish)

    def add_java_param(self, pair, finish=False):
        self._add(pair, " ", finish)

    def update(self, env):  # compatibility with taurus-server
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
        if key:
            key = str(key)
            if is_windows():
                key = key.upper()

            return self.data.get(key, None)
        else:
            # full environment
            return copy.deepcopy(self.data)


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
            self.log.debug("Reading: %s", self.name)
            self.fds.seek(self.offset)
            for line in self._readlines(hint=size):
                self.offset += len(line)
                yield self._decode(line, last_pass)

    def get_line(self):
        line = ""
        if self.is_ready():
            self.log.debug("Reading: %s", self.name)
            self.fds.seek(self.offset)
            line = self.fds.readline()
            self.offset += len(line)

        return self._decode(line)

    def get_bytes(self, size=-1, last_pass=False, decode=True):
        if self.is_ready():
            if last_pass:
                size = -1
            self.log.debug("Reading: %s", self.name)
            self.fds.seek(self.offset)
            _bytes = self.fds.read(size)
            self.offset += len(_bytes)
            if decode:
                return self._decode(_bytes, last_pass)
            else:
                return _bytes

    def __del__(self):
        if self.fds:
            self.fds.close()


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
    :return: :raise TaurusInternalException:
    """
    for key, val in iteritems(dictnr):
        if val == value:
            return key
    raise TaurusInternalException("Value not found in dict: %s" % value)


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
             'Content-Type: %s' % content_type, '', body]
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
        for item in self.__convert_to_list():
            # if (8-bit str (2.7) or bytes (3.x), then no processing, just add, else - encode)
            if isinstance(item, binary_type):
                result_list.append(item)
            elif isinstance(item, text_type):
                result_list.append(item.encode())
            else:
                raise TaurusInternalException("Unhandled form data type: %s" % type(item))

        res_bytes = b("\r\n").join(result_list)
        res_bytes += b("\r\n")
        return res_bytes
        # return b'\r\n'.join(x.encode() if isinstance(x, str) else x for x in self.__convert_to_list())


def to_json(obj, indent=True):
    """
    Convert object into indented json

    :param indent: whether to generate indented JSON
    :param obj: object to convert
    :return:
    """
    # NOTE: you can set allow_nan=False to fail when serializing NaN/Infinity
    return json.dumps(obj, indent=indent, cls=ComplexEncoder)


class JSONDumpable(object):
    """
    Marker class for json dumpable classes
    """
    pass


class JSONConvertable(object):
    @abstractmethod
    def __json__(self):
        "Convert class instance into JSON-dumpable structure (e.g. dict)"
        pass


class ComplexEncoder(json.JSONEncoder):
    """
    Magic class to help serialize in JSON any object.
    """
    TYPES = (dict, list, tuple, text_type, string_types, integer_types, float, bool, type(None))

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
        elif self.__convertable(obj):
            return obj.__json__()
        else:
            return None

    def __dumpable(self, obj):
        """
        Re

        :param obj:
        :rtype: bool
        """
        dumpable_types = tuple(self.TYPES + (JSONDumpable,))
        return isinstance(obj, dumpable_types)

    def __convertable(self, obj):
        return isinstance(obj, JSONConvertable)

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
            logging.debug("Writing %s%s%s", dest_dir, os.path.sep, member.filename)

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
    try:
        int(str_val)
        return True
    except ValueError:
        return False


def shutdown_process(process_obj, log_obj):
    count = 60
    while process_obj and process_obj.poll() is None:
        time.sleep(1)
        count -= 1
        kill_signal = signal.SIGTERM if count > 0 else signal.SIGKILL
        log_obj.info("Terminating process PID %s with signal %s (%s tries left)", process_obj.pid, kill_signal, count)
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


class ExceptionalDownloader(request.FancyURLopener, object):
    def http_error_default(self, url, fp, errcode, errmsg, headers):
        fp.close()
        raise TaurusNetworkError("Unsuccessful download from %s: %s - %s" % (url, errcode, errmsg))

    def get(self, url, filename=None, reporthook=None, data=None, suffix=""):
        fd = None
        try:
            if not filename:
                fd, filename = tempfile.mkstemp(suffix)
            response = self.retrieve(url, filename, reporthook, data)
        except BaseException:
            if fd:
                os.close(fd)
                os.remove(filename)
            raise

        if fd:
            os.close(fd)
        return response


class RequiredTool(object):
    """
    Abstract required tool
    """

    def __init__(self, tool_name, tool_path, download_link=""):
        self.tool_name = tool_name
        self.tool_path = tool_path
        self.download_link = download_link
        self.already_installed = False
        self.mirror_manager = None
        self.log = logging.getLogger('')

    def check_if_installed(self):
        if os.path.exists(self.tool_path):
            self.already_installed = True
            return True
        self.log.debug("File not exists: %s", self.tool_path)
        return False

    def install(self):
        with ProgressBarContext() as pbar:
            if not os.path.exists(os.path.dirname(self.tool_path)):
                os.makedirs(os.path.dirname(self.tool_path))
            downloader = ExceptionalDownloader()
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

        downloader = ExceptionalDownloader()
        sock_timeout = socket.getdefaulttimeout()
        socket.setdefaulttimeout(5)
        for link in links:
            self.log.info("Downloading: %s", link)
            with ProgressBarContext() as pbar:
                try:
                    return downloader.get(link, reporthook=pbar.download_callback, suffix=suffix)[0]
                except KeyboardInterrupt:
                    raise
                except BaseException as exc:
                    self.log.error("Error while downloading %s: %s" % (link, exc))
                finally:
                    socket.setdefaulttimeout(sock_timeout)
        raise TaurusInternalException("%s download failed: No more links to try" % self.tool_name)


class JavaVM(RequiredTool):
    def __init__(self, parent_logger, tool_path='java', download_link=''):
        super(JavaVM, self).__init__("JavaVM", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        cmd = [self.tool_path, '-version']
        self.log.debug("Trying %s: %s", self.tool_name, cmd)
        try:
            output = sync_run(cmd)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except (CalledProcessError, OSError) as exc:
            self.log.debug("Failed to check %s: %s", self.tool_name, exc)
            return False

    def install(self):
        raise ToolError("The %s is not operable or not available. Consider installing it" % self.tool_name)


class ProgressBarContext(ProgressBar):
    def __init__(self, maxval=0):
        widgets = [Percentage(), ' ', Bar(marker='=', left='[', right=']'), ' ', ETA()]
        super(ProgressBarContext, self).__init__(widgets=widgets, maxval=maxval, fd=sys.stdout)

    def __enter__(self):
        if not sys.stdout.isatty():
            logging.debug("No progressbar for non-tty output: %s", sys.stdout)

        self.start()
        return self

    def update(self, value=None):
        if sys.stdout.isatty():
            super(ProgressBarContext, self).update(value)

    def __exit__(self, exc_type, exc_val, exc_tb):
        del exc_type, exc_val, exc_tb
        if sys.stdout.isatty():
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

    def __init__(self, parent_logger):
        super(TclLibrary, self).__init__("Python Tcl library environment variable", "")
        self.log = parent_logger.getChild(self.__class__.__name__)

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
    def __init__(self, parent_logger):
        super(Node, self).__init__("Node.js", "")
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.executable = None

    def check_if_installed(self):
        node_candidates = ["node", "nodejs"]
        for candidate in node_candidates:
            try:
                self.log.debug("Trying %r", candidate)
                output = sync_run([candidate, '--version'])
                self.log.debug("%s output: %s", candidate, output)
                self.executable = candidate
                return True
            except (CalledProcessError, OSError):
                self.log.debug("%r is not installed", candidate)
                continue
        return False

    def install(self):
        raise ToolError("Automatic installation of nodejs is not implemented. Install it manually")


class MirrorsManager(object):
    def __init__(self, base_link, parent_logger):
        self.base_link = base_link
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.page_source = None

    @abstractmethod
    def _parse_mirrors(self):
        return []

    def mirrors(self):
        self.log.debug("Retrieving mirrors from page: %s", self.base_link)
        downloader = ExceptionalDownloader()
        try:
            tmp_file = downloader.get(self.base_link)[0]
            with open(tmp_file) as fds:
                self.page_source = fds.read()
        except BaseException:
            self.log.error("Can't fetch %s", self.base_link)
        mirrors = self._parse_mirrors()
        return (mirror for mirror in mirrors)


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
            with log_std_streams(logger=logging):
                webbrowser.open(url)
    except BaseException as exc:
        logging.warning("Can't open link in browser: %s", exc)


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
        logging.info("Screen %sx%s chars:\n%s", size[0], size[1], data)


def which(filename):
    """unix-style `which` implementation"""
    locations = os.environ.get("PATH").split(os.pathsep)
    candidates = []
    for location in locations:
        candidate = os.path.join(location, filename)
        if os.path.isfile(candidate):
            candidates.append(candidate)
    return candidates


def is_piped(file_obj):
    """check if file-object is a pipe or a file redirect"""
    mode = os.fstat(file_obj.fileno()).st_mode
    return stat.S_ISFIFO(mode) or stat.S_ISREG(mode)


class PythonGenerator(object):
    IMPORTS = ''

    def __init__(self, scenario, parent_logger):
        self.root = etree.Element("PythonCode")
        self.tree = etree.ElementTree(self.root)
        self.log = parent_logger.getChild(self.__class__.__name__)
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
    def gen_method_definition(method_name, params, indent=4):
        def_tmpl = "def {method_name}({params}):"
        method_def_element = etree.Element("method_definition", indent=str(indent))
        method_def_element.text = def_tmpl.format(method_name=method_name, params=",".join(params))
        return method_def_element

    @staticmethod
    def gen_decorator_statement(decorator_name, indent=4):
        def_tmpl = "@{decorator_name}"
        decorator_element = etree.Element("decorator_statement", indent=str(indent))
        decorator_element.text = def_tmpl.format(decorator_name=decorator_name)
        return decorator_element

    @staticmethod
    def gen_statement(statement, indent=8):
        statement_elem = etree.Element("statement", indent=str(indent))
        statement_elem.text = statement
        return statement_elem

    def gen_comment(self, comment, indent=8):
        return self.gen_statement("# %s" % comment, indent)

    def save(self, filename):
        with open(filename, 'wt') as fds:
            for child in self.root.iter():
                if child.text is not None:
                    indent = int(child.get('indent', "0"))
                    fds.write(" " * indent + child.text + "\n")

    def gen_new_line(self, indent=8):
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
    order = int(log(byteval, 2) / 10) if byteval else 0
    # format file size
    # (.4g results in rounded numbers for exact matches and max 3 decimals,
    # should never resort to exponent values)
    return '{:.4g}{}'.format(byteval / (1 << (order * 10)), _suffixes[order])


class LDJSONReader(object):
    def __init__(self, filename, parent_log):
        self.log = parent_log.getChild(self.__class__.__name__)
        self.file = FileReader(filename=filename,
                               file_opener=lambda f: open(f, 'rb', buffering=1),
                               parent_logger=self.log)
        self.partial_buffer = ""

    def read(self, last_pass=False):
        lines = self.file.get_lines(size=1024 * 1024, last_pass=last_pass)

        for line in lines:
            if not line.endswith("\n"):
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
            addr = text_type(iface.address)
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
