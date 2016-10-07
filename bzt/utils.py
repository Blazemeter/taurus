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

import csv
import fnmatch
import itertools
import json
import logging
import mimetypes
import os
import platform
import random
import re
import shlex
import signal
import socket
import stat
import subprocess
import sys
import tempfile
import time
import webbrowser
import zipfile
from abc import abstractmethod
from collections import defaultdict, Counter
from subprocess import PIPE
from webbrowser import GenericBrowser

import psutil
from progressbar import ProgressBar, Percentage, Bar, ETA
from psutil import Popen
from urwid import BaseScreen

from bzt.six import string_types, iteritems, binary_type, text_type, b, integer_types, request, file_type


def get_full_path(path, step_up=0):
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

    :param str_time: string to convert
    :return: float value in seconds
    :raise ValueError: in case of unsupported unit
    """
    if not str_time:
        return 0

    parser = re.compile(r'([\d\.]+)([a-zA-Z]*)')
    parts = parser.findall(str(str_time).replace(' ', ''))

    if len(parts) == 0:
        msg = "String format not supported: %s"
        raise ValueError(msg % str_time)

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

    def get(self, key, default=defaultdict):
        """
        Change get with setdefault

        :type key: object
        :type default: object
        """
        if default == defaultdict:
            default = BetterDict()

        if isinstance(default, BaseException) and key not in self:
            raise default

        value = self.setdefault(key, default)

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
            raise ValueError("Loaded object is not dict [%s]: %s" % (src.__class__, src))

        for key, val in iteritems(src):
            if len(key) and key[0] == '~':  # overwrite flag
                if key[1:] in self:
                    self.pop(key[1:])
                key = key[1:]

            if len(key) and key[0] == '^':  # eliminate flag
                # TODO: improve logic - use val contents to see what to eliminate
                if key[1:] in self:
                    self.pop(key[1:])
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
                    self[key] = val
            elif isinstance(val, list):
                self.__ensure_list_type(val)
                if key not in self:
                    self[key] = []
                if isinstance(self[key], list):
                    self[key].extend(val)
                else:
                    self[key] = val
            else:
                self[key] = val

        return

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
        Deep traverse dict with visitor

        :type obj: list or dict or object
        :type visitor: callable
        """
        if isinstance(obj, dict):
            for key, val in iteritems(obj):
                visitor(val, key, obj)
                cls.traverse(obj[key], visitor)
        elif isinstance(obj, list):
            for idx, val in enumerate(obj):
                visitor(val, idx, obj)
                cls.traverse(obj[idx], visitor)


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
    logging.getLogger(__name__).debug("Executing shell: %s", args)

    if env:
        env = {k: str(v) for k, v in iteritems(env)}

    if is_windows():
        return Popen(args, stdout=stdout, stderr=stderr, stdin=stdin, bufsize=0, cwd=cwd, shell=shell, env=env)
    else:
        return Popen(args, stdout=stdout, stderr=stderr, stdin=stdin, bufsize=0,
                     preexec_fn=os.setpgrp, close_fds=True, cwd=cwd, shell=shell, env=env)


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
    for key, val in iteritems(dictnr):
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
                raise BaseException("Unhandled form data type: %s" % type(item))

        res_bytes = b("\r\n").join(result_list)
        res_bytes += b("\r\n")
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


def guess_csv_dialect(header):
    """ completely arbitrary fn to detect the delimiter

    :type header: str
    :raise ValueError:
    :rtype: csv.Dialect
    """
    possible_delims = ",;\t"

    return csv.Sniffer().sniff(header, delimiters=possible_delims)


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
        raise ValueError("Unsuccessful download from %s: %s - %s" % (url, errcode, errmsg))

    def get(self, url, filename=None, reporthook=None, data=None, suffix=""):
        fd = None
        try:
            if not filename:
                fd, filename = tempfile.mkstemp(suffix)
            response = self.retrieve(url, filename, reporthook, data)
        except:
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
        self.log = None

    def check_if_installed(self):
        if os.path.exists(self.tool_path):
            self.already_installed = True
            return True
        return False

    def install(self):
        with ProgressBarContext() as pbar:
            if not os.path.exists(os.path.dirname(self.tool_path)):
                os.makedirs(os.path.dirname(self.tool_path))
            downloader = ExceptionalDownloader()
            downloader.get(self.download_link, self.tool_path, reporthook=pbar.download_callback)

            if self.check_if_installed():
                return self.tool_path
            else:
                raise RuntimeError("Unable to run %s after installation!" % self.tool_name)

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
        raise RuntimeError("%s download failed: No more links to try" % self.tool_name)


class JavaVM(RequiredTool):
    def __init__(self, tool_path, download_link, parent_logger):
        super(JavaVM, self).__init__("JavaVM", tool_path, download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)

    def check_if_installed(self):
        cmd = ["java", '-version']
        self.log.debug("Trying: %s", cmd)
        try:
            output = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
            self.log.debug("%s output: %s", self.tool_name, output)
            return True
        except BaseException as exc:
            self.log.debug("Failed to start Java: %s", exc)
            raise RuntimeError("The %s is not operable or not available. Consider installing it" % self.tool_name)

    def install(self):
        raise NotImplementedError()


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


def open_browser(url):
    try:
        browser = webbrowser.get()
        if type(browser) != GenericBrowser:  # pylint: disable=unidiomatic-typecheck
            browser.open(url)
    except BaseException as exc:
        logging.warning("Can't open link in browser: %s", exc)


def is_windows():
    return platform.system() == 'Windows'


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
    "check if file-object is a pipe or a file redirect"
    mode = os.fstat(file_obj.fileno()).st_mode
    return stat.S_ISFIFO(mode) or stat.S_ISREG(mode)

