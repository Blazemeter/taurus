"""
Module holds tools used that JMeter executor depends on

Copyright 2017 BlazeMeter Inc.

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
import os
import re
import subprocess
import tempfile
from collections import namedtuple
from distutils.version import LooseVersion

from bzt import ToolError, TaurusNetworkError
from bzt.six import communicate, parse
from bzt.utils import get_full_path, EXE_SUFFIX, MirrorsManager, ExceptionalDownloader
from bzt.utils import shell_exec, BetterDict, unzip, RequiredTool, ProgressBarContext

MIRRORS_SOURCE = "https://jmeter.apache.org/download_jmeter.cgi"
JMETER_DOWNLOAD_LINK = "https://archive.apache.org/dist/jmeter/binaries/apache-jmeter-{version}.zip"
PLUGINS_MANAGER_VERSION = "0.16"
PLUGINS_MANAGER = 'https://search.maven.org/remotecontent?filepath=kg/apc/jmeter-plugins-manager/' \
                  '{ver}/jmeter-plugins-manager-{ver}.jar'.format(ver=PLUGINS_MANAGER_VERSION)
CMDRUNNER = 'https://search.maven.org/remotecontent?filepath=kg/apc/cmdrunner/2.0/cmdrunner-2.0.jar'


class JMeter(RequiredTool):
    """
    JMeter tool
    """

    def __init__(self, tool_path, parent_logger, jmeter_version, jmeter_download_link, plugins, proxy):
        super(JMeter, self).__init__("JMeter", tool_path, jmeter_download_link)
        self.log = parent_logger.getChild(self.__class__.__name__)
        self.version = jmeter_version
        self.mirror_manager = JMeterMirrorsManager(self.log, self.version)
        self.plugins = plugins
        self.proxy_settings = proxy
        self.tool_path = self.tool_path.format(version=self.version)

    def check_if_installed(self):
        self.log.debug("Trying jmeter: %s", self.tool_path)
        try:
            with tempfile.NamedTemporaryFile(prefix="jmeter", suffix="log", delete=False) as jmlog:
                jm_proc = shell_exec([self.tool_path, '-j', jmlog.name, '--version'], stderr=subprocess.STDOUT)
                jmout, jmerr = communicate(jm_proc)
                self.log.debug("JMeter check: %s / %s", jmout, jmerr)

            os.remove(jmlog.name)

            if "is too low to run JMeter" in jmout:
                raise ToolError("Java version is too low to run JMeter")

            return True

        except OSError:
            self.log.debug("JMeter check failed.")
            return False

    def _pmgr_call(self, params):
        cmd = [self._pmgr_path()] + params
        proc = shell_exec(cmd)
        return communicate(proc)

    def install_for_jmx(self, jmx_file):
        if not os.path.isfile(jmx_file):
            self.log.warning("Script %s not found" % jmx_file)
            return

        try:
            out, err = self._pmgr_call(["install-for-jmx", jmx_file])
            self.log.debug("Try to detect plugins for %s\n%s\n%s", jmx_file, out, err)
        except KeyboardInterrupt:
            raise
        except BaseException as exc:
            self.log.warning("Failed to detect plugins for %s: %s", jmx_file, exc)
            return

        if err and "Wrong command: install-for-jmx" in err:  # old manager
            self.log.debug("pmgr can't discover jmx for plugins")

    def __install_jmeter(self, dest):
        if self.download_link:
            jmeter_dist = self._download(use_link=True)
        else:
            jmeter_dist = self._download()

        try:
            self.log.info("Unzipping %s to %s", jmeter_dist, dest)
            unzip(jmeter_dist, dest, 'apache-jmeter-%s' % self.version)
        finally:
            os.remove(jmeter_dist)

        # set exec permissions
        os.chmod(os.path.join(dest, 'bin', 'jmeter'), 0o755)
        os.chmod(os.path.join(dest, 'bin', 'jmeter' + EXE_SUFFIX), 0o755)

        if not self.check_if_installed():
            raise ToolError("Unable to run %s after installation!" % self.tool_name)

    def __download_additions(self, tools):
        downloader = ExceptionalDownloader()
        with ProgressBarContext() as pbar:
            for tool in tools:
                url = tool[0]
                _file = os.path.basename(url)
                self.log.info("Downloading %s from %s", _file, url)
                try:
                    downloader.get(url, tool[1], reporthook=pbar.download_callback)
                except KeyboardInterrupt:
                    raise
                except BaseException as exc:
                    raise TaurusNetworkError("Error while downloading %s: %s" % (_file, exc))

    def __install_plugins_manager(self, plugins_manager_path):
        installer = "org.jmeterplugins.repository.PluginManagerCMDInstaller"
        cmd = ["java", "-cp", plugins_manager_path, installer]
        self.log.debug("Trying: %s", cmd)
        try:
            proc = shell_exec(cmd)
            out, err = communicate(proc)
            self.log.debug("Install PluginsManager: %s / %s", out, err)
        except KeyboardInterrupt:
            raise
        except BaseException as exc:
            raise ToolError("Failed to install PluginsManager: %s" % exc)

    def __install_plugins(self, plugins_manager_cmd):
        plugin_str = ",".join(self.plugins)
        self.log.info("Installing JMeter plugins: %s", plugin_str)
        cmd = [plugins_manager_cmd, 'install', plugin_str]
        self.log.debug("Trying: %s", cmd)
        try:
            # prepare proxy settings
            if self.proxy_settings and self.proxy_settings.get('address'):
                env = BetterDict()
                env.merge(dict(os.environ))
                jvm_args = env.get('JVM_ARGS', '')

                proxy_url = parse.urlsplit(self.proxy_settings.get("address"))
                self.log.debug("Using proxy settings: %s", proxy_url)
                host = proxy_url.hostname
                port = proxy_url.port
                if not port:
                    port = 80

                jvm_args += ' -Dhttp.proxyHost=%s -Dhttp.proxyPort=%s' % (host, port)  # TODO: remove it after pmgr 0.9
                jvm_args += ' -Dhttps.proxyHost=%s -Dhttps.proxyPort=%s' % (host, port)

                username = self.proxy_settings.get('username')
                password = self.proxy_settings.get('password')

                if username and password:
                    # property names correspond to
                    # https://github.com/apache/jmeter/blob/trunk/src/core/org/apache/jmeter/JMeter.java#L110
                    jvm_args += ' -Dhttp.proxyUser="%s" -Dhttp.proxyPass="%s"' % (username, password)

                env['JVM_ARGS'] = jvm_args

            proc = shell_exec(cmd)
            out, err = communicate(proc)
            self.log.debug("Install plugins: %s / %s", out, err)
        except KeyboardInterrupt:
            raise
        except BaseException as exc:
            raise ToolError("Failed to install plugins %s: %s" % (plugin_str, exc))

    def _pmgr_path(self):
        dest = get_full_path(self.tool_path, step_up=2)
        return os.path.join(dest, 'bin', 'PluginsManagerCMD' + EXE_SUFFIX)

    def install(self):
        dest = get_full_path(self.tool_path, step_up=2)
        self.log.info("Will install %s into %s", self.tool_name, dest)
        plugins_manager_name = os.path.basename(PLUGINS_MANAGER)
        cmdrunner_name = os.path.basename(CMDRUNNER)
        plugins_manager_path = os.path.join(dest, 'lib', 'ext', plugins_manager_name)
        cmdrunner_path = os.path.join(dest, 'lib', cmdrunner_name)
        direct_install_tools = [  # source link and destination
            [PLUGINS_MANAGER, plugins_manager_path],
            [CMDRUNNER, cmdrunner_path]]
        plugins_manager_cmd = self._pmgr_path()

        self.__install_jmeter(dest)
        self.__download_additions(direct_install_tools)
        self.__install_plugins_manager(plugins_manager_path)
        self.__install_plugins(plugins_manager_cmd)

        cleaner = JarCleaner(self.log)
        cleaner.clean(os.path.join(dest, 'lib'))

    def ctg_plugin_installed(self):
        """
        Simple check if ConcurrentThreadGroup is available
        :return:
        """
        ext_dir = os.path.join(get_full_path(self.tool_path, step_up=2), 'lib', 'ext')
        if os.path.isdir(ext_dir):
            list_of_jars = [file_name for file_name in os.listdir(ext_dir) if file_name.endswith('.jar')]
            if any([file_name.startswith('jmeter-plugins-casutg') for file_name in list_of_jars]):
                return True

        return False


class JarCleaner(object):
    def __init__(self, parent_logger):
        self.log = parent_logger.getChild(self.__class__.__name__)

    @staticmethod
    def __extract_version(jar):
        version_str = jar.split('-')[-1]
        return version_str.replace('.jar', '')

    def clean(self, path):
        """
        Remove old jars
        :param path: str
        """
        self.log.debug("Removing old jars from %s", path)
        jarlib = namedtuple("jarlib", ("file_name", "lib_name", "version"))
        jars = [fname for fname in os.listdir(path) if '-' in fname and os.path.isfile(os.path.join(path, fname))]
        jar_libs = [jarlib(file_name=jar,
                           lib_name='-'.join(jar.split('-')[:-1]),
                           version=JarCleaner.__extract_version(jar))
                    for jar in jars]

        duplicated_libraries = set()
        for jar_lib_obj in jar_libs:
            similar_packages = [lib for lib in jar_libs if lib.lib_name == jar_lib_obj.lib_name]
            if len(similar_packages) > 1:
                right_version = max(similar_packages, key=lambda l: LooseVersion(l.version))
                similar_packages.remove(right_version)
                duplicated_libraries.update(similar_packages)

        for old_lib in duplicated_libraries:
            os.remove(os.path.join(path, old_lib.file_name))
            self.log.debug("Old jar removed %s", old_lib.file_name)


class JMeterMirrorsManager(MirrorsManager):
    def __init__(self, parent_logger, jmeter_version):
        self.jmeter_version = str(jmeter_version)
        super(JMeterMirrorsManager, self).__init__(MIRRORS_SOURCE, parent_logger)

    def _parse_mirrors(self):
        links = []
        if self.page_source is not None:
            self.log.debug('Parsing mirrors...')
            select_search_pattern = re.compile(r'<select name="Preferred">.*?</select>', re.MULTILINE | re.DOTALL)
            option_search_pattern = re.compile(r'<option value=".*?">')
            select_element = select_search_pattern.findall(self.page_source)
            if select_element:
                option_elements = option_search_pattern.findall(select_element[0])
                link_tail = "/jmeter/binaries/apache-jmeter-{version}.zip".format(version=self.jmeter_version)
                links = [link.strip('<option value="').strip('">') + link_tail for link in option_elements]
        links.append(JMETER_DOWNLOAD_LINK.format(version=self.jmeter_version))
        self.log.debug('Total mirrors: %d', len(links))
        # place HTTPS links first, preserving the order of HTTP links
        sorted_links = sorted(links, key=lambda l: l.startswith("https"), reverse=True)
        return sorted_links
