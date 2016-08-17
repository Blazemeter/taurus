"""
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
import os
import shutil
import sys
import uuid
from setuptools import setup
from setuptools.command.install import install
import bzt


class InstallWithHook(install, object):
    """
    Command adding post-install hook to setup
    """

    def run(self):
        """
        Do the command's job!
        """
        install.run(self)
        self.__hook()

    def __hook(self):
        dirname = bzt.get_configs_dir()
        sys.stdout.write("[%s] Creating %s\n" % (bzt.VERSION, dirname))
        if not os.path.exists(dirname):
            os.makedirs(dirname)

        src = os.path.join(os.path.dirname(__file__), "bzt", "10-base.json")
        sys.stdout.write("Copying %s to %s\n" % (src, dirname))
        shutil.copy(src, dirname + os.path.sep)

        sys.stdout.write("Generating install-id\n")
        install_id = os.path.join(dirname, '99-installID.yml')
        if not os.path.exists(install_id):
            with open(install_id, 'w') as fhd:
                fhd.write("---\ninstall-id: %s" % uuid.uuid4())


setup(
    name="bzt",
    version=bzt.VERSION,
    description='Taurus Tool for Continuous Testing',
    author='Andrey Pokhilko',
    author_email='andrey@blazemeter.com',
    url='http://gettaurus.org/',
    download_url='http://gettaurus.org/docs/DeveloperGuide/#Python-Egg-Snapshots',
    license='Apache 2.0',
    platform='any',
    docs_url='http://gettaurus.org/',

    install_requires=[
        'pyyaml', 'psutil > 3', 'colorlog', 'colorama', 'lxml == 3.6.0',
        'cssselect', 'urwid', 'six', 'nose',
        'selenium', 'progressbar33', 'pyvirtualdisplay', 'requests', ],
    packages=['bzt', 'bzt.six', 'bzt.modules', 'bzt.resources'],
    entry_points={
        'console_scripts': [
            'bzt=bzt.cli:main',
            'jmx2yaml=bzt.jmx2yaml:main'
        ],
    },
    include_package_data=True,
    package_data={
        "bzt": [],
    },
    cmdclass={"install": InstallWithHook}
)
