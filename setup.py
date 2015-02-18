import os
import shutil
import sys
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
        dirname = os.getenv("VIRTUAL_ENV", "") + os.path.sep + "etc" + os.path.sep + "bzt.d"
        sys.stdout.write("Creating %s" % dirname)
        if not os.path.exists(dirname):
            os.makedirs(dirname)
        src = os.path.dirname(__file__)
        src += os.path.sep + "bzt" + os.path.sep + "10-base.json"
        sys.stdout.write("Copying %s to %s" % (src, dirname))
        shutil.copy(src, dirname + os.path.sep)


setup(
    name="bzt",
    version=bzt.version,
    description='Taurus Tool for Continuous Testing',
    author='Andrey Pokhilko',
    author_email='andrey@blazemeter.com',
    url='https://github.com/Blazemeter/taurus/',

    install_requires=[
        'pyyaml', 'psutil', 'colorlog', 'lxml', 'cssselect', 'urwid'
    ],
    packages=['bzt', 'bzt.modules'],
    entry_points={
        'console_scripts': [
            'bzt=bzt.cli:main',
        ],
    },
    package_data={
        "bzt": []
    },
    cmdclass=dict(install=InstallWithHook)
)
