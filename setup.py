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
from setuptools import setup
from distutils.version import LooseVersion

# noinspection PyPackageRequirements
import pip

import bzt


# thanks to pip there are two :incompatible ways to parse requirements.txt
if hasattr(pip, '__version__') and LooseVersion(str(pip.__version__)) >= LooseVersion('7.0'):
    # new versions of pip require a session
    requirements = pip.req.parse_requirements('requirements.txt', session=pip.download.PipSession())
else:
    # old versions do not
    requirements = pip.req.parse_requirements('requirements.txt')

requires = [str(item.req) for item in requirements]

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
    docs_url='http://gettaurus.org/docs/',
    install_requires=requires,
    packages=['bzt', 'bzt.six', 'bzt.jmx', 'bzt.modules', 'bzt.resources'],
    entry_points={
        'console_scripts': [
            'bzt=bzt.cli:main',
            'jmx2yaml=bzt.jmx2yaml:main',
            'soapui2yaml=bzt.soapui2yaml:main',
        ],
    },
    include_package_data=True,
    package_data={
        "bzt": [],
    },

    classifiers=[
        'Development Status :: 5 - Production/Stable',

        'Topic :: Software Development :: Quality Assurance',
        'Topic :: Software Development :: Testing',
        'Topic :: Software Development :: Testing :: Traffic Generation',

        'License :: OSI Approved :: Apache Software License',

        'Operating System :: Microsoft :: Windows',
        'Operating System :: MacOS',
        'Operating System :: POSIX :: Linux',

        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 2.7',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
    ],
)
