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
import pkg_resources
from setuptools import setup

from bzt.resources.version import VERSION

with open('requirements.txt') as _f:
    content = _f.read()
    requires = [str(req) for req in pkg_resources.parse_requirements(content)]

setup(
    name="bzt",
    version=VERSION,
    description='Taurus Tool for Continuous Testing',
    long_description=open('README.md').read(),
    long_description_content_type='text/markdown',
    author='Andrey Pokhilko',
    author_email='andrey@blazemeter.com',
    url='http://gettaurus.org/',
    download_url='http://gettaurus.org/docs/DeveloperGuide/#Python-Egg-Snapshots',
    license='Apache 2.0',
    install_requires=requires,
    packages=['bzt', 'bzt.engine', 'bzt.jmx', 'bzt.modules', 'bzt.resources', 'bzt.resources.version',
              'bzt.modules.java', 'bzt.modules.apiritif', 'bzt.modules.python'],
    entry_points={
        'console_scripts': [
            'bzt=bzt.cli:main',
            'jmx2yaml=bzt.jmx2yaml:main',
            'soapui2yaml=bzt.soapui2yaml:main',
            'swagger2yaml=bzt.swagger2yaml:main',
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

        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
    ],
    python_requires='>=3.5',    # should be '>=3.7', but let's keep it for obsolete configuration
)
