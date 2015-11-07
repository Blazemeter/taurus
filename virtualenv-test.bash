#!/bin/bash -xe

# setup env
virtualenv --clear build
source build/bin/activate

# install depends
pip install --upgrade colorlog pyyaml psutil lxml cssselect nose urwid coverage six pylint selenium progressbar33 locustio pyvirtualdisplay bencode

pylint -d R0903,R0904,C0301,C0302,C0111,E1103,R0201,R0902,W0511,F0401,E0611,R0801 -f parseable bzt > build/pylint.out || echo

# run unit tests
./run-test.sh

# re-setup env
deactivate
virtualenv --clear --system-site-packages build
source build/bin/activate

# run installation test
cd build # cd is to make it not find bzt package from sources
pip install --upgrade ../dist/bzt-*.tar.gz
cd ..

echo '{"install-id": "UnitTest"}' > build/etc/bzt.d/99-zinstallID.json

# run functional tests
bzt -o execution.scenario.script=tests/jmx/dummy.jmx -o settings.artifacts-dir="build/test/%Y-%m-%d_%H-%M-%S.%f" -o modules.jmeter.path=build/jmeter/bin/jmeter
bzt tests/json/get-post.json -o settings.artifacts-dir="build/test/%Y-%m-%d_%H-%M-%S.%f" -o modules.jmeter.path=build/jmeter/bin/jmeter

