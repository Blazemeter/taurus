#!/bin/bash -xe

# setup env
virtualenv --clear build
source build/bin/activate

# install depends
pip install colorlog pyyaml psutil lxml cssselect nose urwid coverage six pylint

pylint -d R0903,R0904,C0301,C0302,C0111,E1103,R0201,R0902,W0511,F0401,E0611,R0801 -f parseable bzt > build/pylint.out || echo

# run unit tests
./run-test.sh

# re-setup env
deactivate
virtualenv --clear --system-site-packages build
source build/bin/activate

# run installation test
cd build # cd is to make it not find bzt package from sources
pip -v install ../dist/bzt-*.tar.gz
cd ..

# run functional tests
bzt -o execution.scenario.script=tests/jmx/dummy.jmx -d build/test -o modules.jmeter.path=build/jmeter/bin/jmeter
bzt -d build/test tests/json/jmeter_minimal.json -o modules.jmeter.path=build/jmeter/bin/jmeter
bzt -d build/test tests/json/get-post.json -o modules.jmeter.path=build/jmeter/bin/jmeter

# generate docs
pip install grip
./make-docs.sh

deactivate