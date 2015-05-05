#!/bin/bash -xe

# setup env
virtualenv --clear --system-site-packages build
source build/bin/activate

# install depends
pip install colorlog pyyaml psutil lxml cssselect nose urwid coverage six

# run unit tests
./run-test.sh

# re-setup env
deactivate
virtualenv --clear --system-site-packages build
source build/bin/activate

# run installation test
pip -v install dist/bzt-*.tar.gz

# run functional tests
bzt -o execution.scenario.script=tests/jmx/dummy.jmx -d build/test -o modules.jmeter.path=build/jmeter/bin/jmeter
bzt -d build/test tests/json/jmeter_minimal.json -o modules.jmeter.path=build/jmeter/bin/jmeter
bzt -d build/test tests/json/get-post.json -o modules.jmeter.path=build/jmeter/bin/jmeter

# generate docs
pip install grip
./make-docs.sh

deactivate