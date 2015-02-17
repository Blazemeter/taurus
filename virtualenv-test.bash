#!/bin/bash -xe
virtualenv --clear --system-site-packages build
source build/bin/activate

pip install colorlog pyyaml psutil lxml cssselect

./run-test.sh

pip -v install dist/bzt-*.tar.gz

bzt -o execution.scenario.script=tests/jmx/dummy.jmx -d build/test -o modules.jmeter.path=build/jmeter/bin/jmeter

bzt -d build/test tests/json/jmeter_minimal.json -o modules.jmeter.path=build/jmeter/bin/jmeter

bzt -d build/test tests/json/get-post.json -o modules.jmeter.path=build/jmeter/bin/jmeter


deactivate