#!/bin/bash -xe
virtualenv --clear --system-site-packages build
source build/bin/activate

pip install colorlog pyyaml psutil lxml cssselect
python -m nose tests \
    --with-xunit --xunit-file=build/xunit.xml \
    --with-coverage --cover-package=bzt \
    --cover-xml --cover-xml-file=build/coverage/coverage.xml \
    --cover-html --cover-html-dir=build/coverage --cover-branches \
    -v --nocapture || echo Tests failed

# install it under virtualenv and try
python setup.py sdist

pip -v install dist/bzt-*.tar.gz

bzt -o execution.scenario.script=tests/jmx/dummy.jmx -d build/test -o modules.jmeter.path=build/jmeter/bin/jmeter

bzt -d build/test tests/json/jmeter_minimal.json -o modules.jmeter.path=build/jmeter/bin/jmeter

bzt -d build/test tests/json/get-post.json -o modules.jmeter.path=build/jmeter/bin/jmeter


deactivate