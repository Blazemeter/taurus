#!/bin/bash -xe

rm -rf build/tools

# setup node.js
rm -rf node_modules
npm install mocha

# setup env
virtualenv --clear build
source build/bin/activate

# install depends
pip install --upgrade colorlog pyyaml psutil lxml cssselect nose urwid coverage six pylint selenium progressbar33 locustio pyvirtualdisplay pynsist

pylint -d R0903,R0904,C0301,C0302,C0111,E1103,R0201,R0902,W0511,F0401,E0611,R0801,R0913,W0613,C0412,I0011 --extension-pkg-whitelist=lxml -f parseable bzt > build/pylint.out || echo

# run unit tests
./run-test.sh

# build a windows installer
./build-windows-installer.sh

# re-setup env
deactivate
virtualenv --clear --system-site-packages build
source build/bin/activate

# run installation test
cd build # cd is to make it not find bzt package from sources
pip install --upgrade ../dist/bzt-*.tar.gz
pip install locustio
cd ..

echo '{"install-id": "UnitTest"}' > build/etc/bzt.d/99-zinstallID.json

# run functional tests

# install and run jmeter
bzt -o execution.scenario.script=tests/jmeter/jmx/dummy.jmx -o settings.artifacts-dir="build/test/%Y-%m-%d_%H-%M-%S.%f" -o modules.jmeter.path=build/tools/jmeter/bin/jmeter
bzt -o settings.artifacts-dir="build/test/%Y-%m-%d_%H-%M-%S.%f" -o modules.jmeter.path=build/tools/jmeter/bin/jmeter tests/json/get-post.json

# run selenium
bzt -o settings.artifacts-dir="build/test/%Y-%m-%d_%H-%M-%S.%f" tests/yaml/func_test/selenium.yml

#run locust
bzt -o settings.artifacts-dir="build/test/%Y-%m-%d_%H-%M-%S.%f" tests/yaml/func_test/locust.yml

#install and run gatling
bzt -o settings.artifacts-dir="build/test/%Y-%m-%d_%H-%M-%S.%f" -o modules.gatling.path=build/tools/gatling/bin/gatling.sh  tests/yaml/func_test/gatling.yml

#install and run grinder
bzt -o settings.artifacts-dir="build/test/%Y-%m-%d_%H-%M-%S.%f" -o modules.grinder.path=build/tools/gatling/grinder/lib/grinder.jar  tests/yaml/func_test/grinder.yml
