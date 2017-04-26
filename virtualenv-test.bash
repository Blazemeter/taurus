#!/bin/bash -xe

rm -rf build/tools

# setup env
virtualenv --clear build
source build/bin/activate

# install depends
pip install --upgrade colorlog pyyaml psutil!=4.4.0 lxml cssselect nose urwid six pylint selenium progressbar33 locustio pyvirtualdisplay pynsist
pylint -d R0903,R0904,C0301,C0302,C0111,E1103,R0201,R0902,W0511,F0401,E0611,R0801,R0913,W0613,C0412,I0011,C0411 --extension-pkg-whitelist=lxml -f parseable bzt > build/pylint.out || echo

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

# prepare and run functional tests
rm -rf ~/.bzt
ln -sf /etc/bzt.d/50-pbench-enhanced.json build/etc/bzt.d/
echo '{"settings": {"artifacts-dir":"build/test/%Y-%m-%d_%H-%M-%S.%f"}}' > build/etc/bzt.d/99-artifacts-dir.json
echo '{"install-id": "UnitTest"}' > build/etc/bzt.d/99-zinstallID.json
mkdir -p ~/.bzt/selenium-taurus/mocha
npm install selenium-webdriver@2.53.3 --prefix ~/.bzt/selenium-taurus/mocha

export DBUS_SESSION_BUS_ADDRESS=/dev/null  # https://github.com/SeleniumHQ/docker-selenium/issues/87
bzt -install-tools
bzt examples/all-executors.yml -o modules.console.disable=true -sequential -o modules.rspec.interpreter=ruby2.0
