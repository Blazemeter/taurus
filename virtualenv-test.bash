#!/bin/bash -xe

rm -rf build/tools

# setup env
virtualenv --clear build
source build/bin/activate

# install depends
pip install --upgrade colorlog pyyaml psutil!=4.4.0 lxml cssselect nose urwid coverage six pylint selenium==2.53.1 progressbar33 locustio pyvirtualdisplay pynsist

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
pip install --upgrade selenium==2.53.1
pip show selenium
cd ..

echo '{"install-id": "UnitTest"}' > build/etc/bzt.d/99-zinstallID.json

# run functional tests
# TODO: restore it rm -r ~/.bzt
ln -s /etc/bzt.d/50-pbench-enhanced.json build/etc/bzt.d/
bzt examples/all-executors.yml -o settings.artifacts-dir="build/test/%Y-%m-%d_%H-%M-%S.%f" -o modules.console.disable=true -o execution.^6=null -o execution.^2=null