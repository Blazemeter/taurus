#!/bin/bash -xe
BUILD_NUMBER=$1

apt-get update -y
apt-get install -y --force-yes gcc-mingw-w64-x86-64 nsis
apt-get install -y libssl-dev libncurses5-dev libsqlite3-dev libreadline-dev libtk8.5 libgdm-dev libdb4o-cil-dev libpcap-dev

# build source distribution
./build-sdist.sh

# build a windows installer
pip3 install virtualenv
./build-windows-installer.sh ./dist/bzt-*.whl
