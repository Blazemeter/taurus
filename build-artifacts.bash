#!/bin/bash -xe
apt-get install -y --force-yes gcc-mingw-w64-x86-64
pip install pynsist

# build source distribution
./build-sdist.sh

# build a windows installer
./build-windows-installer.sh ./dist/bzt-*.tar.gz
