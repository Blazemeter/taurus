#!/bin/sh -xe
apt-get install -y --force-yes gcc-mingw-w64-x86-64
pip install virtualenv
RUBY=ruby NO_UNITTESTS=true $(dirname $0)/virtualenv-test.bash
mv build /bzt-configs/
mv dist /bzt-configs/
