#! /bin/sh -xe

./build-sdist.sh

python $(dirname $0)/setup.py upload
