#! /bin/bash -xe

$(dirname $0)/build-sdist.sh

python $(dirname $0)/setup.py sdist bdist_wheel
twine upload dist/*
