#! /bin/bash -xe

$(dirname $0)/build-sdist.sh

twine upload dist/*
