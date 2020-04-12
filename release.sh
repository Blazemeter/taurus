#! /bin/bash -xe

$(dirname $0)/build-sdist.sh

python -m twine upload dist/*
