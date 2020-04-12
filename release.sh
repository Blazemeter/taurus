#! /bin/bash -xe

$(dirname $0)/build-sdist.sh
python3 -m pip install twine
python3 -m twine upload dist/*
