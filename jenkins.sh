#!/bin/sh -xe
pip install virtualenv
NO_UNITTESTS=true $(dirname $0)/virtualenv-test.bash