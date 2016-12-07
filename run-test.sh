#!/bin/sh -xe

mkdir -p build/test

python -m nose tests \
    --with-xunit --xunit-file=build/xunit.xml \
    -v --nocapture || echo Tests failed

python setup.py clean sdist

