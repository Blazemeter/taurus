#!/bin/sh -xe

mkdir -p build/test

python -m nose tests \
    --with-xunit --xunit-file=build/xunit.xml \
    --with-coverage --cover-package=bzt \
    --cover-xml --cover-xml-file=build/coverage/coverage.xml \
    --cover-html --cover-html-dir=build/coverage --cover-branches \
    -v --nocapture || echo Tests failed

# install it under virtualenv and try
python setup.py sdist

