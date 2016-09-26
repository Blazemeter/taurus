#!/bin/sh

python $(dirname $0)/setup.py sdist bdist_wheel upload
