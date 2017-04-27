#!/bin/sh
# Development helper script, not intended to be used in python egg

PYTHONPATH=$(dirname $0) python -m bzt.cli -o settings.artifacts-dir="/tmp/bzt/%Y-%m-%d_%H-%M-%S.%f" "$@"
